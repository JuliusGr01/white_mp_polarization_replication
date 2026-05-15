"""
Run local projections for White (2022).

Steps:
  1) Place IPUMS CPS extract CSV (see build_employment_from_ipums.py) or existing
     `data/employment_monthly.csv` with columns from that module.
  2) Romer–Romer shocks: place `data/RR_MPshocks_Updated(GBforecasts).csv`
     from Yuriy Gorodnichenko's Berkeley page. The loader uses MTGDATE and
     the final CSV column, sums shocks by month, and fills no-meeting months
     with zero.
  3) python run_all.py

Outputs IRF plots and FEV table under `output/`.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

_ROOT = Path(__file__).resolve().parent
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))

import numpy as np
import pandas as pd

from config import DATA_DIR, H_MAX, N_LAGS_SHOCK, N_LAGS_Y, ROOT, SHOCK_END_DATE, START_DATE
from fetch_rr_shock import load_rr_shock_monthly
from local_projections import estimate_irf_linear, estimate_irf_quad, estimate_irf_sign_both, fev_share_linear
from plotting import plot_irf


def merge_monthly_panel() -> pd.DataFrame:
    emp_path = DATA_DIR / "employment_monthly.csv"
    if not emp_path.exists():
        raise FileNotFoundError(
            f"Missing {emp_path}. Build it with:\n"
            "  python build_employment_from_ipums.py path/to/ipums_cps.csv\n"
            "or from Python:\n"
            "  from pathlib import Path\n"
            "  from build_employment_from_ipums import build_employment_monthly\n"
            "  build_employment_monthly(Path('ipums_cps.csv'))\n"
            "after downloading a CPS basic monthly extract from IPUMS USA (1983+),\n"
            "  OR run: python fetch_bls_cps_occupation.py   (official BLS monthly CPS occupation series)."
        )
    emp = pd.read_csv(emp_path, parse_dates=["date"])
    shock = load_rr_shock_monthly().rename(columns={"shock": "eps"})
    shock["date"] = pd.to_datetime(shock["date"])
    m = emp.merge(shock, on="date", how="inner").sort_values("date")
    # Employment may extend past 2020; updated Romer–Romer shocks stop in 2008 (paper baseline).
    m = m[(m["date"] >= pd.Timestamp(START_DATE)) & (m["date"] <= pd.Timestamp(SHOCK_END_DATE))]
    return m.reset_index(drop=True)


def synthetic_panel(n: int = 400, seed: int = 0) -> pd.DataFrame:
    """Toy panel to verify the pipeline runs (not an economic replication)."""
    rng = np.random.default_rng(seed)
    dates = pd.date_range("1983-01-01", periods=n, freq="MS")
    eps = rng.normal(0, 0.12, size=n)
    eps[::5] += rng.normal(0, 0.2, size=int(np.ceil(n / 5)))
    log_tot = np.cumsum(0.0015 + 0.008 * eps + rng.normal(0, 0.008, size=n)) + 12.0
    log_r = np.cumsum(0.001 + 0.025 * np.maximum(eps, 0) + 0.003 * eps + rng.normal(0, 0.009, size=n)) + 11.3
    log_r = np.minimum(log_r, log_tot - 0.06)
    log_nr = np.log(np.clip(np.exp(log_tot) - np.exp(log_r), 1e-6, None))
    share = np.exp(log_r - log_tot)
    return pd.DataFrame(
        {
            "date": dates,
            "eps": eps,
            "log_total": log_tot,
            "log_routine": log_r,
            "log_nonroutine": log_nr,
            "routine_share": share,
        }
    )


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--synthetic", action="store_true", help="Use synthetic data to test the code path.")
    args = ap.parse_args()

    try:
        _run(args)
    except Exception as exc:
        raise SystemExit(f"Error: {exc}") from exc


def _run(args: argparse.Namespace) -> None:
    out_dir = ROOT / "output"
    out_dir.mkdir(parents=True, exist_ok=True)

    if args.synthetic:
        panel = synthetic_panel()
    else:
        panel = merge_monthly_panel()

    horizons = range(1, H_MAX + 1)

    outcomes = [
        ("log_total", "log total employment", "Δ log total employment"),
        ("log_routine", "log routine employment", "Δ log routine employment"),
        ("log_nonroutine", "log nonroutine employment", "Δ log nonroutine employment"),
        ("routine_share", "routine share of employment", "Δ routine share"),
    ]

    fev_rows = []
    for dep, slug, ylab in outcomes:
        ir = estimate_irf_linear(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
        plot_irf(ir, out_dir / f"irf_linear_{slug.replace(' ', '_')}.png", f"Linear LP — {slug}", ylab)
        fev_rows.append(fev_share_linear(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK).assign(spec="linear"))

        p_plus, p_minus = estimate_irf_sign_both(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
        plot_irf(
            p_plus,
            out_dir / f"irf_sign_contraction_{slug.replace(' ', '_')}.png",
            f"Sign-split LP — contractionary ε+ — {slug}",
            ylab,
        )
        plot_irf(
            p_minus,
            out_dir / f"irf_sign_expansion_{slug.replace(' ', '_')}.png",
            f"Sign-split LP — expansionary ε- — {slug}",
            ylab,
        )

        q_pos, q_neg = estimate_irf_quad(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
        plot_irf(
            q_pos,
            out_dir / f"irf_quad_contraction_{slug.replace(' ', '_')}.png",
            f"Quadratic LP — +1 pp shock — {slug}",
            ylab,
        )
        plot_irf(
            q_neg,
            out_dir / f"irf_quad_expansion_{slug.replace(' ', '_')}.png",
            f"Quadratic LP — -1 pp shock — {slug}",
            ylab,
        )

    fev = pd.concat(fev_rows, ignore_index=True)
    fev_pivot = fev.pivot_table(index="horizon", columns="outcome", values="fev_share")
    fev_pivot.to_csv(out_dir / "fev_linear.csv")
    print(f"Wrote figures and {out_dir / 'fev_linear.csv'}")


if __name__ == "__main__":
    main()
