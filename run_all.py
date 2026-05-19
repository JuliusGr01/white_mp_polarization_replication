"""
Run local projections for White (2022).

Steps:
  1) Build the routine/nonroutine employment panel from the user's
     Employment and Earnings Excel-derived CPS data for 1969-1982 and the BLS
     broad-occupation series from 1983 onward. For the LPs, use the resulting
     seasonally adjusted occupation shares to split BLS aggregate
     nonagricultural wage-and-salary employment.
  2) Romer–Romer shocks: place `data/RR_MPshocks_Updated(GBforecasts).csv`
     from Yuriy Gorodnichenko's Berkeley page. The loader uses MTGDATE and
     the final CSV column, sums shocks by month, and fills no-meeting months
     with zero.
  3) python run_all.py

Outputs Figures 1-3 recreations, IRF plots, and the FEV table under `output/`.
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import replace
from pathlib import Path

_ROOT = Path(__file__).resolve().parent
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))

import numpy as np
import pandas as pd

from bls_monthly import load_bls_monthly_series
from build_employment_panel import (
    BLS_OCC_SOURCE,
    EE_1969_1982_PANEL,
    EXTENDED_EMPLOYMENT_PANEL,
    build_extended_employment_panel,
)
from config import DATA_DIR, END_DATE, H_MAX, N_LAGS_SHOCK, N_LAGS_Y, ROOT, SHOCK_END_DATE, START_DATE
from fetch_rr_shock import load_rr_shock_monthly
from local_projections import estimate_irf_linear, estimate_irf_quad, estimate_irf_sign_both, fev_share_linear
from plot_figures_1_2 import main as plot_figures_1_2
from plotting import plot_figure3, plot_irf
from seasonal_adjust import add_employment_sa_columns

TOTAL_NONAG_EMPLOYMENT_SERIES_ID = "LNS12032187"
LP_EMPLOYMENT_PANEL = DATA_DIR / "employment_monthly_white_lp.csv"


def merge_monthly_panel() -> pd.DataFrame:
    if EE_1969_1982_PANEL.exists() and BLS_OCC_SOURCE.exists():
        emp = build_extended_employment_panel(EE_1969_1982_PANEL, BLS_OCC_SOURCE)
        emp.to_csv(EXTENDED_EMPLOYMENT_PANEL, index=False)
    elif EXTENDED_EMPLOYMENT_PANEL.exists():
        emp = pd.read_csv(EXTENDED_EMPLOYMENT_PANEL, parse_dates=["date"])
    else:
        raise FileNotFoundError(
            f"Missing {EE_1969_1982_PANEL} and/or {BLS_OCC_SOURCE}. Provide the Excel-derived "
            f"1969-1982 panel and BLS broad-occupation monthly file, or a prebuilt "
            f"{EXTENDED_EMPLOYMENT_PANEL}."
        )
    shock = load_rr_shock_monthly().rename(columns={"shock": "eps"})
    shock["date"] = pd.to_datetime(shock["date"])
    m = emp.merge(shock, on="date", how="left").sort_values("date")
    # Keep post-2008 employment so y_{t+h} is available for late-shock observations.
    # Rows with missing current/lagged shocks drop inside each local projection.
    m = m[(m["date"] >= pd.Timestamp(START_DATE)) & (m["date"] <= pd.Timestamp(END_DATE))]
    return m.reset_index(drop=True)


def build_white_lp_panel() -> pd.DataFrame:
    """Build White-style variables used in Figure 3 local projections.

    The occupation data identify the routine/nonroutine shares. Following the
    paper's Figure 3 wording, levels are those shares applied to seasonally
    adjusted BLS employment for nonagricultural wage-and-salary workers.
    """
    panel = add_employment_sa_columns(merge_monthly_panel())
    total_nonag = load_bls_monthly_series(TOTAL_NONAG_EMPLOYMENT_SERIES_ID).rename(
        columns={"value": "total_nonag_employment_thousands"}
    )
    total_nonag["total_nonag_emp"] = total_nonag["total_nonag_employment_thousands"] * 1000.0

    panel = panel.merge(total_nonag[["date", "total_nonag_emp"]], on="date", how="left")
    if panel["total_nonag_emp"].isna().any():
        missing = panel.loc[panel["total_nonag_emp"].isna(), "date"].dt.strftime("%Y-%m").head(12)
        raise ValueError(f"Missing aggregate nonagricultural employment for: {', '.join(missing)}")

    panel["routine_share"] = panel["routine_share_sa"]
    panel["total_emp"] = panel["total_nonag_emp"]
    panel["routine_emp"] = panel["routine_share"] * panel["total_emp"]
    panel["nonroutine_emp"] = (1.0 - panel["routine_share"]) * panel["total_emp"]
    panel["log_total"] = np.log(panel["total_emp"])
    panel["log_routine"] = np.log(panel["routine_emp"])
    panel["log_nonroutine"] = np.log(panel["nonroutine_emp"])

    panel = panel[(panel["date"] >= pd.Timestamp(START_DATE)) & (panel["date"] <= pd.Timestamp(SHOCK_END_DATE))]
    panel = panel.reset_index(drop=True)
    LP_EMPLOYMENT_PANEL.parent.mkdir(parents=True, exist_ok=True)
    panel.to_csv(LP_EMPLOYMENT_PANEL, index=False)
    return panel


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
        panel = build_white_lp_panel()
        plot_figures_1_2()

    horizons = range(1, H_MAX + 1)

    outcomes = [
        ("log_total", "log total employment", "Percent", 100.0),
        ("log_routine", "log routine employment", "Percent", 100.0),
        ("log_nonroutine", "log nonroutine employment", "Percent", 100.0),
        ("routine_share", "routine share of employment", "% Points", 100.0),
    ]

    fev_rows = []
    figure3_irfs = {}
    for dep, slug, ylab, scale in outcomes:
        ir = estimate_irf_linear(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
        ir_scaled = replace(ir, coef=ir.coef * scale, se=ir.se * scale)
        figure3_irfs[dep] = ir_scaled
        plot_irf(
            ir_scaled,
            out_dir / f"irf_linear_{slug.replace(' ', '_')}.png",
            f"Linear LP — {slug}",
            ylab,
        )
        fev_rows.append(fev_share_linear(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK).assign(spec="linear"))

        p_plus, p_minus = estimate_irf_sign_both(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
        plot_irf(
            replace(p_plus, coef=p_plus.coef * scale, se=p_plus.se * scale),
            out_dir / f"irf_sign_contraction_{slug.replace(' ', '_')}.png",
            f"Sign-split LP — contractionary ε+ — {slug}",
            ylab,
        )
        plot_irf(
            replace(p_minus, coef=p_minus.coef * scale, se=p_minus.se * scale),
            out_dir / f"irf_sign_expansion_{slug.replace(' ', '_')}.png",
            f"Sign-split LP — expansionary ε- — {slug}",
            ylab,
        )

        q_pos, q_neg = estimate_irf_quad(panel, dep, "eps", horizons, N_LAGS_Y, N_LAGS_SHOCK)
        plot_irf(
            replace(q_pos, coef=q_pos.coef * scale, se=q_pos.se * scale),
            out_dir / f"irf_quad_contraction_{slug.replace(' ', '_')}.png",
            f"Quadratic LP — +1 pp shock — {slug}",
            ylab,
        )
        plot_irf(
            replace(q_neg, coef=q_neg.coef * scale, se=q_neg.se * scale),
            out_dir / f"irf_quad_expansion_{slug.replace(' ', '_')}.png",
            f"Quadratic LP — -1 pp shock — {slug}",
            ylab,
        )

    plot_figure3(figure3_irfs, out_dir / "figure3_linear_occupations.png")
    fev = pd.concat(fev_rows, ignore_index=True)
    fev_pivot = fev.pivot_table(index="horizon", columns="outcome", values="fev_share")
    fev_pivot.to_csv(out_dir / "fev_linear.csv")
    print(f"Wrote figures and {out_dir / 'fev_linear.csv'}")


if __name__ == "__main__":
    main()
