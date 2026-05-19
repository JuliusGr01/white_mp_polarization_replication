"""
Jordà (2005) local projections for White (2022) Equations (2)–(4).

Equation (2) linear:
    y_{t+h} - y_t = β'_h x_t + γ_h ε_t + u_{t,h}
with x_t using one year of lagged monthly changes in y and one year of
lagged shocks. This matches the Figure 3 replication settings in config.py.

Equation (3) sign-split:
    y_{t+h} - y_t = β'_h x_t + γ^+_h ε^+_t + γ^-_h ε^-_t + u_{t,h}
    ε^+ = max(ε,0), ε^- = min(ε,0).

Equation (4) quadratic:
    y_{t+h} - y_t = β'_h x_t + γ_{1,h} ε_t + γ_{2,h} ε_t^2 + u_{t,h}.

Standard errors: Newey–West (HAC) with lag length from config.NW_LAGS.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Literal

import numpy as np
import pandas as pd
from statsmodels.regression.linear_model import OLS

from config import LP_INCLUDE_TIME_TREND, LP_Y_LAG_TRANSFORM, NW_LAGS

Spec = Literal["linear", "sign", "quad"]


@dataclass
class IRFResult:
    horizons: np.ndarray
    coef: np.ndarray
    se: np.ndarray
    spec: Spec
    outcome: str


def _lag_matrix(series: pd.Series, n_lags: int, prefix: str) -> pd.DataFrame:
    cols = {}
    for i in range(1, n_lags + 1):
        cols[f"{prefix}_L{i}"] = series.shift(i)
    return pd.DataFrame(cols)


def build_design(
    df: pd.DataFrame,
    dep_level: str,
    shock: str,
    n_lag_y: int,
    n_lag_eps: int,
    y_lag_transform: str | None = None,
    include_time_trend: bool | None = None,
) -> pd.DataFrame:
    y_lag_transform = LP_Y_LAG_TRANSFORM if y_lag_transform is None else y_lag_transform
    include_time_trend = LP_INCLUDE_TIME_TREND if include_time_trend is None else include_time_trend

    out = df.copy()
    out["const"] = 1.0
    if include_time_trend:
        out["time_trend"] = np.arange(len(out), dtype=float)
    if y_lag_transform == "level":
        y_lag_source = out[dep_level]
    elif y_lag_transform == "diff":
        y_lag_source = out[dep_level].diff()
    else:
        raise ValueError(f"Unsupported y_lag_transform: {y_lag_transform!r}")
    out = pd.concat([out, _lag_matrix(y_lag_source, n_lag_y, "y")], axis=1)
    out = pd.concat([out, _lag_matrix(out[shock], n_lag_eps, "eps")], axis=1)
    out["eps_plus"] = np.maximum(out[shock], 0.0)
    out["eps_minus"] = np.minimum(out[shock], 0.0)
    out["eps_sq"] = out[shock] ** 2
    return out


def _control_cols(n_lag_y: int, n_lag_eps: int, include_time_trend: bool | None = None) -> list[str]:
    include_time_trend = LP_INCLUDE_TIME_TREND if include_time_trend is None else include_time_trend
    cols = ["const"]
    if include_time_trend:
        cols.append("time_trend")
    cols += [f"y_L{i}" for i in range(1, n_lag_y + 1)]
    cols += [f"eps_L{i}" for i in range(1, n_lag_eps + 1)]
    return cols


def estimate_irf_linear(
    df: pd.DataFrame,
    dep_level: str,
    shock: str,
    horizons: Iterable[int],
    n_lag_y: int,
    n_lag_eps: int,
) -> IRFResult:
    horizons = np.array(list(horizons), dtype=int)
    base = build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
    coefs: list[float] = []
    ses: list[float] = []
    Xcols = _control_cols(n_lag_y, n_lag_eps)
    shock_cols = [shock]

    for h in horizons:
        lhs = base[dep_level].shift(-h) - base[dep_level]
        reg = pd.concat([lhs.rename("lhs"), base[Xcols + shock_cols]], axis=1).dropna()
        X_clean = reg[Xcols + shock_cols]
        res = OLS(reg["lhs"], X_clean).fit(cov_type="HAC", cov_kwds={"maxlags": NW_LAGS})
        coefs.append(float(res.params[shock]))
        ses.append(float(res.bse[shock]))
    return IRFResult(horizons, np.array(coefs), np.array(ses), "linear", dep_level)


def estimate_irf_sign_both(
    df: pd.DataFrame,
    dep_level: str,
    shock: str,
    horizons: Iterable[int],
    n_lag_y: int,
    n_lag_eps: int,
) -> tuple[IRFResult, IRFResult]:
    horizons = np.array(list(horizons), dtype=int)
    base = build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
    Xcols = _control_cols(n_lag_y, n_lag_eps)
    shock_cols = ["eps_plus", "eps_minus"]
    cp, sp, cm, sm = [], [], [], []
    for h in horizons:
        lhs = base[dep_level].shift(-h) - base[dep_level]
        reg = pd.concat([lhs.rename("lhs"), base[Xcols + shock_cols]], axis=1).dropna()
        X_clean = reg[Xcols + shock_cols]
        res = OLS(reg["lhs"], X_clean).fit(cov_type="HAC", cov_kwds={"maxlags": NW_LAGS})
        cp.append(float(res.params["eps_plus"]))
        sp.append(float(res.bse["eps_plus"]))
        cm.append(float(res.params["eps_minus"]))
        sm.append(float(res.bse["eps_minus"]))
    plus = IRFResult(horizons, np.array(cp), np.array(sp), "sign", dep_level + "|eps_plus")
    minus = IRFResult(horizons, np.array(cm), np.array(sm), "sign", dep_level + "|eps_minus")
    return plus, minus


def estimate_irf_quad(
    df: pd.DataFrame,
    dep_level: str,
    shock: str,
    horizons: Iterable[int],
    n_lag_y: int,
    n_lag_eps: int,
) -> tuple[IRFResult, IRFResult]:
    """IRF for +1 pp contraction (γ1+γ2) and -1 pp expansion (-γ1+γ2) with delta-method SE."""
    horizons = np.array(list(horizons), dtype=int)
    base = build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
    Xcols = _control_cols(n_lag_y, n_lag_eps)
    shock_cols = [shock, "eps_sq"]
    c_pos, s_pos, c_neg, s_neg = [], [], [], []
    for h in horizons:
        lhs = base[dep_level].shift(-h) - base[dep_level]
        reg = pd.concat([lhs.rename("lhs"), base[Xcols + shock_cols]], axis=1).dropna()
        X_clean = reg[Xcols + shock_cols]
        res = OLS(reg["lhs"], X_clean).fit(cov_type="HAC", cov_kwds={"maxlags": NW_LAGS})
        g1 = float(res.params[shock])
        g2 = float(res.params["eps_sq"])
        vc = res.cov_params().loc[[shock, "eps_sq"], [shock, "eps_sq"]]
        c_pos.append(g1 + g2)
        J = np.array([1.0, 1.0])
        s_pos.append(float(np.sqrt(J @ vc.values @ J)))
        c_neg.append(-g1 + g2)
        J2 = np.array([-1.0, 1.0])
        s_neg.append(float(np.sqrt(J2 @ vc.values @ J2)))
    pos = IRFResult(horizons, np.array(c_pos), np.array(s_pos), "quad", dep_level + "|+1pp")
    neg = IRFResult(horizons, np.array(c_neg), np.array(s_neg), "quad", dep_level + "|-1pp")
    return pos, neg


def fev_share_linear(
    df: pd.DataFrame,
    dep_level: str,
    shock: str,
    horizons: Iterable[int],
    n_lag_y: int,
    n_lag_eps: int,
) -> pd.DataFrame:
    """Share of FEV due to shocks: 1 - MSFE_full / MSFE_restricted (linear controls)."""
    rows: list[dict] = []
    base = build_design(df, dep_level, shock, n_lag_y, n_lag_eps)
    Xeps = [shock] + [f"eps_L{i}" for i in range(1, n_lag_eps + 1)]
    control_base = _control_cols(n_lag_y, 0)

    for h in horizons:
        lhs = base[dep_level].shift(-h) - base[dep_level]
        reg_full = pd.concat([lhs.rename("lhs"), base[control_base + Xeps]], axis=1).dropna()
        Xf = reg_full[control_base + Xeps]
        res_f = OLS(reg_full["lhs"], Xf).fit()
        reg_res = pd.concat([lhs.rename("lhs"), base[control_base]], axis=1).dropna()
        Xr = reg_res[control_base]
        res_r = OLS(reg_res["lhs"], Xr).fit()
        mse_f = float(np.mean(res_f.resid**2))
        mse_r = float(np.mean(res_r.resid**2))
        share = 1.0 - mse_f / mse_r if mse_r > 0 else float("nan")
        rows.append({"horizon": h, "fev_share": share, "outcome": dep_level})
    return pd.DataFrame(rows)
