"""Seasonal adjustment helpers for monthly employment series."""

from __future__ import annotations

import numpy as np
import pandas as pd
from statsmodels.tsa.seasonal import STL


def seasonally_adjust_positive_series(series: pd.Series, period: int = 12) -> pd.Series:
    """Return a multiplicative STL seasonal adjustment for a positive monthly series."""
    y = pd.to_numeric(series, errors="coerce").astype(float)
    if y.isna().any() or (y <= 0).any():
        raise ValueError("Seasonal adjustment requires a complete positive series.")

    fit = STL(np.log(y.to_numpy()), period=period, seasonal=13, robust=True).fit()
    adjusted = np.exp(np.log(y.to_numpy()) - fit.seasonal)
    return pd.Series(adjusted, index=series.index, name=series.name)


def add_employment_sa_columns(panel: pd.DataFrame) -> pd.DataFrame:
    """Add seasonally adjusted employment, shares, and logs to a standard panel."""
    out = panel.copy()
    out["routine_emp_sa"] = seasonally_adjust_positive_series(out["routine_emp"])
    out["nonroutine_emp_sa"] = seasonally_adjust_positive_series(out["nonroutine_emp"])
    out["total_emp_sa"] = seasonally_adjust_positive_series(out["total_emp"])
    out["routine_share_sa"] = out["routine_emp_sa"] / out["total_emp_sa"]
    out["log_total_sa"] = np.log(out["total_emp_sa"])
    out["log_routine_sa"] = np.log(out["routine_emp_sa"])
    out["log_nonroutine_sa"] = np.log(out["nonroutine_emp_sa"])
    return out
