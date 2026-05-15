"""
Download or load Romer–Romer monetary policy shocks (monthly).

The paper uses Romer & Romer (2004) shocks extended by Coibion et al. (2012) through 2008.
Replication packages differ in formatting. This module:

1. Loads `data/rr_shock_monthly.csv` if present (columns: date, shock) where shock is in
   percentage points of the funds rate target surprise (contractionary = positive), monthly.
2. Otherwise tries a few public GitHub CSV layouts and normalizes to month-end dates.

If nothing is found, raise FileNotFoundError with instructions to place the Coibion et al.
(2012) / Romer replication monthly shock series in `data/rr_shock_monthly.csv`.
"""

from __future__ import annotations

import io
from pathlib import Path
from typing import Optional

import pandas as pd
import requests

from config import DATA_DIR

CANDIDATE_URLS = (
    # Community-maintained replication outputs (layout may change).
    "https://raw.githubusercontent.com/miguel-acosta/RomerRomer2004/master/output/rrshocks.csv",
)


def _parse_date_series(s: pd.Series) -> pd.Series:
    return pd.to_datetime(s, utc=False, errors="coerce")


def _aggregate_meeting_shocks_to_month(df: pd.DataFrame, date_col: str, shock_col: str) -> pd.DataFrame:
    d = df[[date_col, shock_col]].dropna().copy()
    d["month"] = _parse_date_series(d[date_col]).dt.to_period("M").dt.to_timestamp()
    monthly = d.groupby("month", as_index=False)[shock_col].sum()
    monthly = monthly.rename(columns={shock_col: "shock"})
    return monthly


def try_fetch_remote_rr() -> Optional[pd.DataFrame]:
    for url in CANDIDATE_URLS:
        try:
            r = requests.get(url, timeout=30)
            r.raise_for_status()
            raw = pd.read_csv(io.StringIO(r.text))
        except Exception:
            continue
        cols = {c.lower(): c for c in raw.columns}
        # Heuristic column detection
        date_col = None
        for key in ("fomc", "date", "meeting", "month"):
            if key in cols:
                date_col = cols[key]
                break
        shock_col = None
        for key in ("rr_original", "rr_update", "shock", "residual", "epsilon", "mp_shock"):
            if key in cols:
                shock_col = cols[key]
                break
        if date_col is None or shock_col is None:
            continue
        out = _aggregate_meeting_shocks_to_month(raw, date_col, shock_col)
        out["date"] = out["month"]
        return out[["date", "shock"]]
    return None


def load_rr_shock_monthly(path: Optional[Path] = None) -> pd.DataFrame:
    path = path or (DATA_DIR / "rr_shock_monthly.csv")
    if path.exists():
        df = pd.read_csv(path)
        if "date" not in df.columns or "shock" not in df.columns:
            raise ValueError(f"{path} must have columns: date, shock")
        df["date"] = pd.to_datetime(df["date"])
        return df.sort_values("date").reset_index(drop=True)

    remote = try_fetch_remote_rr()
    if remote is not None:
        DATA_DIR.mkdir(parents=True, exist_ok=True)
        remote.to_csv(path, index=False)
        return remote.sort_values("date").reset_index(drop=True)

    raise FileNotFoundError(
        "Could not find Romer–Romer monthly shocks. Create data/rr_shock_monthly.csv with "
        "columns `date` (YYYY-MM-DD) and `shock` (percentage points, contractionary positive), "
        "or install a replication file from Coibion et al. (2012) / Romer & Romer (2004) and "
        "aggregate FOMC meeting shocks to calendar months (sum within month)."
    )
