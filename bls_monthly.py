"""Helpers for monthly BLS flat-file series."""

from __future__ import annotations

from pathlib import Path

import pandas as pd

from config import DATA_DIR

BLS_ALLDATA = DATA_DIR / "bls_raw" / "ln.data.1.AllData"


def load_bls_monthly_series(series_id: str, source_path: Path = BLS_ALLDATA) -> pd.DataFrame:
    """Load one monthly BLS LFS series as date/value rows."""
    raw = pd.read_csv(
        source_path,
        sep=r"\s+",
        engine="python",
        usecols=["series_id", "year", "period", "value"],
    )
    raw = raw[(raw["series_id"] == series_id) & raw["period"].str.match(r"M\d{2}")]
    raw = raw[raw["period"] != "M13"].copy()
    raw["month"] = raw["period"].str[1:].astype(int)
    raw["date"] = pd.to_datetime(
        {"year": raw["year"].astype(int), "month": raw["month"], "day": 1}
    )
    raw["value"] = pd.to_numeric(raw["value"], errors="coerce")
    return raw[["date", "value"]].dropna().sort_values("date").reset_index(drop=True)
