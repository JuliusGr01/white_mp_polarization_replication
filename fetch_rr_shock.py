"""
Load the updated Romer--Romer monetary policy shocks.

The replication uses the CSV distributed from Yuriy Gorodnichenko's Berkeley page
for Coibion, Gorodnichenko, Kueng, and Silvia, "Innocent Bystanders? Monetary
Policy and Inequality in the U.S."  The file is named
``RR_MPshocks_Updated(GBforecasts).csv`` and extends the Greenbook-forecast-based
Romer--Romer monetary-policy shock series through 2008.

The source CSV is an FOMC-meeting-level file, not the normalized monthly
``date, shock`` layout previously used by this repository.  In particular:

1. The meeting date is stored in ``MTGDATE`` and may be exported in several
   formats (for example MMDDYY integers, YYYYMMDD integers, Stata daily dates,
   Excel serial dates, or ordinary date strings).
2. The shock to use is the *last column* of the CSV.
3. Meeting-level shocks are summed within each calendar month.  Months between
   the first and last meeting with no shock observation are filled with zero so
   the returned ``date, shock`` DataFrame is a regular monthly series.
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional

import pandas as pd

from config import DATA_DIR

UPDATED_RR_SHOCK_FILENAME = "RR_MPshocks_Updated(GBforecasts).csv"
LEGACY_RR_SHOCK_FILENAME = "rr_shock_monthly.csv"


def _read_csv(path: Path) -> pd.DataFrame:
    """Read comma- or semicolon-delimited shock CSVs, including decimal commas."""
    return pd.read_csv(path, sep=None, engine="python", decimal=",")


def _to_numeric(values: pd.Series) -> pd.Series:
    if pd.api.types.is_numeric_dtype(values):
        return values
    text = values.astype("string").str.replace(",", ".", regex=False)
    return pd.to_numeric(text, errors="coerce")


def _parse_numeric_dates(values: pd.Series) -> pd.Series:
    """Parse common numeric exports of MTGDATE into pandas timestamps."""
    numeric = pd.to_numeric(values, errors="coerce")
    parsed = pd.Series(pd.NaT, index=values.index, dtype="datetime64[ns]")

    # Compact calendar dates exported as integers/floats.
    as_str = values.astype("string").str.strip().str.replace(r"\.0$", "", regex=True)
    yyyymmdd = as_str.str.fullmatch(r"(19|20)\d{6}").fillna(False)
    parsed.loc[yyyymmdd] = pd.to_datetime(
        as_str.loc[yyyymmdd], format="%Y%m%d", errors="coerce"
    )

    # The Berkeley CSV stores dates such as 11469 for 1969-01-14.
    mmddyy = as_str.str.fullmatch(r"\d{5,6}").fillna(False) & parsed.isna()
    parsed.loc[mmddyy] = pd.to_datetime(
        as_str.loc[mmddyy].str.zfill(6), format="%m%d%y", errors="coerce"
    )

    yyyymm = (
        as_str.str.fullmatch(r"(19|20)\d{2}(0[1-9]|1[0-2])").fillna(False)
        & parsed.isna()
    )
    parsed.loc[yyyymm] = pd.to_datetime(
        as_str.loc[yyyymm] + "01", format="%Y%m%d", errors="coerce"
    )

    remaining = parsed.isna() & numeric.notna()
    if not remaining.any():
        return parsed

    # Stata daily dates count from 1960-01-01.  FOMC dates from 1969--2008 have
    # values roughly 3,000--18,000, whereas Excel serial dates for the same range
    # are roughly 25,000--40,000.
    stata_like = remaining & numeric.between(2500, 20000)
    parsed.loc[stata_like] = pd.to_datetime(
        numeric.loc[stata_like], unit="D", origin="1960-01-01", errors="coerce"
    )

    excel_like = remaining & parsed.isna() & numeric.between(20000, 50000)
    parsed.loc[excel_like] = pd.to_datetime(
        numeric.loc[excel_like], unit="D", origin="1899-12-30", errors="coerce"
    )
    return parsed


def _parse_month_year_dates(text: pd.Series) -> pd.Series:
    """Parse Berkeley month labels such as Feb 97, Dec-09, or Jan 2010."""
    normalized = text.str.replace("-", " ", regex=False)
    parsed = pd.Series(pd.NaT, index=text.index, dtype="datetime64[ns]")
    for fmt in ("%b %y", "%B %y", "%b %Y", "%B %Y"):
        remaining = parsed.isna()
        if not remaining.any():
            break
        parsed.loc[remaining] = pd.to_datetime(
            normalized.loc[remaining], utc=False, errors="coerce", format=fmt
        )
    return parsed


def _parse_date_series(s: pd.Series) -> pd.Series:
    """Parse MTGDATE/date values while avoiding pandas' integer-as-nanoseconds trap."""
    parsed_numeric = _parse_numeric_dates(s)
    remaining = parsed_numeric.isna()
    if not remaining.any():
        return parsed_numeric

    text = s.astype("string").str.strip()
    parsed_month_year = _parse_month_year_dates(text)
    out = parsed_numeric.copy()
    month_year = remaining & parsed_month_year.notna()
    out.loc[month_year] = parsed_month_year.loc[month_year]

    remaining = out.isna()
    if not remaining.any():
        return out

    string_candidates = remaining & pd.to_numeric(text, errors="coerce").isna()
    parsed_strings = pd.Series(pd.NaT, index=s.index, dtype="datetime64[ns]")
    parsed_strings.loc[string_candidates] = pd.to_datetime(
        text.loc[string_candidates], utc=False, errors="coerce", format="mixed"
    )
    # If a CSV is exported with day/month/year strings, the default US-oriented
    # parser can silently reject many rows.  Retry those rows with dayfirst=True
    # and keep whichever parse covers more observations.
    parsed_dayfirst = pd.Series(pd.NaT, index=s.index, dtype="datetime64[ns]")
    parsed_dayfirst.loc[string_candidates] = pd.to_datetime(
        text.loc[string_candidates],
        utc=False,
        errors="coerce",
        dayfirst=True,
        format="mixed",
    )
    if parsed_dayfirst.notna().sum() > parsed_strings.notna().sum():
        parsed_strings = parsed_dayfirst

    out.loc[remaining] = parsed_strings.loc[remaining]
    return out


def _aggregate_meeting_shocks_to_month(df: pd.DataFrame, date_col: str, shock_col: str) -> pd.DataFrame:
    d = df[[date_col, shock_col]].copy()
    d[shock_col] = _to_numeric(d[shock_col])
    d["date"] = _parse_date_series(d[date_col]).dt.to_period("M").dt.to_timestamp()
    d = d.dropna(subset=["date", shock_col])
    if d.empty:
        raise ValueError(
            f"Could not parse any valid meeting dates/shocks from columns {date_col!r}, {shock_col!r}"
        )

    monthly = d.groupby("date", as_index=False)[shock_col].sum()
    monthly = monthly.rename(columns={shock_col: "shock"})
    full_months = pd.date_range(monthly["date"].min(), monthly["date"].max(), freq="MS")
    monthly = (
        monthly.set_index("date")
        .reindex(full_months, fill_value=0.0)
        .rename_axis("date")
        .reset_index()
    )
    return monthly[["date", "shock"]].sort_values("date").reset_index(drop=True)


def _load_updated_rr_shocks(path: Path) -> pd.DataFrame:
    raw = _read_csv(path)
    if raw.empty:
        raise ValueError(f"{path} is empty")

    cols_by_upper = {c.upper(): c for c in raw.columns}
    date_col = cols_by_upper.get("MTGDATE")
    if date_col is None:
        raise ValueError(f"{path} must contain an MTGDATE column with FOMC meeting dates")

    shock_col = raw.columns[-1]
    if shock_col == date_col:
        raise ValueError(f"{path} must contain at least one shock column after MTGDATE")

    return _aggregate_meeting_shocks_to_month(raw, date_col, shock_col)


def _load_legacy_monthly_shocks(path: Path) -> pd.DataFrame:
    df = _read_csv(path)
    if "date" not in df.columns or "shock" not in df.columns:
        raise ValueError(f"{path} must have columns: date, shock")
    df = df[["date", "shock"]].copy()
    df["date"] = _parse_date_series(df["date"])
    df["shock"] = _to_numeric(df["shock"])
    df = df.dropna(subset=["date", "shock"])
    return df.sort_values("date").reset_index(drop=True)


def load_rr_shock_monthly(path: Optional[Path] = None) -> pd.DataFrame:
    """Load monetary-policy shocks as monthly ``date``/``shock`` observations.

    By default this reads ``data/RR_MPshocks_Updated(GBforecasts).csv`` and uses
    the last column as the meeting-level shock, summed to monthly frequency with
    zeros for no-meeting months.  A path may be supplied for tests
    or one-off alternatives.  The previous normalized ``data/rr_shock_monthly.csv``
    is still accepted as a fallback when the updated Berkeley CSV has not yet
    been placed in ``data/``.
    """
    if path is not None:
        path = Path(path)
        if path.name == LEGACY_RR_SHOCK_FILENAME:
            return _load_legacy_monthly_shocks(path)
        return _load_updated_rr_shocks(path)

    updated_path = DATA_DIR / UPDATED_RR_SHOCK_FILENAME
    if updated_path.exists():
        return _load_updated_rr_shocks(updated_path)

    legacy_path = DATA_DIR / LEGACY_RR_SHOCK_FILENAME
    if legacy_path.exists():
        return _load_legacy_monthly_shocks(legacy_path)

    raise FileNotFoundError(
        f"Could not find updated Romer--Romer shocks. Place {UPDATED_RR_SHOCK_FILENAME!r} "
        "from Yuriy Gorodnichenko's Berkeley page for 'Innocent Bystanders? "
        "Monetary Policy and Inequality in the U.S.' in the data/ directory. The loader "
        "expects an MTGDATE column and uses the final CSV column as the shock, summing "
        "meeting-level shocks within month and filling no-meeting months with zero. "
        f"The legacy normalized {LEGACY_RR_SHOCK_FILENAME!r} "
        "file is accepted only as a fallback."
    )
