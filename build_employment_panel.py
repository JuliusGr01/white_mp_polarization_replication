"""Build the monthly routine/nonroutine employment panel.

White (2022) follows the Autor-Levy-Murnane task grouping. With the public BLS
1983+ broad occupation series available in this repo, the closest consistent split is:

Routine:
  - Sales and office occupations
  - Natural resources, construction, and maintenance occupations
  - Production, transportation, and material moving occupations

Nonroutine:
  - Management, professional, and related occupations
  - Service occupations
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

from config import DATA_DIR

BLS_OCC_SOURCE = DATA_DIR / "bls_occ_employed_monthly.csv"
EMPLOYMENT_PANEL = DATA_DIR / "employment_monthly.csv"
EE_1969_1982_PANEL = DATA_DIR / "cps_ee_1969_1982_employment_monthly.csv"
EXTENDED_EMPLOYMENT_PANEL = DATA_DIR / "employment_monthly_extended.csv"

ROUTINE_SERIES_IDS = {
    "LNU02032205",  # Sales and office occupations
    "LNU02032208",  # Natural resources, construction, and maintenance occupations
    "LNU02032212",  # Production, transportation, and material moving occupations
}

NONROUTINE_SERIES_IDS = {
    "LNU02032201",  # Management, professional, and related occupations
    "LNU02032204",  # Service occupations
}

EXPECTED_SERIES_IDS = ROUTINE_SERIES_IDS | NONROUTINE_SERIES_IDS


def build_employment_panel(source_path: Path = BLS_OCC_SOURCE) -> pd.DataFrame:
    raw = pd.read_csv(source_path, parse_dates=["date"])
    missing = EXPECTED_SERIES_IDS - set(raw["series_id"].unique())
    if missing:
        raise ValueError(f"{source_path} is missing expected BLS series: {sorted(missing)}")

    raw = raw[raw["series_id"].isin(EXPECTED_SERIES_IDS)].copy()
    raw["employed"] = pd.to_numeric(raw["employed_thousands"], errors="coerce") * 1000.0
    raw["is_routine"] = raw["series_id"].isin(ROUTINE_SERIES_IDS)
    raw = raw.dropna(subset=["date", "employed"])

    grouped = raw.groupby(["date", "is_routine"], as_index=True)["employed"].sum().unstack()
    grouped = grouped.rename(columns={True: "routine_emp", False: "nonroutine_emp"})
    out = grouped.reset_index().sort_values("date")
    out["total_emp"] = out["routine_emp"] + out["nonroutine_emp"]
    out["routine_share"] = out["routine_emp"] / out["total_emp"]

    for name in ("total", "routine", "nonroutine"):
        out[f"log_{name}"] = np.log(out[f"{name}_emp"])

    return out[
        [
            "date",
            "routine_emp",
            "nonroutine_emp",
            "total_emp",
            "routine_share",
            "log_total",
            "log_routine",
            "log_nonroutine",
        ]
    ]


def write_employment_panel(
    source_path: Path = BLS_OCC_SOURCE,
    out_path: Path = EMPLOYMENT_PANEL,
) -> pd.DataFrame:
    panel = build_employment_panel(source_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    panel.to_csv(out_path, index=False)
    return panel


def build_extended_employment_panel(
    ee_path: Path = EE_1969_1982_PANEL,
    bls_source_path: Path = BLS_OCC_SOURCE,
) -> pd.DataFrame:
    """Build the 1969+ employment panel used in White's baseline sample.

    The pre-1983 segment comes from the user's Employment and Earnings Excel
    extraction, already converted into the standard monthly layout. The 1983+
    segment comes from the BLS broad-occupation series.
    """
    if not ee_path.exists():
        raise FileNotFoundError(
            f"Missing {ee_path}. Run build_cps_ee_1969_1982_panel.py with the Excel input, "
            "or place the prebuilt cps_ee_1969_1982_employment_monthly.csv in data/."
        )
    if not bls_source_path.exists():
        raise FileNotFoundError(f"Missing {bls_source_path}.")

    ee = pd.read_csv(ee_path, parse_dates=["date"])
    bls = build_employment_panel(bls_source_path)

    required = [
        "date",
        "routine_emp",
        "nonroutine_emp",
        "total_emp",
        "routine_share",
        "log_total",
        "log_routine",
        "log_nonroutine",
    ]
    missing = set(required) - set(ee.columns)
    if missing:
        raise ValueError(f"{ee_path} is missing required columns: {sorted(missing)}")

    ee = ee[required].copy()
    ee = ee[(ee["date"] >= "1969-01-01") & (ee["date"] < "1983-01-01")]
    bls = bls[bls["date"] >= "1983-01-01"]
    out = pd.concat([ee, bls], ignore_index=True).sort_values("date")
    out = out.drop_duplicates(subset=["date"], keep="first").reset_index(drop=True)

    expected = pd.date_range(out["date"].min(), out["date"].max(), freq="MS")
    missing_months = expected.difference(pd.DatetimeIndex(out["date"]))
    if len(missing_months):
        preview = ", ".join(d.strftime("%Y-%m-%d") for d in missing_months[:12])
        raise ValueError(f"Extended employment panel is not monthly-continuous. Missing: {preview}")

    return out


def write_extended_employment_panel(
    out_path: Path = EXTENDED_EMPLOYMENT_PANEL,
    ee_path: Path = EE_1969_1982_PANEL,
    bls_source_path: Path = BLS_OCC_SOURCE,
) -> pd.DataFrame:
    panel = build_extended_employment_panel(ee_path, bls_source_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    panel.to_csv(out_path, index=False)
    return panel


if __name__ == "__main__":
    write_extended_employment_panel()
