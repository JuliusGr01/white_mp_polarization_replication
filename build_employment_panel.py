"""Build the monthly routine/nonroutine employment panel from BLS broad occupations.

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


if __name__ == "__main__":
    write_employment_panel()
