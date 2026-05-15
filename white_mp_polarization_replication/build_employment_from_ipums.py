"""
Build monthly employment panel from an IPUMS CPS extract (BLS CPS, 1983+).

Required IPUMS variables (example names; set `IPUMS_COLUMNS` if yours differ):
  YEAR, MONTH, OCC2010, EMPSTAT, EARNWT or WTFINL (weight)

We count employed civilians (EMPSTAT in 1,12) weighted by person weight.
Routine flag from `occ2010_routine_map.occ2010_is_routine`.

Outputs `data/employment_monthly.csv` with:
  date, routine_emp, nonroutine_emp, total_emp, routine_share
and logs used in local projections: log_routine, log_nonroutine, log_total.
"""

from __future__ import annotations

from pathlib import Path
from typing import Mapping, Optional

import numpy as np
import pandas as pd

from config import DATA_DIR
from occ2010_routine_map import occ2010_is_routine

# Employed (ATUS/IPUMS): 1 = AT work, 12 = has job, not at work
EMPLOYED = {1, 12}

# Default IPUMS column names
DEFAULT_COLS = {
    "year": "YEAR",
    "month": "MONTH",
    "occ": "OCC2010",
    "empstat": "EMPSTAT",
    "weight": "WTFINL",
}


def build_employment_monthly(
    ipums_csv: Path,
    out_csv: Optional[Path] = None,
    colmap: Optional[Mapping[str, str]] = None,
) -> pd.DataFrame:
    out_csv = out_csv or (DATA_DIR / "employment_monthly.csv")
    cmap = {**DEFAULT_COLS, **(colmap or {})}

    df = pd.read_csv(ipums_csv, low_memory=False)
    for k in ("year", "month", "occ", "empstat", "weight"):
        if cmap[k] not in df.columns:
            raise KeyError(f"Missing column {cmap[k]} in IPUMS extract; available: {list(df.columns)}")

    y = df[cmap["year"]].astype(int)
    m = df[cmap["month"]].astype(int)
    occ = df[cmap["occ"]].fillna(0).astype(int)
    emp = df[cmap["empstat"]].astype(int)
    w = df[cmap["weight"]].astype(float)

    use = emp.isin(EMPLOYED) & (w > 0) & (occ > 0)
    sub = pd.DataFrame({"y": y[use], "m": m[use], "occ": occ[use], "w": w[use]})
    sub["routine"] = sub["occ"].map(lambda c: occ2010_is_routine(int(c)))

    sub["date"] = pd.to_datetime(dict(year=sub["y"], month=sub["m"], day=1))
    sub["routine_w"] = np.where(sub["routine"], sub["w"], 0.0)
    sub["nonroutine_w"] = np.where(~sub["routine"], sub["w"], 0.0)

    panel = sub.groupby("date", sort=True).agg(
        total_emp=("w", "sum"),
        routine_emp=("routine_w", "sum"),
        nonroutine_emp=("nonroutine_w", "sum"),
    ).reset_index()
    panel["routine_share"] = panel["routine_emp"] / panel["total_emp"]
    panel["log_total"] = np.log(panel["total_emp"].replace(0, np.nan))
    panel["log_routine"] = np.log(panel["routine_emp"].replace(0, np.nan))
    panel["log_nonroutine"] = np.log(panel["nonroutine_emp"].replace(0, np.nan))

    DATA_DIR.mkdir(parents=True, exist_ok=True)
    panel.to_csv(out_csv, index=False)
    return panel


if __name__ == "__main__":
    import argparse

    p = argparse.ArgumentParser(description="Aggregate IPUMS CPS to monthly routine/nonroutine employment.")
    p.add_argument("ipums_csv", type=Path, help="Path to IPUMS USA CPS extract CSV")
    p.add_argument("-o", "--output", type=Path, default=None, help="Output CSV path (default: data/employment_monthly.csv)")
    args = p.parse_args()
    out_path = args.output or (DATA_DIR / "employment_monthly.csv")
    out = build_employment_monthly(args.ipums_csv, out_csv=out_path)
    print(f"Wrote {out_path} with {len(out)} months.")
