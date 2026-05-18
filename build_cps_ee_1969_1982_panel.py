"""Convert historical Employment and Earnings CPS tables into ALM panels.

The input workbook is a table dump from the 1969-1982 Employment and Earnings
PDFs. It contains both parent rows (Total, White-collar workers, Blue-collar
workers, etc.) and finer rows. This script keeps only the finest rows available
inside each table period, maps them to Autor-Levy-Murnane-style task groups, and
writes merge-ready monthly panels.

The script deliberately avoids an Excel dependency such as openpyxl. It reads
the .xlsx file directly with the standard-library zip/xml modules, then uses
pandas for the small amount of tabular work.

Outputs:
  data/data_processed/cps_ee_1969_1982_leaf_alm_long.csv
  data/data_processed/cps_ee_1969_1982_alm_panel.csv
  data/data_processed/cps_ee_1969_1982_employment_monthly.csv
  data/data_processed/cps_ee_1969_1982_alm_crosswalk.csv
  data/data_processed/cps_ee_1969_1982_audit.csv
"""

from __future__ import annotations

import argparse
import math
import re
import zipfile
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from datetime import date
from pathlib import Path

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_INPUT = PROJECT_ROOT / "data" / "data_raw" / "1969_1982_CPS.xlsx"
DEFAULT_OUT_DIR = PROJECT_ROOT / "data" / "data_processed"

LONG_OUT = "cps_ee_1969_1982_leaf_alm_long.csv"
PANEL_OUT = "cps_ee_1969_1982_alm_panel.csv"
EMPLOYMENT_MONTHLY_OUT = "cps_ee_1969_1982_employment_monthly.csv"
CROSSWALK_OUT = "cps_ee_1969_1982_alm_crosswalk.csv"
AUDIT_OUT = "cps_ee_1969_1982_audit.csv"

P1 = "011969 - 011972"
P2 = "021972 - 011974"
P3 = "021974 - 121982"

TOP_LEVEL_CHILDREN_BY_PERIOD = {
    P1: [2, 17, 35, 41],
    P2: [2, 17, 36, 42],
    P3: [2, 17, 36, 42],
}

KNOWN_VALUE_CORRECTIONS = {
    (
        "1973_08",
        P2,
        30,
    ): (
        2726.0,
        "Corrected from 3726 using the transport-equipment parent residual: 3174 - 448.",
    ),
    (
        "1981_04",
        P3,
        45,
    ): (
        810.0,
        "Corrected from 210 using the farm-laborers parent residual: 1030 - 220.",
    ),
}


@dataclass(frozen=True)
class SchemaRow:
    period: str
    occ_pos: int
    parent: str | None
    is_leaf: bool
    alm_group: str | None
    alm_detail: str | None
    notes: str = ""

    @property
    def task_binary(self) -> str | None:
        if self.alm_group == "Routine":
            return "routine"
        if self.alm_group in {"Abstract", "Manual"}:
            return "nonroutine"
        return None


def build_schema() -> dict[tuple[str, int], SchemaRow]:
    rows: list[SchemaRow] = []

    def add(
        period: str,
        positions: list[int],
        parent: str | None,
        is_leaf: bool,
        alm_group: str | None,
        alm_detail: str | None,
        notes: str = "",
    ) -> None:
        for pos in positions:
            rows.append(SchemaRow(period, pos, parent, is_leaf, alm_group, alm_detail, notes))

    p1 = P1
    add(p1, [1], None, False, None, None, "published total; parent row")
    add(p1, [2], "Total", False, None, None, "white-collar parent row")
    add(p1, [3], "White-collar workers", False, None, None, "professional parent row")
    add(p1, [4, 5, 6], "Professional and technical", True, "Abstract", "professional_technical")
    add(p1, [7], "White-collar workers", False, None, None, "managerial parent row")
    add(p1, [8, 9, 10], "Managers, officials, and proprietors", True, "Abstract", "management")
    add(p1, [11], "White-collar workers", False, None, None, "clerical parent row")
    add(p1, [12, 13], "Clerical workers", True, "Routine", "routine_cognitive_clerical")
    add(p1, [14], "White-collar workers", False, None, None, "sales parent row")
    add(p1, [15, 16], "Sales workers", True, "Routine", "routine_cognitive_sales")
    add(p1, [17], "Total", False, None, None, "blue-collar parent row")
    add(p1, [18], "Blue-collar workers", False, None, None, "craft parent row")
    add(p1, [19, 20, 21, 22, 23, 24], "Craftsmen and foremen", True, "Routine", "routine_manual_craft")
    add(p1, [25], "Blue-collar workers", False, None, None, "operatives parent row")
    add(p1, [26], "Operatives", True, "Routine", "routine_manual_transport")
    add(p1, [27], "Operatives", False, None, None, "other operatives parent row")
    add(p1, [28, 29, 30], "Other operatives", True, "Routine", "routine_manual_operators")
    add(p1, [31], "Blue-collar workers", False, None, None, "laborers parent row")
    add(p1, [32, 33, 34], "Nonfarm laborers", True, "Routine", "routine_manual_laborers")
    add(p1, [35], "Total", False, None, None, "service parent row")
    add(p1, [36], "Service workers", True, "Manual", "nonroutine_manual_private_household")
    add(p1, [37], "Service workers", False, None, None, "service except private household parent row")
    add(p1, [38], "Service workers, except private household", True, "Manual", "nonroutine_manual_protective")
    add(p1, [39], "Service workers, except private household", True, "Manual", "nonroutine_manual_food")
    add(p1, [40], "Service workers, except private household", True, "Manual", "nonroutine_manual_other_service")
    add(p1, [41], "Total", False, None, None, "farm parent row")
    add(p1, [42], "Farm workers", True, "Routine", "routine_manual_farmers")
    add(p1, [43], "Farm workers", False, None, None, "farm labor parent row")
    add(p1, [44, 45], "Farm laborers and foremen", True, "Routine", "routine_manual_farm_labor")

    p2 = P2
    add(p2, [1], None, False, None, None, "published total; parent row")
    add(p2, [2], "Total", False, None, None, "white-collar parent row")
    add(p2, [3], "White-collar workers", False, None, None, "professional parent row")
    add(p2, [4, 5, 6], "Professional and technical", True, "Abstract", "professional_technical")
    add(p2, [7], "White-collar workers", False, None, None, "managerial parent row")
    add(p2, [8, 9, 10], "Managers and administrators, except farm", True, "Abstract", "management")
    add(p2, [11], "White-collar workers", False, None, None, "sales parent row")
    add(p2, [12, 13], "Sales workers", True, "Routine", "routine_cognitive_sales")
    add(p2, [14], "White-collar workers", False, None, None, "clerical parent row")
    add(p2, [15, 16], "Clerical workers", True, "Routine", "routine_cognitive_clerical")
    add(p2, [17], "Total", False, None, None, "blue-collar parent row")
    add(p2, [18], "Blue-collar workers", False, None, None, "craft parent row")
    add(p2, [19, 20, 21, 22, 23, 24], "Craftsmen and kindred workers", True, "Routine", "routine_manual_craft")
    add(p2, [25], "Blue-collar workers", False, None, None, "operatives except transport parent row")
    add(p2, [26, 27, 28], "Operatives, except transport", True, "Routine", "routine_manual_operators")
    add(p2, [29], "Blue-collar workers", False, None, None, "transport operatives parent row")
    add(p2, [30, 31], "Transport equipment operatives", True, "Routine", "routine_manual_transport")
    add(p2, [32], "Blue-collar workers", False, None, None, "laborers parent row")
    add(p2, [33, 34, 35], "Nonfarm laborers", True, "Routine", "routine_manual_laborers")
    add(p2, [36], "Total", False, None, None, "service parent row")
    add(p2, [37], "Service workers", True, "Manual", "nonroutine_manual_private_household")
    add(p2, [38], "Service workers", False, None, None, "service except private household parent row")
    add(p2, [39], "Service workers, except private household", True, "Manual", "nonroutine_manual_food")
    add(p2, [40], "Service workers, except private household", True, "Manual", "nonroutine_manual_protective")
    add(p2, [41], "Service workers, except private household", True, "Manual", "nonroutine_manual_other_service")
    add(p2, [42], "Total", False, None, None, "farm parent row")
    add(p2, [43], "Farm workers", True, "Routine", "routine_manual_farmers")
    add(p2, [44], "Farm workers", False, None, None, "farm labor parent row")
    add(p2, [45, 46], "Farm laborers and foremen", True, "Routine", "routine_manual_farm_labor")

    p3 = P3
    add(p3, [1], None, False, None, None, "published total; parent row")
    add(p3, [2], "Total", False, None, None, "white-collar parent row")
    add(p3, [3], "White-collar workers", False, None, None, "professional parent row")
    add(p3, [4, 5, 6], "Professional and technical", True, "Abstract", "professional_technical")
    add(p3, [7], "White-collar workers", False, None, None, "managerial parent row")
    add(p3, [8, 9, 10], "Managers and administrators, except farm", True, "Abstract", "management")
    add(p3, [11], "White-collar workers", False, None, None, "sales parent row")
    add(p3, [12, 13], "Sales workers", True, "Routine", "routine_cognitive_sales")
    add(p3, [14], "White-collar workers", False, None, None, "clerical parent row")
    add(p3, [15, 16], "Clerical workers", True, "Routine", "routine_cognitive_clerical")
    add(p3, [17], "Total", False, None, None, "blue-collar parent row")
    add(p3, [18], "Blue-collar workers", False, None, None, "craft parent row")
    add(p3, [19, 20, 21, 22, 23, 24], "Craft and kindred workers", True, "Routine", "routine_manual_craft")
    add(p3, [25], "Blue-collar workers", False, None, None, "operatives except transport parent row")
    add(p3, [26, 27, 28], "Operatives, except transport", True, "Routine", "routine_manual_operators")
    add(p3, [29], "Blue-collar workers", False, None, None, "transport operatives parent row")
    add(p3, [30, 31], "Transport equipment operatives", True, "Routine", "routine_manual_transport")
    add(p3, [32], "Blue-collar workers", False, None, None, "laborers parent row")
    add(p3, [33, 34, 35], "Nonfarm laborers", True, "Routine", "routine_manual_laborers")
    add(p3, [36], "Total", False, None, None, "service parent row")
    add(p3, [37], "Service workers", True, "Manual", "nonroutine_manual_private_household")
    add(p3, [38], "Service workers", False, None, None, "service except private household parent row")
    add(p3, [39], "Service workers, except private household", True, "Manual", "nonroutine_manual_food")
    add(p3, [40], "Service workers, except private household", True, "Manual", "nonroutine_manual_protective")
    add(p3, [41], "Service workers, except private household", True, "Manual", "nonroutine_manual_other_service")
    add(p3, [42], "Total", False, None, None, "farm parent row")
    add(p3, [43], "Farm workers", True, "Routine", "routine_manual_farmers")
    add(p3, [44], "Farm workers", False, None, None, "farm labor parent row")
    add(p3, [45, 46], "Farm laborers and supervisors", True, "Routine", "routine_manual_farm_labor")

    out = {(row.period, row.occ_pos): row for row in rows}
    if len(out) != len(rows):
        raise RuntimeError("Duplicate schema keys detected.")
    return out


def xlsx_col_to_index(cell_ref: str) -> int:
    letters = "".join(ch for ch in cell_ref if ch.isalpha())
    value = 0
    for char in letters:
        value = value * 26 + ord(char.upper()) - 64
    return value - 1


def cell_text(cell: ET.Element, shared_strings: list[str], ns: dict[str, str]) -> str:
    if cell.attrib.get("t") == "inlineStr":
        return "".join(t.text or "" for t in cell.findall(".//x:t", ns))
    value = cell.find("x:v", ns)
    if value is None or value.text is None:
        return ""
    if cell.attrib.get("t") == "s":
        return shared_strings[int(value.text)]
    return value.text


def read_xlsx_first_sheet(path: Path) -> pd.DataFrame:
    ns = {"x": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
    with zipfile.ZipFile(path) as archive:
        shared_strings: list[str] = []
        if "xl/sharedStrings.xml" in archive.namelist():
            root = ET.fromstring(archive.read("xl/sharedStrings.xml"))
            for item in root.findall("x:si", ns):
                shared_strings.append("".join(t.text or "" for t in item.findall(".//x:t", ns)))

        root = ET.fromstring(archive.read("xl/worksheets/sheet1.xml"))
        rows: list[list[str]] = []
        max_col = 0
        for row in root.findall(".//x:sheetData/x:row", ns):
            values: dict[int, str] = {}
            for cell in row.findall("x:c", ns):
                col_idx = xlsx_col_to_index(cell.attrib["r"])
                max_col = max(max_col, col_idx)
                values[col_idx] = cell_text(cell, shared_strings, ns)
            rows.append([values.get(idx, "") for idx in range(max_col + 1)])

    if not rows:
        raise RuntimeError(f"{path} did not contain any rows.")
    header = [str(value).strip() for value in rows[0]]
    return pd.DataFrame(rows[1:], columns=header)


def parse_month_year(value: str) -> date:
    match = re.fullmatch(r"\s*(\d{4})[_-](\d{1,2})\s*", str(value))
    if not match:
        raise ValueError(f"Could not parse Month_Year value {value!r}")
    year = int(match.group(1))
    month = int(match.group(2))
    if not 1 <= month <= 12:
        raise ValueError(f"Invalid month in Month_Year value {value!r}")
    return date(year, month, 1)


def parse_employment_value(value: object) -> float:
    """Parse table employment counts reported in thousands.

    The workbook mixes plain integers, comma thousands separators, and cells
    where the PDF thousands comma was interpreted as a decimal point
    (for example, 75.358 means 75,358, not 75.358).
    """
    text = str(value).strip().replace("\u00a0", "")
    if text == "":
        return float("nan")
    if "," in text:
        return float(text.replace(",", ""))

    numeric = float(text)
    if "." in text and not numeric.is_integer():
        return float(round(numeric * 1000))
    return numeric


def apply_known_value_corrections(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["value_thousands_original"] = out["value_thousands"]
    out["value_correction_note"] = ""

    for (month_year, period, occ_pos), (corrected_value, note) in KNOWN_VALUE_CORRECTIONS.items():
        mask = (
            out["Month_Year"].eq(month_year)
            & out["period"].eq(period)
            & out["occ_pos"].eq(occ_pos)
        )
        if mask.sum() != 1:
            raise RuntimeError(
                "Known correction did not match exactly one row: "
                f"{month_year}, {period}, occ_pos {occ_pos}"
            )
        out.loc[mask, "value_thousands"] = corrected_value
        out.loc[mask, "value_correction_note"] = note

    return out


def clean_input(df: pd.DataFrame) -> pd.DataFrame:
    required = {"value", "occupation", "period", "Month_Year", "occ_pos"}
    missing = required - set(df.columns)
    if missing:
        raise RuntimeError(f"Workbook missing required columns: {sorted(missing)}")

    out = df.copy()
    out["occupation"] = out["occupation"].astype(str).str.strip()
    out["period"] = out["period"].astype(str).str.strip()
    out["occ_pos"] = pd.to_numeric(out["occ_pos"], errors="raise").astype(int)
    out["value_thousands"] = out["value"].map(parse_employment_value)
    out["ym"] = out["Month_Year"].map(parse_month_year)
    out["year"] = out["ym"].map(lambda x: x.year)
    out["month"] = out["ym"].map(lambda x: x.month)
    if out["value_thousands"].isna().any():
        bad = out.loc[out["value_thousands"].isna(), ["Month_Year", "occupation", "value"]].head()
        raise RuntimeError(f"Non-numeric value rows found:\n{bad}")
    out = apply_known_value_corrections(out)
    return out


def attach_schema(df: pd.DataFrame, schema: dict[tuple[str, int], SchemaRow]) -> pd.DataFrame:
    rows = []
    missing: list[tuple[str, int, str]] = []
    for record in df.to_dict("records"):
        key = (record["period"], record["occ_pos"])
        meta = schema.get(key)
        if meta is None:
            missing.append((record["period"], record["occ_pos"], record["occupation"]))
            continue
        rows.append(
            {
                **record,
                "parent_occupation": meta.parent,
                "is_leaf": meta.is_leaf,
                "alm_group": meta.alm_group,
                "alm_detail": meta.alm_detail,
                "task_binary": meta.task_binary,
                "classification_notes": meta.notes,
            }
        )
    if missing:
        preview = "\n".join(map(str, sorted(set(missing))[:20]))
        raise RuntimeError(f"Missing schema rows for period/occ_pos combinations:\n{preview}")
    return pd.DataFrame(rows)


def build_panel(leaf: pd.DataFrame) -> pd.DataFrame:
    grouped = (
        leaf.groupby(["ym", "alm_group"], as_index=False)["employment"]
        .sum()
        .pivot(index="ym", columns="alm_group", values="employment")
        .fillna(0.0)
        .reset_index()
    )
    for col in ["Routine", "Abstract", "Manual"]:
        if col not in grouped.columns:
            grouped[col] = 0.0

    grouped["routine_emp_abs"] = grouped["Routine"]
    grouped["abstract_emp_abs"] = grouped["Abstract"]
    grouped["manual_emp_abs"] = grouped["Manual"]
    grouped["nonroutine_emp_abs"] = grouped["abstract_emp_abs"] + grouped["manual_emp_abs"]
    grouped["total_emp_abs"] = grouped["routine_emp_abs"] + grouped["nonroutine_emp_abs"]
    grouped["routine_emp_rel_emp"] = grouped["routine_emp_abs"] / grouped["total_emp_abs"]
    grouped["nonroutine_emp_rel_emp"] = grouped["nonroutine_emp_abs"] / grouped["total_emp_abs"]
    grouped["routine_emp_share"] = grouped["routine_emp_rel_emp"]

    for col in ["routine_emp_abs", "nonroutine_emp_abs", "total_emp_abs", "abstract_emp_abs", "manual_emp_abs"]:
        grouped[f"log_{col.replace('_abs', '')}"] = grouped[col].map(lambda x: math.log(x) if x > 0 else float("nan"))

    grouped["ym"] = pd.to_datetime(grouped["ym"])
    grouped = grouped.sort_values("ym")
    return grouped[
        [
            "ym",
            "routine_emp_abs",
            "nonroutine_emp_abs",
            "abstract_emp_abs",
            "manual_emp_abs",
            "total_emp_abs",
            "routine_emp_rel_emp",
            "nonroutine_emp_rel_emp",
            "routine_emp_share",
            "log_routine_emp",
            "log_nonroutine_emp",
            "log_abstract_emp",
            "log_manual_emp",
            "log_total_emp",
        ]
    ]


def build_employment_monthly(panel: pd.DataFrame) -> pd.DataFrame:
    out = panel.rename(
        columns={
            "ym": "date",
            "routine_emp_abs": "routine_emp",
            "nonroutine_emp_abs": "nonroutine_emp",
            "total_emp_abs": "total_emp",
            "routine_emp_share": "routine_share",
        }
    ).copy()
    out["log_total"] = out["log_total_emp"]
    out["log_routine"] = out["log_routine_emp"]
    out["log_nonroutine"] = out["log_nonroutine_emp"]
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


def build_top_category_totals(df: pd.DataFrame) -> pd.DataFrame:
    pieces = []
    for period, children in TOP_LEVEL_CHILDREN_BY_PERIOD.items():
        period_rows = df.loc[df["period"].eq(period) & df["occ_pos"].isin(children)]
        pieces.append(
            period_rows.groupby("ym", as_index=False)["value_thousands"]
            .sum()
            .rename(columns={"value_thousands": "top_category_total_thousands"})
        )
    if not pieces:
        return pd.DataFrame(columns=["ym", "top_category_total_thousands"])
    return pd.concat(pieces, ignore_index=True)


def build_audit(df: pd.DataFrame, leaf: pd.DataFrame) -> pd.DataFrame:
    total_rows = (
        df.loc[df["occ_pos"].eq(1), ["ym", "value_thousands"]]
        .rename(columns={"value_thousands": "published_total_thousands"})
        .drop_duplicates(subset=["ym"])
    )
    top_totals = build_top_category_totals(df)
    leaf_total = (
        leaf.groupby("ym", as_index=False)["value_thousands"]
        .sum()
        .rename(columns={"value_thousands": "leaf_total_thousands"})
    )
    audit = total_rows.merge(top_totals, on="ym", how="outer").merge(leaf_total, on="ym", how="outer")
    audit["coverage_ratio"] = audit["leaf_total_thousands"] / audit["published_total_thousands"]
    audit["coverage_gap_thousands"] = audit["leaf_total_thousands"] - audit["published_total_thousands"]
    audit["leaf_to_top_category_ratio"] = audit["leaf_total_thousands"] / audit["top_category_total_thousands"]
    audit["leaf_to_top_category_gap_thousands"] = (
        audit["leaf_total_thousands"] - audit["top_category_total_thousands"]
    )
    audit["published_to_top_category_ratio"] = (
        audit["published_total_thousands"] / audit["top_category_total_thousands"]
    )
    audit["published_to_top_category_gap_thousands"] = (
        audit["published_total_thousands"] - audit["top_category_total_thousands"]
    )

    def status(row: pd.Series) -> str:
        leaf_ratio = row["leaf_to_top_category_ratio"]
        published_ratio = row["published_to_top_category_ratio"]
        coverage_ratio = row["coverage_ratio"]
        leaf_matches_top = pd.notna(leaf_ratio) and 0.995 <= leaf_ratio <= 1.005
        published_matches_top = pd.notna(published_ratio) and 0.995 <= published_ratio <= 1.005
        leaf_matches_published = pd.notna(coverage_ratio) and 0.995 <= coverage_ratio <= 1.005

        if leaf_matches_top and not published_matches_top:
            return "source_total_needs_review"
        if leaf_matches_published and not leaf_matches_top:
            return "source_top_category_needs_review"
        if not leaf_matches_top:
            return "leaf_needs_review"
        return "ok"

    audit["status"] = audit.apply(status, axis=1)
    audit["ym"] = pd.to_datetime(audit["ym"])
    return audit.sort_values("ym")


def write_outputs(df: pd.DataFrame, out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    schema = build_schema()
    classified = attach_schema(df, schema)

    crosswalk = (
        classified[
            [
                "period",
                "occ_pos",
                "occupation",
                "parent_occupation",
                "is_leaf",
                "alm_group",
                "alm_detail",
                "task_binary",
                "classification_notes",
            ]
        ]
        .drop_duplicates()
        .sort_values(["period", "occ_pos"])
    )

    leaf = classified.loc[classified["is_leaf"]].copy()
    leaf["employment"] = leaf["value_thousands"] * 1000.0
    leaf = leaf.sort_values(["ym", "occ_pos"])

    panel = build_panel(leaf)
    employment_monthly = build_employment_monthly(panel)
    audit = build_audit(classified, leaf)

    leaf.to_csv(out_dir / LONG_OUT, index=False)
    panel.to_csv(out_dir / PANEL_OUT, index=False)
    employment_monthly.to_csv(out_dir / EMPLOYMENT_MONTHLY_OUT, index=False)
    crosswalk.to_csv(out_dir / CROSSWALK_OUT, index=False)
    audit.to_csv(out_dir / AUDIT_OUT, index=False)

    print(f"Wrote {out_dir / LONG_OUT}")
    print(f"Wrote {out_dir / PANEL_OUT}")
    print(f"Wrote {out_dir / EMPLOYMENT_MONTHLY_OUT}")
    print(f"Wrote {out_dir / CROSSWALK_OUT}")
    print(f"Wrote {out_dir / AUDIT_OUT}")
    print()
    print("Panel rows:", len(panel))
    print("Date range:", panel["ym"].min().date(), "to", panel["ym"].max().date())
    print("Audit status counts:")
    print(audit["status"].value_counts(dropna=False).to_string())


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Build a merge-ready 1969-1982 ALM employment panel from the CPS Employment and Earnings workbook."
    )
    parser.add_argument("--input", type=Path, default=DEFAULT_INPUT, help="Path to 1969_1982_CPS.xlsx")
    parser.add_argument("--out-dir", type=Path, default=DEFAULT_OUT_DIR, help="Output directory")
    return parser


def main() -> None:
    args = build_arg_parser().parse_args()
    raw = read_xlsx_first_sheet(args.input)
    cleaned = clean_input(raw)
    write_outputs(cleaned, args.out_dir)


if __name__ == "__main__":
    main()
