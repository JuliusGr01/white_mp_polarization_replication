"""Recreate White (2022) Figures 1 and 2 for the available BLS sample."""

from __future__ import annotations

import sys
from pathlib import Path

_ROOT = Path(__file__).resolve().parent
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))

import matplotlib

matplotlib.use("Agg")

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import pandas as pd

from build_employment_panel import build_employment_panel
from config import DATA_DIR, END_DATE, ROOT, START_DATE

BLS_ALLDATA = DATA_DIR / "bls_raw" / "ln.data.1.AllData"
POPULATION_SERIES_ID = "LNU00000000"  # Civilian noninstitutional population, thousands.
PAPER_FIGURES_END_DATE = "2019-12-31"

RECESSIONS = [
    ("1981-07-01", "1982-11-01"),
    ("1990-07-01", "1991-03-01"),
    ("2001-03-01", "2001-11-01"),
    ("2007-12-01", "2009-06-01"),
    ("2020-02-01", "2020-04-01"),
]


def load_bls_monthly_series(series_id: str, source_path: Path = BLS_ALLDATA) -> pd.DataFrame:
    raw = pd.read_csv(source_path, sep=r"\s+", engine="python")
    raw = raw[(raw["series_id"] == series_id) & raw["period"].str.match(r"M\d{2}")]
    raw = raw[raw["period"] != "M13"].copy()
    raw["month"] = raw["period"].str[1:].astype(int)
    raw["date"] = pd.to_datetime(
        {"year": raw["year"].astype(int), "month": raw["month"], "day": 1}
    )
    raw["value"] = pd.to_numeric(raw["value"], errors="coerce")
    return raw[["date", "value"]].dropna().sort_values("date")


def build_descriptive_panel() -> pd.DataFrame:
    emp = build_employment_panel()
    pop = load_bls_monthly_series(POPULATION_SERIES_ID).rename(
        columns={"value": "civilian_noninstitutional_population_thousands"}
    )
    panel = emp.merge(pop, on="date", how="inner")
    panel["population"] = panel["civilian_noninstitutional_population_thousands"] * 1000.0
    panel["routine_emp_per_capita"] = panel["routine_emp"] / panel["population"]
    panel["routine_share_percent"] = panel["routine_share"] * 100.0
    end_date = min(pd.Timestamp(END_DATE), pd.Timestamp(PAPER_FIGURES_END_DATE))
    panel = panel[(panel["date"] >= pd.Timestamp(START_DATE)) & (panel["date"] <= end_date)]
    return panel.reset_index(drop=True)


def shade_recessions(ax: plt.Axes) -> None:
    for start, end in RECESSIONS:
        ax.axvspan(pd.Timestamp(start), pd.Timestamp(end), color="0.85", zorder=0)


def format_axis(ax: plt.Axes, ylabel: str, y_ticks: list[float], y_lim: tuple[float, float]) -> None:
    ax.set_ylabel(ylabel)
    ax.set_xlim(pd.Timestamp(START_DATE), pd.Timestamp("2020-01-01"))
    ax.set_ylim(*y_lim)
    ax.set_yticks(y_ticks)
    ax.xaxis.set_major_locator(mdates.YearLocator(5))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.set_xlabel("Year")
    ax.grid(False)


def plot_line(
    panel: pd.DataFrame,
    column: str,
    ylabel: str,
    title: str,
    y_ticks: list[float],
    y_lim: tuple[float, float],
    out_path: Path,
) -> None:
    plt.rcParams.update({"font.family": "serif"})
    fig, ax = plt.subplots(figsize=(6.5, 3.8))
    shade_recessions(ax)
    ax.plot(panel["date"], panel[column], color="0.0", linewidth=1.4, zorder=2)
    ax.set_title(title)
    format_axis(ax, ylabel, y_ticks, y_lim)
    fig.tight_layout()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=200)
    plt.close(fig)


def main() -> None:
    out_dir = ROOT / "output"
    panel = build_descriptive_panel()
    panel[
        [
            "date",
            "routine_emp",
            "nonroutine_emp",
            "total_emp",
            "population",
            "routine_emp_per_capita",
            "routine_share",
            "routine_share_percent",
        ]
    ].to_csv(out_dir / "figures_1_2_series.csv", index=False)

    plot_line(
        panel,
        "routine_emp_per_capita",
        "Routine Emp. / Civilian Pop. (16+)",
        "Per Capita Employment in Routine Jobs",
        [0.25, 0.30, 0.35],
        (0.25, 0.35),
        out_dir / "figure1_routine_employment_per_capita.png",
    )
    plot_line(
        panel,
        "routine_share_percent",
        "Percent of Total Employment",
        "Routine Jobs as a Share of Total Employment",
        [40, 45, 50, 55, 60, 65],
        (40, 65),
        out_dir / "figure2_routine_employment_share.png",
    )
    print(f"Wrote Figure 1 and 2 recreations under {out_dir}")


if __name__ == "__main__":
    main()
