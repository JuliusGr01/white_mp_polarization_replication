"""Configuration for White (2022) replication (monthly, Romer--Romer shocks)."""

from pathlib import Path

ROOT = Path(__file__).resolve().parent
DATA_DIR = ROOT / "data"

START_DATE = "1969-01-01"
END_DATE = "2020-12-31"
SHOCK_END_DATE = "2008-12-31"

H_MAX = 48
N_LAGS_Y = 12
N_LAGS_SHOCK = 12
NW_LAGS = 12

LP_Y_LAG_TRANSFORM = "diff"
LP_INCLUDE_TIME_TREND = False
