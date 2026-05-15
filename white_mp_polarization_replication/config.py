"""Configuration for White (2022) replication (monthly, Romer–Romer shocks)."""

from pathlib import Path

ROOT = Path(__file__).resolve().parent
DATA_DIR = ROOT / "data"

# Occupational employment panel (BLS CPS, 1983+).
START_DATE = "1983-01-01"
END_DATE = "2020-12-31"

# Romer–Romer shocks end in 2008; local projections in run_all.py use this merge window.
SHOCK_END_DATE = "2008-12-31"

# Local projection horizons (months), inclusive of H.
H_MAX = 48

# Controls: 12 monthly lags of dependent variable and shock; constant + linear time trend (paper).
N_LAGS_Y = 12
N_LAGS_SHOCK = 12

# Newey–West lag length for monthly data (paper uses NW; 12–18 common for monthly).
NW_LAGS = 18
