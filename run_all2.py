"""Project runner for the White-style 1969 sample.

The default run now:
  1. rebuilds the 1969-1982 Employment and Earnings occupation panel from the
     Excel extraction,
  2. runs the White baseline R estimation, which binds that panel to the
     post-1982 CPS panel and estimates Figure-3-style responses.

Use `--legacy-sample` only when you deliberately want the older CPS-only setup.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parent


def find_rscript() -> str | None:
    direct = shutil.which("Rscript")
    if direct:
        return direct

    candidates = sorted(
        Path("C:/Program Files/R").glob("R-*/bin/Rscript.exe"),
        reverse=True,
    )
    if candidates:
        return str(candidates[0])
    return None


def run(cmd: list[str], env: dict[str, str] | None = None) -> None:
    print("+", " ".join(cmd), flush=True)
    subprocess.run(cmd, cwd=PROJECT_ROOT, env=env, check=True)


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Run the MP_LaborPol pipeline.")
    parser.add_argument(
        "--legacy-sample",
        action="store_true",
        help="Use the older CPS-only sample instead of the White 1969 sample.",
    )
    parser.add_argument(
        "--sample-start",
        default="1969-01-01",
        help="Monthly sample start passed to R. Default: 1969-01-01.",
    )
    parser.add_argument(
        "--sample-end",
        default=None,
        help="Optional monthly sample end. By default R uses the end of the local Romer/Romer shock file.",
    )
    parser.add_argument(
        "--skip-r",
        action="store_true",
        help="Only rebuild the Excel-derived 1969-1982 panel.",
    )
    parser.add_argument(
        "--r-entry",
        default="code/white_1969_estimation.R",
        help="R entry script to run via Rscript. Default: code/white_1969_estimation.R.",
    )
    parser.add_argument(
        "--rscript",
        default=None,
        help="Explicit path to Rscript.exe when R is not on PATH.",
    )
    return parser


def main() -> int:
    args = build_arg_parser().parse_args()

    run([sys.executable, "code/build_cps_ee_1969_1982_panel.py"])

    if args.skip_r:
        return 0

    rscript = args.rscript or find_rscript()
    if rscript is None:
        raise SystemExit(
            "Could not find Rscript. The Excel panel was rebuilt, but the R estimation setup was not run."
        )

    env = os.environ.copy()
    env["MP_LABORPOL_USE_WHITE_1969"] = "0" if args.legacy_sample else "1"
    env["MP_LABORPOL_SAMPLE_START"] = args.sample_start
    if args.sample_end:
        env["MP_LABORPOL_SAMPLE_END"] = args.sample_end

    run([rscript, args.r_entry], env=env)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
