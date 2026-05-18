"""Compatibility wrapper for the project runner.

The active pipeline is `run_all.py`.  This file is kept so older calls to
`python run_all2.py` still work from the replication folder.
"""

from __future__ import annotations

import run_all


def main() -> int:
    return run_all.main()


if __name__ == "__main__":
    raise SystemExit(main())
