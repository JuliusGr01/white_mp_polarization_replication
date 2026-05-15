"""Plot IRFs with 90% error bands (White 2022 style)."""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

from local_projections import IRFResult


def plot_irf(
    res: IRFResult,
    out_path: Path,
    title: str,
    ylabel: str,
    alpha: float = 0.10,
) -> None:
    z = 1.645  # one-sided approx; two-sided 90% uses ~1.645 for normal
    h = res.horizons
    c = res.coef
    band = z * res.se
    lo, hi = c - band, c + band
    plt.figure(figsize=(7, 4))
    plt.fill_between(h, lo, hi, color="0.7", alpha=0.5, label="90% CI")
    plt.plot(h, c, color="0.1", linewidth=2, label="Point estimate")
    plt.axhline(0, color="0.5", linewidth=1)
    plt.xlabel("Horizon (months)")
    plt.ylabel(ylabel)
    plt.title(title)
    plt.legend(loc="best")
    plt.tight_layout()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(out_path, dpi=150)
    plt.close()
