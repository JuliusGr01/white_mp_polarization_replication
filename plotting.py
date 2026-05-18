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
    h = np.r_[0, res.horizons]
    c = np.r_[0.0, res.coef]
    band = np.r_[0.0, z * res.se]
    lo, hi = c - band, c + band
    plt.figure(figsize=(7, 4))
    plt.fill_between(h, lo, hi, color="0.85", alpha=1.0)
    plt.plot(h, c, color="0.0", linewidth=2)
    plt.axhline(0, color="0.3", linewidth=0.8, linestyle=":")
    plt.xlim(0, max(h))
    plt.xlabel("Months")
    plt.ylabel(ylabel)
    plt.title(title)
    plt.tight_layout()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(out_path, dpi=150)
    plt.close()


def plot_figure3(irfs: dict[str, IRFResult], out_path: Path) -> None:
    """Create a four-panel plot matching the layout of White's Figure 3."""
    z = 1.645
    panels = [
        ("log_routine", "Routine Employment", "Percent", (-3.0, 1.0)),
        ("log_nonroutine", "Nonroutine Employment", "Percent", (-3.0, 1.0)),
        ("routine_share", "Routine Share", "% Points", (-1.0, 0.5)),
        ("log_total", "Total Employment", "Percent", (-3.0, 1.0)),
    ]

    fig, axes = plt.subplots(2, 2, figsize=(8, 5.8), sharex=True)
    for ax, (key, title, ylabel, ylim) in zip(axes.flat, panels):
        res = irfs[key]
        h = np.r_[0, res.horizons]
        c = np.r_[0.0, res.coef]
        band = np.r_[0.0, z * res.se]
        ax.fill_between(h, c - band, c + band, color="0.88", alpha=1.0)
        ax.plot(h, c, color="0.0", linewidth=2)
        ax.axhline(0, color="0.3", linewidth=0.8, linestyle=":")
        ax.set_title(title)
        ax.set_ylabel(ylabel)
        ax.set_xlim(0, 48)
        ax.set_ylim(*ylim)
        ax.set_xticks([0, 12, 24, 36, 48])

    axes[1, 0].set_xlabel("Months")
    axes[1, 1].set_xlabel("Months")
    fig.tight_layout()
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=200)
    plt.close(fig)
