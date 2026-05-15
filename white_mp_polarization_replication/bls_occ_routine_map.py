"""
Map BLS CPS *major* occupation groups (Table A-13 / ln database titles) to routine vs nonroutine.

White (2022) uses Autor et al. (2003) task categories on *detailed* occupations. BLS published
monthly aggregates are coarser (roughly 7–11 groups depending on era). We assign each group
a routine employment *share* (0–1) based on Autor–Dorn–style task content, then allocate
group employment to routine = share * employed, nonroutine = (1-share) * employed.

Shares are approximate; replace with a detailed occ1990/occ2010 crosswalk if you use microdata.
"""

from __future__ import annotations

import re
from typing import Optional

# Substrings matched case-insensitively against ln series_title (longest match wins).
# Shares: fraction of group employment treated as ROUTINE (ALM).
MAJOR_OCC_ROUTINE_SHARE: tuple[tuple[str, float], ...] = (
    ("employment level - management, professional, and related", 0.12),
    ("employment level - service occupations", 0.38),
    ("employment level - sales and office occupations", 0.82),
    ("employment level - natural resources, construction, and maintenance", 0.88),
    ("employment level - production, transportation and material moving", 0.92),
    ("production, transportation, and material moving", 0.92),
    ("production, transportation, and material moving occupations", 0.92),
    ("natural resources, construction, and maintenance", 0.88),
    ("natural resources, construction, and maintenance occupations", 0.88),
    ("farming, fishing, and forestry", 0.85),
    ("farming, fishing, and forestry occupations", 0.85),
    ("sales and office", 0.82),
    ("sales and office occupations", 0.82),
    ("sales and related", 0.78),
    ("office and administrative support", 0.90),
    ("construction and extraction", 0.88),
    ("installation, maintenance, and repair", 0.85),
    ("production occupations", 0.92),
    ("transportation and material moving", 0.90),
    ("service occupations", 0.38),
    ("service", 0.38),
    ("management, professional, and related", 0.12),
    ("management, business, and financial operations", 0.10),
    ("professional and related", 0.08),
    ("management, professional, and related occupations", 0.12),
)


def routine_share_from_series_title(title: str) -> Optional[float]:
    t = re.sub(r"\s+", " ", title.lower().strip())
    if "unemploy" in t:
        return None
    if "employ" not in t:
        return None
    if "occupation" not in t and "occupations" not in t:
        return None
    best: Optional[float] = None
    best_len = -1
    for key, share in MAJOR_OCC_ROUTINE_SHARE:
        if key in t and len(key) > best_len:
            best = share
            best_len = len(key)
    return best
