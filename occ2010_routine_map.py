"""
Map IPUMS/Census OCC2010 codes to routine vs nonroutine (approximation to Autor–Levy–Murnane).

White (2022) uses the task-based ALM classification. A full DOT-consistent map requires
Dorn/Autor crosswalk files. This module uses Census 2010 occupation *code ranges* that
align with the broad groups used in polarization work: routine cognitive (office/sales
clerical), routine manual (construction, production, transportation, installation).

Nonroutine manual (food prep, cleaning, personal care) and nonroutine cognitive
(professional/managerial) are excluded from routine.

Ranges follow the IPUMS USA OCC2010 code structure (see IPUMS OCC2010 codebook).
Adjust `OCC2010_ROUTINE_RANGES` if you vendor a precise occ2010→ALM file from replication data.
"""

from __future__ import annotations

# Inclusive (low, high) intervals where employment counts as ROUTINE for this replication code.
# Gaps default to nonroutine. Unemployed / NILF handled upstream.
OCC2010_ROUTINE_RANGES: tuple[tuple[int, int], ...] = (
    # Sales and related (clerical / retail sales; excludes supervisors in 0010–0960)
    (4700, 4965),
    # Office and administrative support
    (5000, 5940),
    # Farming/fishing/forestry (often treated as manual routine in polarization work)
    (6000, 6130),
    # Construction and extraction
    (6200, 6940),
    # Installation, maintenance, and repair
    (7000, 7630),
    # Production
    (7700, 8960),
    # Transportation and material moving
    (9000, 9750),
)


def occ2010_is_routine(occ: int) -> bool:
    if occ <= 0 or occ >= 9990:
        return False
    for lo, hi in OCC2010_ROUTINE_RANGES:
        if lo <= occ <= hi:
            return True
    return False


def build_occ2010_table() -> list[tuple[int, int]]:
    """Return rows (occ2010, is_routine_int) for all plausible codes."""
    rows: list[tuple[int, int]] = []
    for code in range(10, 9760):
        rows.append((code, 1 if occ2010_is_routine(code) else 0))
    return rows
