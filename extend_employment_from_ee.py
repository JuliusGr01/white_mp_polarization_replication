"""Extend the employment panel with FRASER Employment and Earnings PDFs.

The public BLS broad-occupation files in this repo begin in 1983. White's sample
starts earlier, so this script reconstructs a compatible 1967-1982 monthly panel
from the occupation tables printed in the historical Employment and Earnings
issues archived by FRASER.

The extractor is deliberately conservative:
  * every PDF is kept on disk;
  * every extracted text file is kept on disk;
  * the final panel is written only for months that pass table-level validation;
  * an audit CSV and manual-review snippets are written for low-confidence months.

Install notes:
  * Requires pandas and numpy from requirements.txt.
  * Uses Poppler's pdftotext. If tesseract and pdftoppm are installed, the optional
    OCR fallback can be used when embedded text is too poor.

The printed household tables are usually for the month before the issue date
(for example, the December 1982 issue reports November 1982). Output panel
dates therefore use the reference month, while the audit keeps the issue month.

Example:
    python extend_employment_from_ee.py
    python extend_employment_from_ee.py --start 1967-01 --end 1982-12 --merge-current
"""

from __future__ import annotations

import argparse
import calendar
import dataclasses
import math
import re
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from datetime import date
from pathlib import Path
from typing import Iterable
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

import numpy as np
import pandas as pd

from config import DATA_DIR

FRASER_EMPLOYMENT_BASE = "https://fraser.stlouisfed.org/files/docs/publications/employment"
DEFAULT_START = "1967-01"
DEFAULT_END = "1982-12"
USER_AGENT = "white-mp-polarization-replication/1.0"

PDF_DIR = DATA_DIR / "ee_pdfs"
TEXT_DIR = DATA_DIR / "ee_text"
OUT_DIR = DATA_DIR / "ee_extracted"
MANUAL_REVIEW_DIR = OUT_DIR / "manual_review"

HISTORICAL_PANEL_PATH = OUT_DIR / "employment_monthly_1967_1982.csv"
RAW_LONG_PATH = OUT_DIR / "ee_occupation_monthly_raw.csv"
RAW_WIDE_PATH = OUT_DIR / "ee_occupation_monthly_wide.csv"
AUDIT_PATH = OUT_DIR / "ee_extraction_audit.csv"
EXTENDED_PANEL_PATH = DATA_DIR / "employment_monthly_extended.csv"

PANEL_COLUMNS = [
    "date",
    "routine_emp",
    "nonroutine_emp",
    "total_emp",
    "routine_share",
    "log_total",
    "log_routine",
    "log_nonroutine",
]


@dataclass(frozen=True)
class CategorySpec:
    name: str
    label: str
    task_group: str | None
    patterns: tuple[str, ...]
    plausible_min: float = 200.0
    plausible_max: float = 30_000.0

    def compiled(self) -> tuple[re.Pattern[str], ...]:
        return tuple(re.compile(pattern, re.IGNORECASE) for pattern in self.patterns)


CATEGORIES: tuple[CategorySpec, ...] = (
    CategorySpec(
        "professional_technical",
        "Professional and technical",
        "nonroutine",
        (r"^Professional\s+and\s+technical\b",),
        6_000.0,
        30_000.0,
    ),
    CategorySpec(
        "white_collar_workers",
        "White-collar workers",
        None,
        (r"^White[-\s]?collar\s+workers?\b",),
        20_000.0,
        70_000.0,
    ),
    CategorySpec(
        "managers",
        "Managers, officials, proprietors / administrators",
        "nonroutine",
        (
            r"^Managers?,\s*officials?,\s*and\s*proprietors\b",
            r"^Managers?\s+officials?\s+and\s+proprietors\b",
            r"^Managers?\s+officials?\s+and\s+proprietor[s!]*",
            r"^Managers?\s+and\s+administrators?,\s*except\s+farm\b",
            r"^Managers?\s+and\s+administrators?\s+except\s+farm\b",
        ),
        4_000.0,
        30_000.0,
    ),
    CategorySpec(
        "sales",
        "Sales workers",
        "routine",
        (r"^Sales\s+workers?\b",),
        2_500.0,
        15_000.0,
    ),
    CategorySpec(
        "clerical",
        "Clerical workers",
        "routine",
        (r"^Clerical\s+workers?\b",),
        7_000.0,
        30_000.0,
    ),
    CategorySpec(
        "craft",
        "Craft/craftsmen workers",
        "routine",
        (
            r"^Craftsmen\s+and\s+foremen\b",
            r"^Craft\s+and\s+kindred\s+workers\b",
            r"^Craftsmen,\s*foremen,\s*and\s*kindred\s*workers\b",
        ),
        7_000.0,
        25_000.0,
    ),
    CategorySpec(
        "blue_collar_workers",
        "Blue-collar workers",
        None,
        (r"^Blue[-\s]?collar\s+workers?\b",),
        15_000.0,
        60_000.0,
    ),
    CategorySpec(
        "operatives_total",
        "Operatives",
        "routine",
        (r"^Operatives\b(?!\s*(?:,\s*)?except\s+transport)(?!\s+except\s+transport)",),
        7_000.0,
        25_000.0,
    ),
    CategorySpec(
        "operatives_except_transport",
        "Operatives, except transport",
        "routine",
        (r"^Operatives\s*,\s*except\s+transport\b", r"^Operatives\s+except\s+transport\b"),
        4_000.0,
        20_000.0,
    ),
    CategorySpec(
        "transport_equipment_operatives",
        "Transport equipment operatives",
        "routine",
        (r"^Transport\s+equipment\s+operatives\b",),
        1_500.0,
        10_000.0,
    ),
    CategorySpec(
        "nonfarm_laborers",
        "Nonfarm laborers",
        "routine",
        (r"^Non[-\s]?farm\s+laborers\b",),
        2_000.0,
        10_000.0,
    ),
    CategorySpec(
        "service_workers",
        "Service workers",
        "nonroutine",
        (r"^Service\s+workers\b(?!\s*,\s*except)(?!\s+except)",),
        6_000.0,
        25_000.0,
    ),
    CategorySpec(
        "farm_workers",
        "Farm workers",
        None,
        (
            r"^Farm\s+workers\b",
            r"^Farmers\s+and\s+farm\s+laborers\b",
        ),
        1_500.0,
        10_000.0,
    ),
)

CATEGORY_BY_NAME = {category.name: category for category in CATEGORIES}
COMPILED_CATEGORY_PATTERNS = {
    category.name: category.compiled()
    for category in CATEGORIES
}

NUMERIC_CHAR_TRANSLATION = str.maketrans(
    {
        "O": "0",
        "o": "0",
        "Q": "0",
        "D": "0",
        "I": "1",
        "l": "1",
        "|": "1",
        "!": "1",
        "B": "8",
        "S": "5",
        "Z": "2",
        "k": "4",
        "K": "4",
    }
)

# The token allows spaces inside comma-grouped numbers, which is common in the
# OCR layer of these PDFs, without joining adjacent columns.
NUMBER_RE = re.compile(
    r"[-+]?(?:"
    r"\d(?:[ \t]?\d){0,2}\s*,\s*(?:\d\s*){3}"
    r"|\d{4,6}"
    r"|\d{1,3}"
    r")(?:\.\s*\d+)?"
)

YEAR_TOKEN_RE = re.compile(r"\b(?:19)?[0-9SOBIl|]{2}\b")


@dataclass
class ExtractedValue:
    category: str
    value_thousands: float
    line_number: int
    line: str
    pattern: str


@dataclass
class CandidateParse:
    page_index: int
    block_start_line: int
    block_end_line: int
    block: str
    values: dict[str, ExtractedValue]
    score: float
    warnings: list[str]


@dataclass
class MonthResult:
    year: int
    month: int
    pdf_path: Path
    text_path: Path
    source_url: str
    status: str
    routine_emp: float | None = None
    nonroutine_emp: float | None = None
    total_emp: float | None = None
    routine_share: float | None = None
    candidate: CandidateParse | None = None
    extraction_method: str = "pdftotext"
    warnings: list[str] = dataclasses.field(default_factory=list)
    error: str | None = None

    @property
    def ym(self) -> str:
        return f"{self.year:04d}-{self.month:02d}"


def parse_ym(value: str) -> tuple[int, int]:
    match = re.fullmatch(r"(\d{4})-(\d{1,2})", value.strip())
    if not match:
        raise argparse.ArgumentTypeError(f"Expected YYYY-MM, got {value!r}")
    year = int(match.group(1))
    month = int(match.group(2))
    if month < 1 or month > 12:
        raise argparse.ArgumentTypeError(f"Invalid month in {value!r}")
    return year, month


def iter_months(start: tuple[int, int], end: tuple[int, int]) -> Iterable[tuple[int, int]]:
    start_year, start_month = start
    end_year, end_month = end
    if (start_year, start_month) > (end_year, end_month):
        raise ValueError("start must be before or equal to end")

    year, month = start_year, start_month
    while (year, month) <= (end_year, end_month):
        yield year, month
        month += 1
        if month == 13:
            month = 1
            year += 1


def fraser_pdf_url(year: int, month: int) -> str:
    decade = f"{(year // 10) * 10}s"
    return f"{FRASER_EMPLOYMENT_BASE}/{decade}/empl_{month:02d}{year}.pdf"


def fraser_pdf_urls(year: int, month: int) -> list[str]:
    decade = f"{(year // 10) * 10}s"
    stem = f"empl_{month:02d}{year}.pdf"
    urls = []
    if 1970 <= year <= 1974:
        urls.append(f"{FRASER_EMPLOYMENT_BASE}/{decade}/1970-1974/{stem}")
    elif 1975 <= year <= 1979:
        urls.append(f"{FRASER_EMPLOYMENT_BASE}/{decade}/1975-1979/{stem}")
    urls.append(fraser_pdf_url(year, month))
    return urls


def month_stem(year: int, month: int) -> str:
    return f"empl_{month:02d}{year}"


def reference_year_month(issue_year: int, issue_month: int) -> tuple[int, int]:
    if issue_month == 1:
        return issue_year - 1, 12
    return issue_year, issue_month - 1


def download_pdf(year: int, month: int, out_dir: Path, force: bool = False, retries: int = 3) -> tuple[Path, str]:
    out_dir.mkdir(parents=True, exist_ok=True)
    path = out_dir / f"{month_stem(year, month)}.pdf"
    if path.exists() and path.stat().st_size > 10_000 and not force:
        return path, fraser_pdf_urls(year, month)[0]

    tmp_path = path.with_suffix(".pdf.tmp")
    last_error: Exception | None = None
    for url in fraser_pdf_urls(year, month):
        request = Request(url, headers={"User-Agent": USER_AGENT})
        for attempt in range(1, retries + 1):
            try:
                with urlopen(request, timeout=60) as response, tmp_path.open("wb") as fh:
                    shutil.copyfileobj(response, fh)
                if tmp_path.stat().st_size < 10_000:
                    raise RuntimeError(f"Downloaded file is suspiciously small: {tmp_path.stat().st_size} bytes")
                tmp_path.replace(path)
                return path, url
            except (HTTPError, URLError, TimeoutError, RuntimeError) as exc:
                last_error = exc
                if tmp_path.exists():
                    tmp_path.unlink()
                if isinstance(exc, HTTPError) and exc.code in {403, 404}:
                    break
                if attempt < retries:
                    time.sleep(1.5 * attempt)

    urls = ", ".join(fraser_pdf_urls(year, month))
    raise RuntimeError(f"Failed to download any candidate URL ({urls}): {last_error}") from last_error


def command_path(name: str) -> str | None:
    return shutil.which(name)


def run_checked(command: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        check=True,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def extract_text_pdftotext(pdf_path: Path, text_path: Path, force: bool = False) -> Path:
    text_path.parent.mkdir(parents=True, exist_ok=True)
    if text_path.exists() and text_path.stat().st_size > 5_000 and not force:
        return text_path

    pdftotext = command_path("pdftotext")
    if not pdftotext:
        raise RuntimeError("pdftotext was not found. Install Poppler or TeX Live's Poppler utilities.")

    tmp_path = text_path.with_suffix(".txt.tmp")
    command = [
        pdftotext,
        "-layout",
        "-enc",
        "UTF-8",
        str(pdf_path),
        str(tmp_path),
    ]
    try:
        run_checked(command)
        tmp_path.replace(text_path)
    finally:
        if tmp_path.exists():
            tmp_path.unlink()
    return text_path


def extract_text_tesseract(pdf_path: Path, text_path: Path, force: bool = False, dpi: int = 350) -> Path:
    """OCR fallback for image-only PDFs.

    This is intentionally optional because FRASER's embedded OCR is usually
    better aligned for the tables than a fresh OCR pass. It is useful for months
    whose embedded text layer is missing or badly damaged.
    """
    text_path.parent.mkdir(parents=True, exist_ok=True)
    if text_path.exists() and text_path.stat().st_size > 5_000 and not force:
        return text_path

    pdftoppm = command_path("pdftoppm")
    tesseract = command_path("tesseract")
    if not pdftoppm or not tesseract:
        raise RuntimeError("OCR fallback requires both pdftoppm and tesseract on PATH.")

    with tempfile.TemporaryDirectory(prefix="ee_ocr_") as tmp:
        tmp_dir = Path(tmp)
        image_prefix = tmp_dir / "page"
        run_checked([pdftoppm, "-r", str(dpi), "-png", str(pdf_path), str(image_prefix)])
        page_texts: list[str] = []
        for image_path in sorted(tmp_dir.glob("page-*.png")):
            proc = run_checked([tesseract, str(image_path), "stdout", "-l", "eng", "--psm", "6"])
            page_texts.append(proc.stdout)
        text_path.write_text("\f".join(page_texts), encoding="utf-8", newline="\n")
    return text_path


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def light_normalize(text: str) -> str:
    text = text.replace("\u00a0", " ")
    text = text.replace("\u2010", "-").replace("\u2011", "-").replace("\u2013", "-")
    text = text.replace("\u2022", " ")
    text = text.replace("•", " ")
    return text


def normalize_for_matching(line: str) -> str:
    line = light_normalize(line)
    line = re.sub(r"[.·,*_~<>]+", " ", line)
    line = re.sub(r"\s+", " ", line)
    return line.strip()


def repair_numeric_text(value: str) -> str:
    value = re.sub(r"(?<!\d)[1Il]\*(?=\s*,\s*\d{3})", "4", value)
    value = re.sub(r"(?<=\d)\*(?=\d)", "4", value)
    value = re.sub(r"(?<=,)\*(?=\d)", "4", value)
    value = value.translate(NUMERIC_CHAR_TRANSLATION)
    value = re.sub(r"(?<=\d)\s*,\s*(?=\d)", ",", value)
    return value


def numeric_values(value: str) -> list[float]:
    cleaned = repair_numeric_text(value)
    out: list[float] = []
    for match in NUMBER_RE.finditer(cleaned):
        token = re.sub(r"\s+", "", match.group(0))
        token = token.replace(",", "")
        if token in {"", "-", "+", "."}:
            continue
        try:
            number = float(token)
        except ValueError:
            continue
        out.append(number)
    return out


def parse_year_token(token: str) -> int | None:
    cleaned = token.translate(
        str.maketrans(
            {
                "S": "9",
                "O": "0",
                "B": "8",
                "I": "1",
                "l": "1",
                "|": "1",
            }
        )
    )
    if not cleaned.isdigit():
        return None
    year = int(cleaned)
    if year < 100:
        year += 1900
    if 1900 <= year <= 1999:
        return year
    return None


def infer_current_value_index(block: str, reference_year: int | None) -> int:
    if reference_year is None:
        return 0

    header = "\n".join(block.splitlines()[:45])
    years: list[int] = []
    for match in YEAR_TOKEN_RE.finditer(header):
        parsed = parse_year_token(match.group(0))
        if parsed is not None:
            years.append(parsed)

    wanted = [year for year in years if year in {reference_year, reference_year - 1}]
    for idx, year in enumerate(wanted[:4]):
        if year == reference_year:
            return idx
    return 0


def is_major_category_line(line: str) -> bool:
    normalized = normalize_for_matching(line)
    return any(
        pattern.search(normalized)
        for category in CATEGORIES
        for pattern in COMPILED_CATEGORY_PATTERNS[category.name]
    )


def label_hit_count(text: str) -> int:
    count = 0
    for category in CATEGORIES:
        if category.name == "farm_workers":
            continue
        found = False
        for line in text.splitlines():
            normalized = normalize_for_matching(line)
            if any(pattern.search(normalized) for pattern in COMPILED_CATEGORY_PATTERNS[category.name]):
                found = True
                break
        if found:
            count += 1
    return count


def split_pages(text: str) -> list[tuple[int, int, int, str]]:
    pages = text.split("\f")
    result: list[tuple[int, int, int, str]] = []
    line_cursor = 1
    for page_index, page in enumerate(pages):
        lines = page.splitlines()
        start_line = line_cursor
        end_line = line_cursor + max(len(lines) - 1, 0)
        result.append((page_index, start_line, end_line, page))
        line_cursor = end_line + 1
    return result


def candidate_pages(text: str) -> list[tuple[int, int, int, str]]:
    pages = split_pages(text)
    candidates: list[tuple[int, int, int, str]] = []

    for page_index, start_line, end_line, page in pages:
        normalized_page = normalize_for_matching(page)
        hits = label_hit_count(page)
        has_occupation = bool(re.search(r"\bOCCUPATION\b", normalized_page, re.IGNORECASE))
        has_employed_occupation = bool(
            re.search(r"\bEmployed\s+persons\s+by\s+(?:major\s+)?occupation\b", normalized_page, re.IGNORECASE)
        )
        is_percent_distribution = bool(
            re.search(r"\bPercent\s+distribution\b", normalized_page, re.IGNORECASE)
        )
        if has_employed_occupation and hits >= 3 and not is_percent_distribution:
            candidates.append((page_index, start_line, end_line, page))
        elif hits >= 5 and (has_occupation or has_employed_occupation):
            candidates.append((page_index, start_line, end_line, page))

    if candidates:
        return candidates

    # Fallback: broad windows around an occupation heading. This catches pages
    # where OCR mangles the printed title but leaves the table body intact.
    lines = text.splitlines()
    for idx, line in enumerate(lines):
        normalized = normalize_for_matching(line)
        if re.search(r"\bOCCUPATION\b", normalized, re.IGNORECASE):
            start = max(0, idx - 25)
            end = min(len(lines), idx + 75)
            block = "\n".join(lines[start:end])
            if label_hit_count(block) >= 5:
                candidates.append((0, start + 1, end, block))

    return candidates


def find_category_value(
    category: CategorySpec,
    block: str,
    block_start_line: int,
    value_index: int,
) -> ExtractedValue | None:
    lines = block.splitlines()
    for idx, raw_line in enumerate(lines):
        normalized_line = normalize_for_matching(raw_line)
        for pattern in COMPILED_CATEGORY_PATTERNS[category.name]:
            match = pattern.search(normalized_line)
            if not match:
                continue

            # Use the raw row plus a short continuation. Several older FRASER
            # text layers place the numeric cells a few lines after the label.
            suffix_lines = [raw_line]
            for next_idx in range(idx + 1, min(idx + 8, len(lines))):
                if is_major_category_line(lines[next_idx]):
                    break
                suffix_lines.append(lines[next_idx])
            suffix = " ".join(suffix_lines)

            values = numeric_values(suffix)
            values = [value for value in values if value >= category.plausible_min]
            if not values:
                continue
            value = values[value_index] if value_index < len(values) else values[0]
            return ExtractedValue(
                category=category.name,
                value_thousands=value,
                line_number=block_start_line + idx,
                line=raw_line.strip(),
                pattern=pattern.pattern,
            )
    return None


def first_line_matching(
    lines: list[str],
    pattern: str,
    start: int = 0,
    end: int | None = None,
) -> int | None:
    end = len(lines) if end is None else min(end, len(lines))
    compiled = re.compile(pattern, re.IGNORECASE)
    for idx in range(max(start, 0), end):
        if compiled.search(normalize_for_matching(lines[idx])):
            return idx
    return None


def looks_like_unlabeled_numeric_row(line: str) -> bool:
    if not numeric_values(line):
        return False
    words = re.findall(r"[A-Za-z]+", normalize_for_matching(line))
    return not words or all(len(word) == 1 and word.lower() in {"f", "i", "l"} for word in words)


def inferred_value_from_line(
    category: CategorySpec,
    line: str,
    block_start_line: int,
    line_index: int,
    value_index: int,
    pattern: str,
) -> ExtractedValue | None:
    values = [
        value
        for value in numeric_values(line)
        if category.plausible_min <= value <= category.plausible_max
    ]
    if not values:
        return None
    value = values[value_index] if value_index < len(values) else values[0]
    return ExtractedValue(
        category=category.name,
        value_thousands=value,
        line_number=block_start_line + line_index,
        line=line.strip(),
        pattern=pattern,
    )


def parent_value_before(
    category_name: str,
    lines: list[str],
    block_start_line: int,
    start: int,
    end: int,
    value_index: int,
    pattern: str,
) -> ExtractedValue | None:
    category = CATEGORY_BY_NAME[category_name]
    for idx in range(min(end, len(lines)) - 1, max(start, 0) - 1, -1):
        line = lines[idx]
        if not looks_like_unlabeled_numeric_row(line):
            continue
        extracted = inferred_value_from_line(
            category=category,
            line=line,
            block_start_line=block_start_line,
            line_index=idx,
            value_index=value_index,
            pattern=pattern,
        )
        if extracted:
            return extracted
    return None


def infer_missing_parent_rows(
    values: dict[str, ExtractedValue],
    block: str,
    block_start_line: int,
    value_index: int,
) -> dict[str, ExtractedValue]:
    """Recover parent occupation rows when the scan drops the row label.

    Several 1960s FRASER text layers keep the numeric cells for the broad
    household occupation rows but lose a few labels in the far-left margin. The
    published table order is stable, so we infer only parent rows that are
    bracketed by clearly identified subrows.
    """
    normalized_block = normalize_for_matching(block)
    if not re.search(r"\bEmployed\s+persons\s+by\s+(?:major\s+)?occupation\b", normalized_block, re.IGNORECASE):
        return values
    if re.search(r"\bPercent\s+distribution\b", normalized_block, re.IGNORECASE):
        return values
    if re.search(r"\bseasonally\s+adjusted\b", normalized_block, re.IGNORECASE):
        return values

    inferred = dict(values)
    lines = block.splitlines()

    manager_idx = (
        values["managers"].line_number - block_start_line
        if "managers" in values
        else first_line_matching(lines, r"\bManagers?\b")
    )
    clerical_sub_idx = first_line_matching(
        lines,
        r"\bStenographers\b|\btypists\b|\bsecretaries\b|\bOther\s+clerical\s+workers\b",
        start=(manager_idx + 1 if manager_idx is not None else 0),
    )
    if "clerical" not in inferred and manager_idx is not None and clerical_sub_idx is not None:
        extracted = parent_value_before(
            "clerical",
            lines,
            block_start_line,
            manager_idx + 1,
            clerical_sub_idx,
            value_index,
            "inferred parent row before clerical subrows",
        )
        if extracted:
            inferred["clerical"] = extracted

    sales_sub_idx = first_line_matching(
        lines,
        r"\bRetail\s+trade\b|\bOther\s+sales\s+workers\b",
        start=(clerical_sub_idx + 1 if clerical_sub_idx is not None else 0),
    )
    sales_start = clerical_sub_idx + 1 if clerical_sub_idx is not None else (manager_idx + 1 if manager_idx is not None else 0)
    if "sales" not in inferred and sales_sub_idx is not None:
        extracted = parent_value_before(
            "sales",
            lines,
            block_start_line,
            sales_start,
            sales_sub_idx,
            value_index,
            "inferred parent row before sales subrows",
        )
        if extracted:
            inferred["sales"] = extracted

    blue_idx = first_line_matching(lines, r"\bBlue[-\s]collar\s+workers\b")
    craft_sub_idx = first_line_matching(
        lines,
        r"\bCarpenters\b|\bConstruction\s+craftsmen\b|\bMechanics\s+and\s+repairmen\b|"
        r"\bMetal\s+craftsmen\b|\bOther\s+craftsmen\b|\bForemen\b",
        start=(blue_idx + 1 if blue_idx is not None else 0),
    )
    if "craft" not in inferred and blue_idx is not None and craft_sub_idx is not None:
        extracted = parent_value_before(
            "craft",
            lines,
            block_start_line,
            blue_idx + 1,
            craft_sub_idx,
            value_index,
            "inferred parent row before craft subrows",
        )
        if extracted:
            inferred["craft"] = extracted

    nonfarm_idx = (
        values["nonfarm_laborers"].line_number - block_start_line
        if "nonfarm_laborers" in values
        else first_line_matching(lines, r"\bNon[-\s]?farm\s+laborers\b")
    )
    service_except_idx = first_line_matching(
        lines,
        r"\bService\s+workers\s+except\s+private\s+household\b|"
        r"\bService\s+workers\s+except\b|"
        r"\bService\s+workers\s*,\s*except\b",
        start=(nonfarm_idx + 1 if nonfarm_idx is not None else 0),
    )
    if nonfarm_idx is not None and service_except_idx is not None:
        extracted = parent_value_before(
            "service_workers",
            lines,
            block_start_line,
            nonfarm_idx + 1,
            service_except_idx,
            value_index,
            "inferred parent row before service subrows",
        )
        existing = inferred.get("service_workers")
        if extracted and (
            existing is None
            or abs(existing.value_thousands - extracted.value_thousands)
            / max(extracted.value_thousands, 1.0)
            > 0.15
        ):
            inferred["service_workers"] = extracted

    return inferred


def parse_total_line(
    block: str,
    value_index: int = 0,
    before_line: int | None = None,
) -> tuple[float | None, int | None, str | None]:
    lines = block.splitlines()
    if before_line is not None:
        start = max(0, before_line - 30)
        search_order = range(min(before_line, len(lines)) - 1, start - 1, -1)
    else:
        search_order = range(len(lines))

    for idx in search_order:
        line = lines[idx]
        normalized = normalize_for_matching(line)
        if not re.match(r"^Total(?:\s+16\s+years\s+and\s+over)?\b", normalized, re.IGNORECASE):
            continue
        values = [value for value in numeric_values(line) if value >= 20_000]
        if values:
            value = values[value_index] if value_index < len(values) else values[0]
            return value, idx, line.strip()

    if before_line is not None:
        return parse_total_line(block, value_index, before_line=None)
    return None, None, None


def score_candidate(page_index: int, block: str, values: dict[str, ExtractedValue]) -> tuple[float, list[str]]:
    warnings: list[str] = []
    score = 0.0
    normalized_block = normalize_for_matching(block)

    score += len(values) * 10.0
    if re.search(r"\bOCCUPATION\b", normalized_block, re.IGNORECASE):
        score += 15.0
    if re.search(r"\bEmployed\s+persons\s+by\s+occupation\b", normalized_block, re.IGNORECASE):
        score += 15.0
    if re.search(r"\bEmployed\s+persons\s+by\s+(?:major\s+)?occupation\b", normalized_block, re.IGNORECASE):
        score += 15.0
    if re.search(r"\bWhite[-\s]collar\s+workers\b", normalized_block, re.IGNORECASE):
        score += 5.0
    if re.search(r"\bBlue[-\s]collar\s+workers\b", normalized_block, re.IGNORECASE):
        score += 5.0
    if re.search(r"\bPercent\s+distribution\b", normalized_block, re.IGNORECASE):
        score -= 80.0
        warnings.append("block is a percent-distribution table rather than levels")
    if re.search(r"\bUnemployed\s+persons\s+by\s+occupation\b", normalized_block, re.IGNORECASE):
        score -= 60.0
        warnings.append("block looks like an unemployed-persons table")
    if re.search(r"\bseasonally\s+adjusted\b", normalized_block, re.IGNORECASE):
        score -= 30.0
        warnings.append("block is seasonally adjusted rather than the printed current-month level table")
    if re.search(
        r"\bPersons\s+at\s+work\b|\bAverage\s+hours\b|\bpart\s*time\b|\bfull\s*-\s*or\s+part\s*-\s*time\b",
        normalized_block,
        re.IGNORECASE,
    ):
        score -= 45.0
        warnings.append("block may be a persons-at-work or hours table")

    # Earlier pages get a mild preference because annual/historical tables later
    # in the issue can repeat the same labels.
    score -= min(page_index, 50) * 0.25

    for name, extracted in values.items():
        spec = CATEGORY_BY_NAME[name]
        if extracted.value_thousands < spec.plausible_min or extracted.value_thousands > spec.plausible_max:
            score -= 20.0
            warnings.append(
                f"{name} value {extracted.value_thousands:g} is outside "
                f"{spec.plausible_min:g}-{spec.plausible_max:g}"
            )

    return score, warnings


def parse_candidates(text: str, reference_year: int | None = None) -> list[CandidateParse]:
    text = light_normalize(text)
    candidates: list[CandidateParse] = []
    for page_index, start_line, end_line, block in candidate_pages(text):
        value_index = infer_current_value_index(block, reference_year)
        values: dict[str, ExtractedValue] = {}
        for category in CATEGORIES:
            extracted = find_category_value(category, block, start_line, value_index)
            if extracted:
                values[category.name] = extracted
        values = infer_missing_parent_rows(values, block, start_line, value_index)

        score, warnings = score_candidate(page_index, block, values)
        candidates.append(
            CandidateParse(
                page_index=page_index,
                block_start_line=start_line,
                block_end_line=end_line,
                block=block,
                values=values,
                score=score,
                warnings=warnings,
            )
        )

    return sorted(candidates, key=lambda candidate: candidate.score, reverse=True)


def routine_components(values: dict[str, ExtractedValue]) -> tuple[dict[str, float], list[str]]:
    components: dict[str, float] = {}
    missing: list[str] = []
    for required in ("sales", "clerical"):
        if required in values:
            components[required] = values[required].value_thousands
        else:
            missing.append(required)

    if "blue_collar_workers" in values:
        components["blue_collar_workers"] = values["blue_collar_workers"].value_thousands
    else:
        for required in ("craft", "nonfarm_laborers"):
            if required in values:
                components[required] = values[required].value_thousands
            else:
                missing.append(required)

        if "operatives_total" in values:
            components["operatives_total"] = values["operatives_total"].value_thousands
        else:
            split_parts = ("operatives_except_transport", "transport_equipment_operatives")
            if all(part in values for part in split_parts):
                for part in split_parts:
                    components[part] = values[part].value_thousands
            else:
                missing.extend(part for part in split_parts if part not in values)

    return components, missing


def nonroutine_components(values: dict[str, ExtractedValue]) -> tuple[dict[str, float], list[str]]:
    components: dict[str, float] = {}
    missing: list[str] = []
    for required in ("professional_technical", "managers", "service_workers"):
        if required in values:
            components[required] = values[required].value_thousands
        else:
            missing.append(required)
    return components, missing


def build_month_result(
    year: int,
    month: int,
    pdf_path: Path,
    text_path: Path,
    source_url: str,
    extraction_method: str,
) -> MonthResult:
    text = read_text(text_path)
    warnings: list[str] = []

    if len(text) < 10_000:
        warnings.append(f"short extracted text ({len(text)} characters)")
    text_label_hits = label_hit_count(text)
    if text_label_hits < 5:
        warnings.append(
            f"low occupation-label hit count ({text_label_hits}); "
            "the PDF may need OCR (--ocr-fallback with Tesseract installed)"
        )

    ref_year, _ = reference_year_month(year, month)
    candidates = parse_candidates(text, ref_year)
    if not candidates:
        return MonthResult(
            year=year,
            month=month,
            pdf_path=pdf_path,
            text_path=text_path,
            source_url=source_url,
            status="failed",
            extraction_method=extraction_method,
            warnings=warnings,
            error="no occupation table candidate found",
        )

    best = candidates[0]
    routine, missing_routine = routine_components(best.values)
    nonroutine, missing_nonroutine = nonroutine_components(best.values)

    warnings.extend(best.warnings)
    missing = missing_routine + missing_nonroutine
    if missing:
        warnings.append(f"missing required categories: {', '.join(sorted(set(missing)))}")

    routine_thousands = sum(routine.values())
    nonroutine_thousands = sum(nonroutine.values())
    total_thousands = routine_thousands + nonroutine_thousands
    routine_share = routine_thousands / total_thousands if total_thousands else math.nan

    value_index = infer_current_value_index(best.block, ref_year)
    first_category_line = None
    if best.values:
        first_category_line = min(
            extracted.line_number - best.block_start_line
            for extracted in best.values.values()
        )
    published_total, _, _ = parse_total_line(best.block, value_index, first_category_line)
    if published_total is not None:
        if "farm_workers" in best.values:
            benchmark_total = published_total - best.values["farm_workers"].value_thousands
            coverage_label = "printed total net of farm workers"
            coverage = total_thousands / benchmark_total
            if coverage < 0.985 or coverage > 1.015:
                warnings.append(
                    f"routine+nonroutine coverage of {coverage_label} is {coverage:.3f}"
                )

    if not (35_000 <= total_thousands <= 130_000):
        warnings.append(f"routine+nonroutine total is implausible: {total_thousands:g} thousand")
    if not (0.40 <= routine_share <= 0.75):
        warnings.append(f"routine share is implausible: {routine_share:.3f}")

    if missing:
        status = "failed"
    elif warnings:
        status = "needs_review"
    else:
        status = "ok"

    return MonthResult(
        year=year,
        month=month,
        pdf_path=pdf_path,
        text_path=text_path,
        source_url=source_url,
        status=status,
        routine_emp=routine_thousands * 1000.0,
        nonroutine_emp=nonroutine_thousands * 1000.0,
        total_emp=total_thousands * 1000.0,
        routine_share=routine_share,
        candidate=best,
        extraction_method=extraction_method,
        warnings=warnings,
    )


def write_manual_review(result: MonthResult) -> None:
    MANUAL_REVIEW_DIR.mkdir(parents=True, exist_ok=True)
    path = MANUAL_REVIEW_DIR / f"{result.year:04d}_{result.month:02d}.txt"
    ref_year, ref_month = reference_year_month(result.year, result.month)
    lines = [
        f"Issue month: {result.ym}",
        f"Reference month: {ref_year:04d}-{ref_month:02d}",
        f"Status: {result.status}",
        f"Source: {result.source_url}",
        f"PDF: {result.pdf_path}",
        f"Text: {result.text_path}",
        f"Warnings: {'; '.join(result.warnings) if result.warnings else '(none)'}",
        f"Error: {result.error or '(none)'}",
        "",
    ]
    if result.candidate:
        lines.extend(
            [
                f"Best candidate page: {result.candidate.page_index + 1}",
                f"Candidate lines: {result.candidate.block_start_line}-{result.candidate.block_end_line}",
                f"Candidate score: {result.candidate.score:.1f}",
                "",
                "Extracted category values (thousands):",
            ]
        )
        for name in sorted(result.candidate.values):
            extracted = result.candidate.values[name]
            lines.append(
                f"  {name}: {extracted.value_thousands:g} "
                f"(line {extracted.line_number}: {extracted.line})"
            )
        lines.extend(["", "Candidate block:", result.candidate.block])
    path.write_text("\n".join(lines), encoding="utf-8", newline="\n")


def result_to_panel_row(result: MonthResult) -> dict[str, object] | None:
    if result.routine_emp is None or result.nonroutine_emp is None or result.total_emp is None:
        return None
    if result.status != "ok":
        return None
    ref_year, ref_month = reference_year_month(result.year, result.month)
    row = {
        "date": date(ref_year, ref_month, 1).isoformat(),
        "routine_emp": result.routine_emp,
        "nonroutine_emp": result.nonroutine_emp,
        "total_emp": result.total_emp,
        "routine_share": result.routine_share,
        "log_total": np.log(result.total_emp),
        "log_routine": np.log(result.routine_emp),
        "log_nonroutine": np.log(result.nonroutine_emp),
    }
    return row


def result_to_audit_row(result: MonthResult) -> dict[str, object]:
    ref_year, ref_month = reference_year_month(result.year, result.month)
    return {
        "date": date(ref_year, ref_month, 1).isoformat(),
        "issue_year": result.year,
        "issue_month": result.month,
        "status": result.status,
        "source_url": result.source_url,
        "pdf_path": str(result.pdf_path),
        "text_path": str(result.text_path),
        "extraction_method": result.extraction_method,
        "routine_emp": result.routine_emp,
        "nonroutine_emp": result.nonroutine_emp,
        "total_emp": result.total_emp,
        "routine_share": result.routine_share,
        "candidate_page": result.candidate.page_index + 1 if result.candidate else None,
        "candidate_score": result.candidate.score if result.candidate else None,
        "warnings": "; ".join(result.warnings),
        "error": result.error,
    }


def raw_long_rows(result: MonthResult) -> list[dict[str, object]]:
    if not result.candidate:
        return []
    ref_year, ref_month = reference_year_month(result.year, result.month)
    rows = []
    for name, extracted in sorted(result.candidate.values.items()):
        rows.append(
            {
                "date": date(ref_year, ref_month, 1).isoformat(),
                "issue_year": result.year,
                "issue_month": result.month,
                "category": name,
                "label": CATEGORY_BY_NAME[name].label,
                "task_group": CATEGORY_BY_NAME[name].task_group,
                "employed_thousands": extracted.value_thousands,
                "line_number": extracted.line_number,
                "source_line": extracted.line,
                "source_url": result.source_url,
            }
        )
    return rows


def write_outputs(results: list[MonthResult], merge_current: bool = False) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    audit = pd.DataFrame(result_to_audit_row(result) for result in results)
    audit.to_csv(AUDIT_PATH, index=False)

    raw_rows = [row for result in results for row in raw_long_rows(result)]
    raw = pd.DataFrame(raw_rows)
    raw.to_csv(RAW_LONG_PATH, index=False)
    if not raw.empty:
        wide = raw.pivot_table(
            index="date",
            columns="category",
            values="employed_thousands",
            aggfunc="first",
        ).reset_index()
        wide.to_csv(RAW_WIDE_PATH, index=False)
    else:
        pd.DataFrame(columns=["date"]).to_csv(RAW_WIDE_PATH, index=False)

    panel_rows = [row for result in results if (row := result_to_panel_row(result)) is not None]
    panel = pd.DataFrame(panel_rows, columns=PANEL_COLUMNS)
    panel.to_csv(HISTORICAL_PANEL_PATH, index=False)

    for result in results:
        if result.status != "ok":
            write_manual_review(result)

    if merge_current:
        write_extended_panel(panel)


def write_extended_panel(historical_panel: pd.DataFrame) -> pd.DataFrame:
    if historical_panel.empty:
        raise RuntimeError("Historical panel is empty; refusing to write merged panel.")

    try:
        from build_employment_panel import BLS_OCC_SOURCE, build_employment_panel

        current = build_employment_panel(BLS_OCC_SOURCE)
    except Exception:
        current_path = DATA_DIR / "employment_monthly.csv"
        if not current_path.exists():
            raise
        current = pd.read_csv(current_path, parse_dates=["date"])

    hist = historical_panel.copy()
    hist["date"] = pd.to_datetime(hist["date"])
    current = current[PANEL_COLUMNS].copy()
    current["date"] = pd.to_datetime(current["date"])
    merged = pd.concat([hist, current], ignore_index=True)
    merged = merged.drop_duplicates(subset=["date"], keep="last").sort_values("date")
    merged.to_csv(EXTENDED_PANEL_PATH, index=False)
    return merged


def low_quality_text(text_path: Path) -> bool:
    try:
        text = read_text(text_path)
    except FileNotFoundError:
        return True
    return len(text) < 10_000 or label_hit_count(text) < 5


def process_month(
    year: int,
    month: int,
    force_download: bool,
    force_extract: bool,
    extract_only: bool,
    download_only: bool,
    ocr_fallback: bool,
) -> MonthResult | None:
    pdf_path, source_url = download_pdf(year, month, PDF_DIR, force=force_download)
    text_path = TEXT_DIR / f"{month_stem(year, month)}.txt"
    if download_only:
        return None

    extraction_method = "pdftotext"
    extract_text_pdftotext(pdf_path, text_path, force=force_extract)
    if ocr_fallback and low_quality_text(text_path):
        ocr_path = TEXT_DIR / f"{month_stem(year, month)}_ocr.txt"
        extract_text_tesseract(pdf_path, ocr_path, force=force_extract)
        if not low_quality_text(ocr_path):
            text_path = ocr_path
            extraction_method = "tesseract"

    if extract_only:
        return None

    return build_month_result(year, month, pdf_path, text_path, source_url, extraction_method)


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Fetch and parse FRASER Employment and Earnings PDFs into a 1967-1982 employment panel.",
    )
    parser.add_argument("--start", type=parse_ym, default=parse_ym(DEFAULT_START), help="First month, YYYY-MM.")
    parser.add_argument("--end", type=parse_ym, default=parse_ym(DEFAULT_END), help="Last month, YYYY-MM.")
    parser.add_argument("--force-download", action="store_true", help="Re-download PDFs even if already present.")
    parser.add_argument("--force-extract", action="store_true", help="Re-run PDF text extraction even if text exists.")
    parser.add_argument("--download-only", action="store_true", help="Fetch PDFs and stop.")
    parser.add_argument("--extract-only", action="store_true", help="Fetch/extract text and stop before parsing.")
    parser.add_argument(
        "--ocr-fallback",
        action="store_true",
        help="If pdftotext is low-quality, use pdftoppm+tesseract when installed.",
    )
    parser.add_argument("--merge-current", action="store_true", help="Also write data/employment_monthly_extended.csv.")
    parser.add_argument("--strict", action="store_true", help="Exit nonzero when any month fails or needs review.")
    parser.add_argument("--max-months", type=int, default=None, help="Debug limit on number of months processed.")
    parser.add_argument("--sleep", type=float, default=0.15, help="Seconds to sleep between downloads.")
    return parser


def main() -> None:
    parser = build_arg_parser()
    args = parser.parse_args()

    if args.download_only and args.extract_only:
        raise SystemExit("Choose at most one of --download-only and --extract-only.")

    months = list(iter_months(args.start, args.end))
    if args.max_months is not None:
        months = months[: args.max_months]

    results: list[MonthResult] = []
    failures = 0
    review = 0
    for idx, (year, month) in enumerate(months, start=1):
        ym = f"{year:04d}-{month:02d}"
        print(f"[{idx:03d}/{len(months):03d}] {ym}", flush=True)
        try:
            result = process_month(
                year,
                month,
                force_download=args.force_download,
                force_extract=args.force_extract,
                extract_only=args.extract_only,
                download_only=args.download_only,
                ocr_fallback=args.ocr_fallback,
            )
        except Exception as exc:
            pdf_path = PDF_DIR / f"{month_stem(year, month)}.pdf"
            text_path = TEXT_DIR / f"{month_stem(year, month)}.txt"
            result = MonthResult(
                year=year,
                month=month,
                pdf_path=pdf_path,
                text_path=text_path,
                source_url=fraser_pdf_url(year, month),
                status="failed",
                error=str(exc),
            )
            print(f"  failed: {exc}", flush=True)

        if result is not None:
            results.append(result)
            if result.status == "failed":
                failures += 1
            elif result.status == "needs_review":
                review += 1
            detail = result.error or ", ".join(result.warnings) or "clean"
            print(f"  {result.status}: {detail}", flush=True)

        if idx < len(months) and args.sleep > 0:
            time.sleep(args.sleep)

    if not args.download_only and not args.extract_only:
        write_outputs(results, merge_current=args.merge_current)
        print(f"Wrote {HISTORICAL_PANEL_PATH}")
        print(f"Wrote {AUDIT_PATH}")
        print(f"Wrote {RAW_LONG_PATH}")
        if args.merge_current:
            print(f"Wrote {EXTENDED_PANEL_PATH}")

    if args.strict and (failures or review):
        raise SystemExit(f"{failures} failed month(s), {review} month(s) need review.")


if __name__ == "__main__":
    main()
