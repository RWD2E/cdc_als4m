"""
Download SEER U.S. county population estimates and build a per-year age x sex
population table as a local file. No database dependency.

Source
------
NCI SEER Population Data (https://seer.cancer.gov/popdata/), the bridged-race July 1
county population estimates produced by the U.S. Census Bureau Population Estimates
Program under interagency agreement with NCI. This is the conventional denominator
source for U.S. disease-rate estimation and is what SEER*Stat itself uses.

Why the SINGLE-YEAR-OF-AGE file and not the pre-grouped one
-----------------------------------------------------------
1. Band alignment. The grouped files impose SEER's own bands, and those move: the
   April 2026 release switched from 19 age groups (top band 85+) to 20 (85-89, 90+).
   Emitting single years lets the consumer band them with its own cutpoints.
2. The grouped files encode age as an ORDINAL, not an age: code 02 means "5-9 years",
   not "age 2". Parsing one as the other yields a silently wrong table.

Vintage
-------
Census REVISES all post-censal years with each annual vintage, so the same URL
legitimately returns different numbers over time and a re-download changes history.
Every run writes a `.manifest.json` sidecar recording the URL, SHA-256 and retrieval
time. That hash is what makes a downstream analysis reproducible -- keep it with the
results.

Coverage
--------
SEER lags Census by 1-2 years; the most recent calendar years are simply not
published yet. `coverage_note()` reports what is missing for a requested range.

Outputs
-------
    <dest_dir>/<name>.txt.gz      raw download, cached (re-runs reuse it)
    <out>                         parsed table: YEAR, SEX, AGE, POPULATION,
                                  AGE_IS_OPEN_ENDED   (csv or parquet)
    <out>.manifest.json           provenance for the run

CLI
---
    python seer_population.py --discover
    python seer_population.py --start-year 2011 --end-year 2025 --out std_pop.csv
    python seer_population.py --url '<paste .txt.gz link>' --out std_pop.csv
"""

from __future__ import annotations

import argparse
import hashlib
import json
import logging
import re
import shutil
import urllib.request
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Sequence
from urllib.parse import urljoin

import pandas as pd

log = logging.getLogger(__name__)

SEER_DOWNLOAD_PAGE = "https://seer.cancer.gov/popdata/download.html"
SEER_DATA_DICTIONARY = "https://seer.cancer.gov/popdata/popdic.html"


# --------------------------------------------------------------------------- #
# Source registry
# --------------------------------------------------------------------------- #
@dataclass(frozen=True)
class SeerSource:
    """A SEER population file."""
    name: str
    url: str
    first_year: int
    last_year: int
    top_age: int = 0       # 0 = infer the open-ended band from the data (preferred)
    race_scheme: str = "expanded"   # "wbo" (race 1-3, origin=9) | "expanded" (1-4 x 0/1)

    def covers(self, start: int, end: int) -> tuple[bool, list[int]]:
        missing = [y for y in range(start, end + 1)
                   if not self.first_year <= y <= self.last_year]
        return (not missing), missing


# Fallback registry ONLY -- best-effort guesses at the naming convention that WILL go
# stale. SEER renames the release directory annually (yr1969_2020.19ages ->
# yr1990_2024.20ages -> ...) and retires the old one on a published schedule, so any
# URL constant here has a shelf life of months. `discover_sources()` reads the live
# download page and is the PRIMARY path; download_seer_population() falls back to it
# automatically when one of these 404s.
SOURCES: dict[str, SeerSource] = {
    "us.1990_2024.singleages": SeerSource(
        name="us.1990_2024.singleages",
        url="https://seer.cancer.gov/popdata/yr1990_2024.singleages.through89.90plus/us.1990_2024.singleages.through89.90plus.adjusted.txt.gz",
        first_year=1990, last_year=2024, race_scheme="expanded",
    ),
    "us.1990_2022.singleages": SeerSource(
        name="us.1990_2022.singleages",
        url="https://seer.cancer.gov/popdata/yr1990_2022.singleages/us.1990_2022.singleages.txt.gz",
        first_year=1990, last_year=2022, race_scheme="expanded",
    ),
    "us.1969_2023.singleages": SeerSource(
        name="us.1969_2023.singleages",
        url="https://seer.cancer.gov/popdata/yr1969_2023.singleages/us.1969_2023.singleages.txt.gz",
        first_year=1969, last_year=2023, race_scheme="wbo",
    ),
}

DEFAULT_SOURCE = "us.1990_2024.singleages"


# Fixed-width layout, per the SEER Population Data Dictionary. 0-indexed half-open
# spans for pandas; the dictionary states them 1-indexed inclusive.
#   1-4 year | 5-6 state | 7-8 state FIPS | 9-11 county FIPS | 12-13 registry
#   14 race  | 15 origin | 16 sex | 17-18 age | 19-26 population
COLSPECS = [(0, 4), (4, 6), (6, 8), (8, 11), (11, 13), (13, 14), (14, 15),
            (15, 16), (16, 18), (18, 26)]
COLNAMES = ["YEAR", "STATE", "STATE_FIPS", "COUNTY_FIPS", "REGISTRY",
            "RACE", "ORIGIN", "SEX", "AGE", "POPULATION"]
RECORD_LEN = 26

# SEER codes sex 1=Male, 2=Female.
SEER_SEX = {1: "M", 2: "F"}


# --------------------------------------------------------------------------- #
# URL discovery
# --------------------------------------------------------------------------- #
# Filenames follow {scope}.{first}_{last}.{layout}[.adjusted].txt.gz, where scope is
# "us" or a 2-letter state and layout is "singleages" or "<N>ages".
LINK_RE = re.compile(
    r'((?:us|[a-z]{2})\.(\d{4})_(\d{4})\.(singleages|\d{1,2}ages)(\.adjusted)?\.txt\.gz)',
    re.IGNORECASE,
)
HREF_RE = re.compile(r'href\s*=\s*["\']([^"\']+\.txt\.gz)["\']', re.IGNORECASE)


def discover_sources(
    page_url: str = SEER_DOWNLOAD_PAGE,
    layout: str = "singleages",
    scope: str = "us",
    adjusted: bool = False,
    timeout: int = 60,
) -> list[SeerSource]:
    """
    Scrape the SEER download page for the CURRENT file links.

    Hardcoding a URL is a losing game: SEER renames the release directory every year
    and retires the old one, so reading the page means tracking the rename instead of
    404ing on it.

    `adjusted` selects the Katrina/Rita-adjusted 2005 county variant. Irrelevant to a
    2011+ window, but it is a different vintage -- don't let it in by accident.

    Returns sources oldest-first, so `[-1]` is the most recent matching release.
    """
    req = urllib.request.Request(page_url, headers={"User-Agent": "python-urllib"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            html = r.read().decode("utf-8", errors="replace")
    except Exception as e:
        raise RuntimeError(f"Could not read {page_url} to discover files: {e}") from e

    found: dict[str, SeerSource] = {}
    for href in HREF_RE.findall(html):
        m = LINK_RE.search(href)
        if not m:
            continue
        full, first, last, lay, adj = m.groups()
        if lay.lower() != layout.lower():
            continue
        if not full.lower().startswith(scope.lower() + "."):
            continue
        if bool(adj) != adjusted:
            continue
        found[full] = SeerSource(
            name=full[: -len(".txt.gz")],
            url=urljoin(page_url, href),
            first_year=int(first),
            last_year=int(last),
            race_scheme="expanded" if int(first) >= 1990 else "wbo",
        )

    out = sorted(found.values(), key=lambda s: (s.last_year, s.first_year))
    if not out:
        raise RuntimeError(
            f"No {scope}.*.{layout}*.txt.gz links found on {page_url}. The page layout "
            f"may have changed; inspect it manually and pass --url explicitly."
        )
    log.info("discovered %d %s file(s)", len(out), layout)
    return out


def source_from_url(url: str) -> SeerSource:
    """Build a SeerSource from a pasted .txt.gz link, parsing the year range out."""
    m = LINK_RE.search(url)
    if not m:
        raise ValueError(
            f"Does not look like a SEER population file: {url}\n"
            f"Expected something like us.1990_2023.singleages.txt.gz"
        )
    full, first, last, _lay, _adj = m.groups()
    return SeerSource(
        name=full[: -len(".txt.gz")], url=url,
        first_year=int(first), last_year=int(last),
        race_scheme="expanded" if int(first) >= 1990 else "wbo",
    )


def coverage_note(source: SeerSource, start_year: int, end_year: int) -> str:
    ok, missing = source.covers(start_year, end_year)
    if ok:
        return f"{source.name} covers {start_year}-{end_year} in full."
    return (
        f"{source.name} covers {source.first_year}-{source.last_year}; "
        f"NO population data for {missing}. Census population estimates lag by "
        f"1-2 years, so recent years are simply not published yet. Years outside "
        f"the source range are silently absent from the output -- check before "
        f"using it as a per-year denominator."
    )


# --------------------------------------------------------------------------- #
# Download
# --------------------------------------------------------------------------- #
def download_seer_population(
    source: SeerSource | str = DEFAULT_SOURCE,
    dest_dir: str | Path = "./_seer_cache",
    force: bool = False,
    timeout: int = 300,
    autodiscover: bool = True,
) -> tuple[Path, str, SeerSource]:
    """
    Stream the gzip to disk and return (path, sha256, source_actually_used).

    Streamed rather than read into memory (the national single-age file is ~90 MB
    compressed, several GB expanded) and hashed to pin the vintage.

    On failure, retries once against the URL discovered from SEER's download page,
    since the registry defaults go stale as releases are renamed and retired. The
    returned SeerSource reflects what was actually fetched, which may differ from
    what was requested.
    """
    src = SOURCES[source] if isinstance(source, str) else source
    dest_dir = Path(dest_dir)
    dest_dir.mkdir(parents=True, exist_ok=True)
    path = dest_dir / f"{src.name}.txt.gz"

    if path.exists() and not force:
        log.info("using cached %s", path)
        return path, _sha256(path), src

    try:
        p, d = _fetch(src, path, timeout)
        return p, d, src
    except Exception as first_err:
        if not autodiscover:
            raise _download_error(src, first_err) from first_err

        log.warning("%s failed (%s); discovering current URL from %s",
                    src.url, first_err, SEER_DOWNLOAD_PAGE)
        try:
            found = discover_sources(layout="singleages", scope="us", adjusted=False)
        except Exception as disc_err:
            raise _download_error(src, first_err, disc_err) from first_err

        newest = found[-1]
        log.warning("retrying with discovered source %s -> %s", newest.name, newest.url)
        path = dest_dir / f"{newest.name}.txt.gz"
        if path.exists() and not force:
            return path, _sha256(path), newest
        try:
            p, d = _fetch(newest, path, timeout)
            return p, d, newest
        except Exception as e2:
            raise _download_error(newest, e2) from e2


def _fetch(src: SeerSource, path: Path, timeout: int) -> tuple[Path, str]:
    log.info("downloading %s -> %s", src.url, path)
    tmp = path.with_suffix(".part")
    try:
        req = urllib.request.Request(src.url, headers={"User-Agent": "python-urllib"})
        with urllib.request.urlopen(req, timeout=timeout) as r, open(tmp, "wb") as f:
            shutil.copyfileobj(r, f, length=1 << 20)
    except Exception:
        tmp.unlink(missing_ok=True)
        raise
    tmp.replace(path)
    digest = _sha256(path)
    log.info("downloaded %.1f MB, sha256=%s", path.stat().st_size / 1e6, digest[:16])
    return path, digest


def _download_error(src: SeerSource, err: Exception, disc_err: Exception | None = None):
    extra = f"\n  Auto-discovery also failed: {disc_err}" if disc_err else ""
    return RuntimeError(
        f"Failed to download {src.url}\n"
        f"  Original error: {err}{extra}\n"
        f"  SEER renames the release directory each year and retires the old one, so "
        f"the built-in defaults go stale.\n"
        f"  Run `python seer_population.py --discover` to list what SEER currently "
        f"publishes, or open {SEER_DOWNLOAD_PAGE} and pass the link with --url."
    )


def _sha256(path: Path, chunk: int = 1 << 20) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(chunk), b""):
            h.update(block)
    return h.hexdigest()


# --------------------------------------------------------------------------- #
# Parse
# --------------------------------------------------------------------------- #
def parse_seer_population(
    path: str | Path,
    source: SeerSource,
    start_year: int,
    end_year: int,
    chunksize: int = 2_000_000,
) -> pd.DataFrame:
    """
    Parse the fixed-width file and aggregate to national (YEAR, SEX, AGE, POPULATION).

    The input is ~15-40M county-level records; the output is ~n_years x 91 x 2 rows.
    Aggregation happens per chunk so peak memory stays flat and the file is never
    fully materialized.

    Summing over race and origin is correct and does NOT double count: within a single
    file every person appears once in exactly one (race, origin) cell -- the "wbo"
    scheme carries race 1-3 with origin held at 9, the "expanded" scheme cross-
    classifies race 1-4 by origin 0/1. Concatenating BOTH files WOULD double count,
    which is why only one source is read per call.
    """
    parts: list[pd.DataFrame] = []
    reader = pd.read_fwf(
        path,
        colspecs=COLSPECS,
        names=COLNAMES,
        dtype={"YEAR": "int16", "STATE": "string", "STATE_FIPS": "string",
               "COUNTY_FIPS": "string", "REGISTRY": "string", "RACE": "int8",
               "ORIGIN": "int8", "SEX": "int8", "AGE": "int8", "POPULATION": "int64"},
        compression="gzip",
        chunksize=chunksize,
    )
    for i, chunk in enumerate(reader):
        chunk = chunk[chunk["YEAR"].between(start_year, end_year)]
        if chunk.empty:
            continue
        g = chunk.groupby(["YEAR", "SEX", "AGE"], observed=True)["POPULATION"].sum()
        parts.append(g.reset_index())
        log.debug("chunk %d -> %d rows kept", i, len(chunk))

    if not parts:
        raise ValueError(
            f"No records for {start_year}-{end_year} in {path}. "
            f"{source.name} covers {source.first_year}-{source.last_year}."
        )

    df = (
        pd.concat(parts, ignore_index=True)
        .groupby(["YEAR", "SEX", "AGE"], as_index=False)["POPULATION"].sum()
    )
    df["SEX"] = df["SEX"].map(SEER_SEX)
    if df["SEX"].isna().any():
        raise ValueError("unmapped SEX code; expected 1=Male, 2=Female")

    # Infer the open-ended top band from the data rather than trusting a constant.
    # SEER moved the top code from 85 (86 single ages) to 90 (91 single ages) in the
    # April 2026 release; a hardcoded top_age silently mislabels the band across that
    # boundary. The highest code present IS the open-ended band, by construction.
    top_age = int(df["AGE"].max())
    if source.top_age and source.top_age != top_age:
        log.warning("declared top_age=%d but file tops out at %d; using %d",
                    source.top_age, top_age, top_age)
    log.info("open-ended top age band: %d+ (%d single-age codes)",
             top_age, df["AGE"].nunique())

    df["AGE_IS_OPEN_ENDED"] = df["AGE"] >= top_age
    df = df.sort_values(["YEAR", "SEX", "AGE"]).reset_index(drop=True)
    df["YEAR"] = df["YEAR"].astype("int32")
    df["AGE"] = df["AGE"].astype("int16")
    return df[["YEAR", "SEX", "AGE", "POPULATION", "AGE_IS_OPEN_ENDED"]]


# --------------------------------------------------------------------------- #
# Quality checks
# --------------------------------------------------------------------------- #
# Published U.S. resident population, July 1 (Census PEP national totals, millions).
# An order-of-magnitude tripwire only -- a vintage revision moves these by a few
# tenths of a percent, so the tolerance is deliberately loose.
US_TOTALS_M = {
    2011: 311.6, 2012: 313.9, 2013: 316.1, 2014: 318.4, 2015: 320.7,
    2016: 323.1, 2017: 325.1, 2018: 326.8, 2019: 328.3, 2020: 331.5,
    2021: 332.0, 2022: 333.3, 2023: 334.9, 2024: 337.0,
}


def qa_seer_population(df: pd.DataFrame, tolerance: float = 0.02) -> pd.DataFrame:
    """
    Sanity-check the parsed table against published national totals.

    Catches the failure modes that otherwise pass silently: a misaligned fixed-width
    span (population off by an order of magnitude), an accidental double count
    (~2x totals), or a partially-read file (totals too low).
    """
    known = [y for y in df["YEAR"].unique() if int(y) in US_TOTALS_M]
    if not known:
        log.warning("no overlap with published national totals; skipping QA")
        return pd.DataFrame()

    tot = df[df["YEAR"].isin(known)].groupby("YEAR")["POPULATION"].sum()
    tot = tot.rename("PARSED").to_frame()
    tot["EXPECTED"] = [US_TOTALS_M[int(y)] * 1e6 for y in tot.index]
    tot["PCT_DIFF"] = (tot["PARSED"] - tot["EXPECTED"]) / tot["EXPECTED"] * 100
    tot["OK"] = tot["PCT_DIFF"].abs() <= tolerance * 100

    bad = tot[tot["OK"].eq(False)]
    if len(bad):
        raise ValueError(
            f"SEER parse failed QA against published national totals:\n{bad}\n"
            f"A ~2x discrepancy suggests double counting across race/origin; an "
            f"order-of-magnitude one suggests a fixed-width span misalignment "
            f"(verify COLSPECS against {SEER_DATA_DICTIONARY})."
        )

    # Sex balance is a cheap second tripwire: a column-span slip usually destroys it.
    sex = df.groupby(["YEAR", "SEX"])["POPULATION"].sum().unstack()
    if {"M", "F"} <= set(sex.columns):
        ratio = (sex["M"] / sex["F"]).rename("M_F_RATIO")
        if not ratio.between(0.90, 1.05).all():
            raise ValueError(f"implausible male/female ratio by year:\n{ratio}")
        return tot.join(ratio)
    return tot


def validate_sex_domain(pop_df: pd.DataFrame, cohort_sex_values: Sequence) -> None:
    """
    Confirm this table's SEX domain intersects a cohort's.

    Worth calling before using this as a standardization weight set. SEX is a join
    key, and joins are usually inner: if the cohort codes sex as 1/2 (CMS SEX_CD)
    while this table carries 'M'/'F', the join matches nothing, every stratum drops,
    and the adjusted rate comes back empty -- with no error anywhere.
    """
    have = set(pop_df["SEX"].unique())
    want = set(cohort_sex_values)
    if not (have & want):
        raise ValueError(
            f"SEX domain mismatch: this table has {sorted(map(str, have))}, cohort "
            f"uses {sorted(map(str, want))}. A standardization join on SEX would "
            f"silently produce empty results. Recode one side (see --sex-labels)."
        )
    if have != want:
        log.warning("SEX domains overlap but differ: table=%s cohort=%s",
                    sorted(map(str, have)), sorted(map(str, want)))


# --------------------------------------------------------------------------- #
# Local output
# --------------------------------------------------------------------------- #
def write_local(
    df: pd.DataFrame,
    out_path: str | Path,
    source: SeerSource,
    digest: str,
    start_year: int,
    end_year: int,
    fmt: str | None = None,
) -> tuple[Path, Path]:
    """
    Write the parsed table plus a provenance sidecar. Returns (data_path, manifest_path).

    The sidecar is not bookkeeping for its own sake: Census revises every post-censal
    year with each vintage, so this exact URL will return different numbers next year.
    Without the hash, a re-run silently changes downstream results and nothing records
    which vintage produced what.
    """
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fmt = (fmt or out_path.suffix.lstrip(".") or "csv").lower()

    if fmt == "csv":
        df.to_csv(out_path, index=False)
    elif fmt in ("parquet", "pq"):
        df.to_parquet(out_path, index=False)
    else:
        raise ValueError(f"unsupported format {fmt!r}; use csv or parquet")

    ok, missing = source.covers(start_year, end_year)
    manifest_path = out_path.with_suffix(out_path.suffix + ".manifest.json")
    manifest = {
        "retrieved_at_utc": datetime.now(timezone.utc).isoformat(),
        "source_name": source.name,
        "source_url": source.url,
        "source_sha256": digest,
        "source_first_year": source.first_year,
        "source_last_year": source.last_year,
        "requested_start_year": start_year,
        "requested_end_year": end_year,
        "years_not_covered": missing,
        "years_present": sorted(int(y) for y in df["YEAR"].unique()),
        "top_age_open_ended": int(df["AGE"].max()),
        "n_rows": int(len(df)),
        "total_population": int(df["POPULATION"].sum()),
        "output_file": str(out_path),
    }
    manifest_path.write_text(json.dumps(manifest, indent=2))
    log.info("wrote %d rows -> %s", len(df), out_path)
    log.info("wrote provenance -> %s", manifest_path)
    return out_path, manifest_path


# --------------------------------------------------------------------------- #
# Orchestration
# --------------------------------------------------------------------------- #
def fetch_population(
    start_year: int = 2011,
    end_year: int = 2025,
    out_path: str | Path | None = "std_pop_seer.csv",
    source: SeerSource | str = DEFAULT_SOURCE,
    dest_dir: str | Path = "./_seer_cache",
    sex_labels: dict[int, object] | None = None,
    force_download: bool = False,
    run_qa: bool = True,
    fmt: str | None = None,
) -> pd.DataFrame:
    """
    Download -> parse -> QA -> write locally. Returns the parsed DataFrame.

    `sex_labels` overrides the SEER 1=M/2=F mapping -- pass {1: 1, 2: 2} to keep
    numeric codes matching a CMS SEX_CD cohort.
    """
    global SEER_SEX
    if sex_labels:
        SEER_SEX = dict(sex_labels)

    path, digest, src = download_seer_population(source, dest_dir, force=force_download)

    ok, missing = src.covers(start_year, end_year)
    if not ok:
        log.warning("%s", coverage_note(src, start_year, end_year))

    df = parse_seer_population(path, src, start_year, min(end_year, src.last_year))

    if run_qa:
        report = qa_seer_population(df)
        if len(report):
            log.info("QA passed:\n%s", report.to_string())

    if out_path:
        write_local(df, out_path, src, digest, start_year, end_year, fmt=fmt)
    return df


def main() -> None:
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    p = argparse.ArgumentParser(
        description="Download SEER population estimates to a local file"
    )
    p.add_argument("--start-year", type=int, default=2011)
    p.add_argument("--end-year", type=int, default=2025)
    p.add_argument("--out", default="std_pop_seer.csv",
                   help="output path; .csv or .parquet (default: std_pop_seer.csv)")
    p.add_argument("--source", default=DEFAULT_SOURCE, choices=list(SOURCES),
                   help="fallback registry entry; discovery overrides on 404")
    p.add_argument("--url", default=None,
                   help="explicit .txt.gz link, bypassing the registry and discovery")
    p.add_argument("--dest-dir", default="./_seer_cache",
                   help="where the raw .gz is cached")
    p.add_argument("--sex-labels", default=None,
                   help="remap SEER sex codes, e.g. '1=1,2=2' to keep CMS SEX_CD")
    p.add_argument("--force-download", action="store_true")
    p.add_argument("--no-qa", action="store_true")
    p.add_argument("--discover", action="store_true",
                   help="list the singleages files SEER currently publishes, then exit")
    p.add_argument("--parse-only", action="store_true",
                   help="download and parse; print a summary but write no output file")
    a = p.parse_args()

    if a.discover:
        for s in discover_sources(layout="singleages", scope="us", adjusted=False):
            print(f"{s.name:<34} {s.first_year}-{s.last_year}\n    {s.url}")
        return

    src = source_from_url(a.url) if a.url else SOURCES[a.source]
    print(coverage_note(src, a.start_year, a.end_year))

    sex_labels = None
    if a.sex_labels:
        sex_labels = {}
        for pair in a.sex_labels.split(","):
            k, v = pair.split("=")
            v = v.strip()
            sex_labels[int(k)] = int(v) if v.isdigit() else v

    df = fetch_population(
        start_year=a.start_year,
        end_year=a.end_year,
        out_path=None if a.parse_only else a.out,
        source=src,
        dest_dir=a.dest_dir,
        sex_labels=sex_labels,
        force_download=a.force_download,
        run_qa=not a.no_qa,
    )

    print(f"\n{len(df):,} rows | years {df.YEAR.min()}-{df.YEAR.max()} | "
          f"ages {df.AGE.min()}-{df.AGE.max()} | "
          f"sexes {sorted(map(str, df.SEX.unique()))}")
    print(df.head(10).to_string(index=False))


if __name__ == "__main__":
    main()