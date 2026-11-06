
import logging
import os
import re
from datetime import date
from functools import reduce
from typing import Iterable, Optional, Tuple, Union

from snowflake.snowpark import Session, Window
from snowflake.snowpark import functions as F

from config import env, conn


logger = logging.getLogger(__name__)


HISTORY_TABLE = "{site}.LDS_ADDRESS_HISTORY"
GEOCODE_TABLE = "{site}.LDS_ADDRESS_GEOCODE"

CMS_SITE = "GROUSE_DB.CMS_PCORNET_CDM"

MIN_DATE = "1900-01-01"
MAX_DATE = "9999-12-30"

GEO_COLS = [
    "GEOCODE_STATE", "GEOCODE_COUNTY", "GEOCODE_TRACT",
    "GEOCODE_GROUP", "GEOCODE_BLOCK", "GEOCODE_ZCTA",
    "GEOCODE_LATITUDE", "GEOCODE_LONGITUDE"
]

RSEI_YEARS = tuple(range(2011, 2021))
RSEI_MEASURES = ("CCONC", "CONC", "NCCONC", "SCORE", "TOXCONC")

# Number of leading block-group GEOID digits identifying each level.
GEO_LEVELS = {"CENSUS_TRACT": 11, "COUNTY": 5, "STATE": 2}
GEO_LEVEL_ALIASES = {"TRACT": "CENSUS_TRACT", "CT": "CENSUS_TRACT"}


def _optional(df, name, dtype="string"):
    """Return a typed column or typed NULL if absent."""
    if name.upper() in {c.upper() for c in df.columns}:
        return F.col(name).cast(dtype)
    return F.lit(None).cast(dtype)


def _digits(column, n: int):
    return column.rlike(f"^[0-9]{{{n}}}$")


def _clean(column):
    return F.trim(column.cast("string"))


def _census_block_group():
    """Resolve a block group, treating 11/12/15-digit GEOCODEIDs as FIPS GEOIDs."""
    geoid = _clean(F.col("GEOCODEID"))
    state = _clean(F.col("GEOCODE_STATE"))
    county = _clean(F.col("GEOCODE_COUNTY"))
    tract = _clean(F.col("GEOCODE_TRACT"))
    group = _clean(F.col("GEOCODE_GROUP"))
    block = _clean(F.col("GEOCODE_BLOCK"))

    return (
        F.when(_digits(geoid, 12), geoid)
        .when(_digits(geoid, 15), F.substring(geoid, 1, 12))
        .when(_digits(group, 12), group)
        .when(_digits(block, 15), F.substring(block, 1, 12))
        .when(_digits(geoid, 11) & _digits(group, 1), F.concat(geoid, group))
        .when(
            _digits(geoid, 11) & _digits(block, 4),
            F.concat(geoid, F.substring(block, 1, 1)),
        )
        .when(_digits(tract, 11) & _digits(group, 1), F.concat(tract, group))
        .when(
            _digits(tract, 11) & _digits(block, 4),
            F.concat(tract, F.substring(block, 1, 1)),
        )
        .when(
            _digits(state, 2) & _digits(county, 3) & _digits(tract, 6)
            & _digits(group, 1),
            F.concat(state, county, tract, group),
        )
        .when(
            _digits(state, 2) & _digits(county, 3) & _digits(tract, 6)
            & _digits(block, 4),
            F.concat(state, county, tract, F.substring(block, 1, 1)),
        )
        .when(
            _digits(county, 5) & _digits(tract, 6) & _digits(group, 1),
            F.concat(county, tract, group),
        )
        .when(
            _digits(county, 5) & _digits(tract, 6) & _digits(block, 4),
            F.concat(county, tract, F.substring(block, 1, 1)),
        )
        .otherwise(F.lit(None).cast("string"))
    )


def _census_tract(block_group):
    """Resolve an 11-digit tract without treating ZIP codes as census GEOIDs."""
    geoid = _clean(F.col("GEOCODEID"))
    state = _clean(F.col("GEOCODE_STATE"))
    county = _clean(F.col("GEOCODE_COUNTY"))
    tract = _clean(F.col("GEOCODE_TRACT"))
    block = _clean(F.col("GEOCODE_BLOCK"))

    return (
        F.when(block_group.is_not_null(), F.substring(block_group, 1, 11))
        .when(
            _digits(geoid, 11) | _digits(geoid, 12) | _digits(geoid, 15),
            F.substring(geoid, 1, 11),
        )
        .when(_digits(tract, 11), tract)
        .when(_digits(block, 15), F.substring(block, 1, 11))
        .when(
            _digits(state, 2) & _digits(county, 3) & _digits(tract, 6),
            F.concat(state, county, tract),
        )
        .when(_digits(county, 5) & _digits(tract, 6), F.concat(county, tract))
        .otherwise(F.lit(None).cast("string"))
    )


def _geography_detail(block_group, tract):
    """Rank the geographic detail actually present in a geocode."""
    accuracy = F.upper(_clean(F.col("GEO_ACCURACY")))
    geoid = _clean(F.col("GEOCODEID"))
    state = _clean(F.col("GEOCODE_STATE"))
    county = _clean(F.col("GEOCODE_COUNTY"))
    block = _clean(F.col("GEOCODE_BLOCK"))

    return (
        F.when(
            _digits(geoid, 15) | _digits(block, 15)
            | (_digits(block, 4) & tract.is_not_null()),
            F.lit(5),
        )
        .when(block_group.is_not_null(), F.lit(4))
        .when(tract.is_not_null(), F.lit(3))
        .when((_digits(geoid, 5) & (accuracy == "CN")) | _digits(county, 5), F.lit(2))
        .when(_digits(state, 2) & _digits(county, 3), F.lit(2))
        .when((_digits(geoid, 2) & (accuracy == "ST")) | _digits(state, 2), F.lit(1))
        .otherwise(F.lit(0))
    )


def _validate_period(period) -> int:
    if (
        isinstance(period, bool)
        or not isinstance(period, int)
        or not 2011 <= period <= 2020
    ):
        raise ValueError("period must be an integer calendar year from 2011 to 2020.")
    return period


def _validate_years(years: Iterable[int]) -> Tuple[int, ...]:
    years = tuple(dict.fromkeys(years))
    if not years:
        raise ValueError("At least one period is required.")
    for year in years:
        _validate_period(year)
    return years


def _chemical_number(chem) -> Optional[int]:
    if chem is None:
        return None
    if isinstance(chem, bool) or re.fullmatch(r"[0-9]{1,38}", str(chem)) is None:
        raise ValueError("chem must be a nonnegative integer chemical number or None.")
    return int(chem)


def _site_geo_df(session: Session, site: str):
    """
    Build one record per patient/address period, attaching
    the best available geocoding result.

    Rank nonempty assignments by GEO_ACCURACY: BL, BG/Z9, TR/Z5,
    CN/CY, ST, then unknown codes. Resolve ties by available geographic
    detail, completeness, assignment date, and deterministic field order.
    ZIP-only identifiers do not supply a census block group or tract.
    Does not collapse a patient's longitudinal address history.
    """

    h = session.table(HISTORY_TABLE.format(site=site))
    g = session.table(GEOCODE_TABLE.format(site=site))

    history = h.select(
        F.col("PATID").cast("string").alias("PATID"),
        F.col("ADDRESSID").cast("string").alias("ADDRESSID"),
        _optional(h, "ADDRESS_USE").alias("ADDRESS_USE"),
        _optional(h, "ADDRESS_TYPE").alias("ADDRESS_TYPE"),
        _optional(h, "ADDRESS_PREFERRED").alias("ADDRESS_PREFERRED"),
        _optional(h, "CURRENT_ADDRESS_FLAG").alias("CURRENT_ADDRESS_FLAG"),
        _optional(h, "ADDRESS_CITY").alias("ADDRESS_CITY"),
        _optional(h, "ADDRESS_STATE").alias("ADDRESS_STATE"),
        _optional(h, "ADDRESS_ZIP5").alias("ADDRESS_ZIP5"),
        _optional(h, "STATE_FIPS").alias("STATE_FIPS"),
        _optional(h, "COUNTY_FIPS").alias("COUNTY_FIPS"),
        _optional(h, "RUCA_ZIP").alias("RUCA_ZIP"),
        _optional(h, "ADDRESS_PERIOD_START", "date").alias("ADDRESS_PERIOD_START"),
        _optional(h, "ADDRESS_PERIOD_END", "date").alias("ADDRESS_PERIOD_END")
    ).filter(
        F.col("PATID").is_not_null()
        & F.col("ADDRESSID").is_not_null()
    )

    geocode = g.select(
        F.col("ADDRESSID").cast("string").alias("ADDRESSID"),
        _optional(g, "GEOCODEID").alias("GEOCODEID"),
        _optional(g, "ASSIGNMENT_DATE", "date").alias("ASSIGNMENT_DATE"),
        _optional(g, "GEO_ACCURACY").alias("GEO_ACCURACY"),
        _optional(g, "GEO_PROV_REF").alias("GEO_PROV_REF"),
        _optional(g, "GEOCODE_STATE").alias("GEOCODE_STATE"),
        _optional(g, "GEOCODE_COUNTY").alias("GEOCODE_COUNTY"),
        _optional(g, "GEOCODE_TRACT").alias("GEOCODE_TRACT"),
        _optional(g, "GEOCODE_GROUP").alias("GEOCODE_GROUP"),
        _optional(g, "GEOCODE_BLOCK").alias("GEOCODE_BLOCK"),
        _optional(g, "GEOCODE_ZCTA").alias("GEOCODE_ZCTA"),
        _optional(g, "GEOCODE_LATITUDE", "double").alias("GEOCODE_LATITUDE"),
        _optional(g, "GEOCODE_LONGITUDE", "double").alias("GEOCODE_LONGITUDE")
    )

    accuracy = F.upper(_clean(F.col("GEO_ACCURACY")))
    has_geography = reduce(
        lambda present, name: present | (F.length(_clean(F.col(name))) > 0),
        ("GEOCODEID", *GEO_COLS),
        F.lit(False),
    )

    geocode = geocode.with_column(
        "GEOCODE_SCORE",
        sum(
            (
                F.when(F.col(c).is_not_null(), F.lit(1))
                 .otherwise(F.lit(0))
                for c in GEO_COLS
            ),
            F.lit(0)
        )
    ).with_column(
        "_HAS_GEOGRAPHY", F.when(has_geography, F.lit(1)).otherwise(F.lit(0))
    ).with_column(
        "_ACCURACY_RANK",
        F.when(accuracy == "BL", F.lit(5))
        .when(accuracy.isin("BG", "Z9"), F.lit(4))
        .when(accuracy.isin("TR", "Z5"), F.lit(3))
        .when(accuracy.isin("CN", "CY"), F.lit(2))
        .when(accuracy == "ST", F.lit(1))
        .otherwise(F.lit(0))
    ).with_column(
        "_BLOCK_GROUP", _census_block_group()
    ).with_column(
        "_TRACT", _census_tract(F.col("_BLOCK_GROUP"))
    ).with_column(
        "_DETAIL_RANK",
        _geography_detail(F.col("_BLOCK_GROUP"), F.col("_TRACT"))
    )

    gw = Window.partition_by("ADDRESSID").order_by(
        F.col("_HAS_GEOGRAPHY").desc(),
        F.col("_ACCURACY_RANK").desc(),
        F.col("_DETAIL_RANK").desc(),
        F.col("GEOCODE_SCORE").desc(),
        F.col("ASSIGNMENT_DATE").desc_nulls_last(),
        F.col("GEOCODEID").asc_nulls_last(),
        *[F.col(c).asc_nulls_last() for c in GEO_COLS],
        F.col("GEO_ACCURACY").asc_nulls_last(),
        F.col("GEO_PROV_REF").asc_nulls_last(),
    )

    geocode = (
        geocode
        .with_column("_RN", F.row_number().over(gw))
        .filter(F.col("_RN") == 1)
        .drop(
            "_RN", "_HAS_GEOGRAPHY", "_ACCURACY_RANK", "_DETAIL_RANK",
            "_BLOCK_GROUP", "_TRACT",
        )
    )

    result = history.join(geocode, "ADDRESSID", "left")

    result = (
        result
        .with_column("SOURCE_SITE", F.lit(site))
        .with_column(
            "START_DATE_UNKNOWN",
            F.col("ADDRESS_PERIOD_START").is_null()
        )
        .with_column(
            "END_DATE_UNKNOWN",
            F.col("ADDRESS_PERIOD_END").is_null()
        )
        .with_column(
            "START_DATE",
            F.coalesce(
                F.col("ADDRESS_PERIOD_START"),
                F.to_date(F.lit(MIN_DATE))
            )
        )
        .with_column(
            "END_DATE",
            F.coalesce(
                F.col("ADDRESS_PERIOD_END"),
                F.to_date(F.lit(MAX_DATE))
            )
        )
        .filter(F.col("START_DATE") <= F.col("END_DATE"))
    )

    return result


def _build_pat_geo_site(
    session: Session,
    out_tbl: str,
    site: str
):
    """
    Build longitudinal patient-geography table for one site.

    One row per source address record, with the selected
    geocoding assignment. INTERVAL_START/INTERVAL_END mirror the
    (imputed) address-period dates so the table can feed
    _build_pat_geo_year directly; records may overlap.
    """

    df = (
        _site_geo_df(session, site)
        .with_column("INTERVAL_START", F.col("START_DATE"))
        .with_column("INTERVAL_END", F.col("END_DATE"))
    )

    df.write.mode("overwrite").save_as_table(out_tbl)

    return session.table(out_tbl)


def _build_pat_geo(
    session: Session,
    out_tbl: str,
    sites: list
):
    """
    Build a reconciled longitudinal patient-geography table, if same patient appears
    across multiple PCORnet sites or data sources

    Output:
        One row per patient per non-overlapping time interval
        (multiple sites). With a single site, delegates to
        _build_pat_geo_site: one row per address record, with
        INTERVAL_START/INTERVAL_END that may overlap.

    Conflict resolution:
        1. Address preferred flag
        2. Available geocoding completeness
        3. Current address flag
        4. Most recent address-period start
        5. Most recent geocode assignment
        6. Deterministic source-site/address tie break

    Note: No cross-site identity resolution is performed here.
    """

    sites = list(dict.fromkeys(sites))

    # Single site: keep per-record address history (possibly overlapping);
    # _build_pat_geo_year merges overlaps when counting days.
    if len(sites) == 1:
        return _build_pat_geo_site(session, out_tbl, sites[0])

    if not sites:
        raise ValueError("At least one site is required.")

    # --------------------------------------------------
    # 1. Build and combine site-level address histories
    # --------------------------------------------------

    frames = [_site_geo_df(session, s) for s in sites]

    combined = reduce(
        lambda x, y: x.union_all_by_name(y),
        frames
    )

    # Exclude exact duplicate source address records.
    combined = combined.drop_duplicates()

    # --------------------------------------------------
    # 2. Construct temporal boundaries
    #
    # An address covering [start, end] is represented
    # internally as [start, end + 1 day).
    # --------------------------------------------------

    starts = combined.select(
        "PATID",
        F.col("START_DATE").alias("BOUNDARY")
    )

    ends = combined.select(
        "PATID",
        F.dateadd(
            "day", F.lit(1), F.col("END_DATE")
        ).alias("BOUNDARY")
    )

    boundaries = (
        starts.union_all_by_name(ends)
        .distinct()
    )

    bw = Window.partition_by("PATID").order_by("BOUNDARY")

    intervals = (
        boundaries
        .with_column(
            "NEXT_BOUNDARY",
            F.lead("BOUNDARY").over(bw)
        )
        .filter(F.col("NEXT_BOUNDARY").is_not_null())
        .select(
            "PATID",
            F.col("BOUNDARY").alias("INTERVAL_START"),
            F.dateadd(
                "day", F.lit(-1), F.col("NEXT_BOUNDARY")
            ).alias("INTERVAL_END")
        )
    )

    # --------------------------------------------------
    # 3. Match each interval to covering address records
    # --------------------------------------------------

    # Rename interval columns so the join has no duplicate names;
    # Snowpark otherwise suffixes them (e.g. PATIDI/PATIDA).
    i = intervals.select(
        F.col("PATID").alias("_I_PATID"),
        F.col("INTERVAL_START").alias("_I_INTERVAL_START"),
        F.col("INTERVAL_END").alias("_I_INTERVAL_END"),
    )

    candidates = i.join(
        combined,
        (F.col("_I_PATID") == F.col("PATID"))
        & (F.col("START_DATE") <= F.col("_I_INTERVAL_START"))
        & (F.col("END_DATE") >= F.col("_I_INTERVAL_END")),
        "inner"
    ).select(
        F.col("_I_INTERVAL_START").alias("INTERVAL_START"),
        F.col("_I_INTERVAL_END").alias("INTERVAL_END"),
        *[F.col(c) for c in combined.columns]
    )

    # --------------------------------------------------
    # 4. Identify overlapping / conflicting geography
    # --------------------------------------------------

    # Use a normalized geographic signature.
    # This captures disagreements in geocode IDs, census tract,
    # block group, block, ZIP/ZCTA, and coordinates.
    candidates = candidates.with_column(
        "_GEO_SIGNATURE",
        F.concat_ws(
            F.lit("|"),
            *[
                F.coalesce(
                    F.col(c).cast("string"),
                    F.lit("<NULL>")
                )
                for c in ("GEOCODEID", *GEO_COLS)
            ]
        )
    )

    conflict = (
        candidates
        .group_by(
            "PATID", "INTERVAL_START", "INTERVAL_END"
        )
        .agg(
            F.count_distinct("SOURCE_SITE").alias("N_SITES"),
            F.count(F.lit(1)).alias("N_ADDRESS_RECORDS"),
            F.count_distinct("_GEO_SIGNATURE").alias("N_GEO_VALUES")
        )
        .with_column(
            "GEO_CONFLICT",
            F.col("N_GEO_VALUES") > 1
        )
        .with_column(
            "ADDRESS_OVERLAP",
            F.col("N_ADDRESS_RECORDS") > 1
        )
    )

    # --------------------------------------------------
    # 5. Rank candidate geography assignments
    # --------------------------------------------------

    candidates = (
        candidates
        .with_column(
            "_PREFERRED",
            F.when(
                F.upper(F.col("ADDRESS_PREFERRED")) == "Y", 1
            ).otherwise(0)
        )
        .with_column(
            "_CURRENT",
            F.when(
                F.upper(F.col("CURRENT_ADDRESS_FLAG")) == "Y", 1
            ).otherwise(0)
        )
    )

    rw = Window.partition_by(
        "PATID", "INTERVAL_START", "INTERVAL_END"
    ).order_by(
        F.col("_PREFERRED").desc(),
        F.col("GEOCODE_SCORE").desc_nulls_last(),
        F.col("_CURRENT").desc(),
        F.col("ADDRESS_PERIOD_START").desc_nulls_last(),
        F.col("ASSIGNMENT_DATE").desc_nulls_last(),
        F.col("SOURCE_SITE").asc(),
        F.col("ADDRESSID").asc(),
        F.col("GEOCODEID").asc_nulls_last()
    )

    selected = (
        candidates
        .with_column("_RN", F.row_number().over(rw))
        .filter(F.col("_RN") == 1)
        .drop(
            "_RN", "_PREFERRED", "_CURRENT",
            "_GEO_SIGNATURE"
        )
    )

    # --------------------------------------------------
    # 6. Attach conflict diagnostics
    # --------------------------------------------------

    result = selected.join(
        conflict,
        ["PATID", "INTERVAL_START", "INTERVAL_END"],
        "left"
    )

    # Output intervals are disjoint and use inclusive dates.
    # Keep original address dates for provenance.
    result = result.drop("START_DATE", "END_DATE")

    # --------------------------------------------------
    # 7. Save consolidated table
    # --------------------------------------------------

    result.write.mode("overwrite").save_as_table(out_tbl)

    return session.table(out_tbl)


def _islands(df, keys):
    """
    Tag rows with _ISLAND so overlapping or adjacent [_S, _E] date ranges
    within keys share an id (gaps-and-islands), letting days be counted once.
    """
    order = (F.col("_S"), F.col("_E"))
    prior = (
        Window.partition_by(*keys).order_by(*order)
        .rows_between(Window.UNBOUNDED_PRECEDING, -1)
    )
    running = (
        Window.partition_by(*keys).order_by(*order)
        .rows_between(Window.UNBOUNDED_PRECEDING, Window.CURRENT_ROW)
    )
    return (
        df.with_column("_PREV_END", F.max("_E").over(prior))
        .with_column(
            "_NEW",
            F.when(
                F.col("_PREV_END").is_null()
                | (F.col("_S") > F.dateadd("day", F.lit(1), F.col("_PREV_END"))),
                F.lit(1),
            ).otherwise(F.lit(0)),
        )
        .with_column("_ISLAND", F.sum("_NEW").over(running))
    )


def _build_pat_geo_year(
    session: Session,
    geo_hist: str,
    out_tbl: str,
    years: Iterable[int] = RSEI_YEARS,
):
    """
    Overwrite out_tbl with one row per patient-year:
    PATID, YEAR, CENSUS_BLOCK_GROUP, CENSUS_TRACT, DAYS_ASSIGNED,
    DAYS_COVERED, N_BLOCK_GROUPS, DATE_IMPUTED.

    geo_hist comes from _build_pat_geo: reconciled disjoint intervals
    (multiple sites) or per-record, possibly overlapping intervals (single
    site). Overlapping days are counted once in DAYS_ASSIGNED and
    DAYS_COVERED. Each interval is split across calendar years; the block group covering
    the most days in a year is assigned (ties: latest interval start,
    then lowest GEOID). Patient-years whose addresses never resolve to a
    block group keep a NULL CENSUS_BLOCK_GROUP, so they remain unmatched
    rather than dropped. DATE_IMPUTED flags assignments relying on a
    missing address-period start or end date.
    """
    years = _validate_years(years)

    hist = session.table(geo_hist)
    required = {
        "PATID", "INTERVAL_START", "INTERVAL_END", "GEOCODEID",
        "GEOCODE_STATE", "GEOCODE_COUNTY", "GEOCODE_TRACT",
        "GEOCODE_GROUP", "GEOCODE_BLOCK",
        "START_DATE_UNKNOWN", "END_DATE_UNKNOWN",
    }
    missing = sorted(required - {c.strip('"').upper() for c in hist.columns})
    if missing:
        raise ValueError(
            f"{geo_hist} is missing {', '.join(missing)}; build it with "
            "_build_pat_geo or _build_pat_geo_site."
        )

    block_group = _census_block_group()
    intervals = hist.select(
        F.col("PATID"),
        F.col("INTERVAL_START"),
        F.col("INTERVAL_END"),
        block_group.alias("CENSUS_BLOCK_GROUP"),
        _census_tract(block_group).alias("CENSUS_TRACT"),
        F.when(
            F.col("START_DATE_UNKNOWN") | F.col("END_DATE_UNKNOWN"), F.lit(1)
        ).otherwise(F.lit(0)).alias("_IMPUTED"),
    )

    calendar = session.create_dataframe(
        [(year, date(year, 1, 1), date(year, 12, 31)) for year in years],
        schema=["YEAR", "YEAR_START", "YEAR_END"],
    )

    overlap = (
        intervals.cross_join(calendar)
        .filter(
            (F.col("INTERVAL_START") <= F.col("YEAR_END"))
            & (F.col("INTERVAL_END") >= F.col("YEAR_START"))
        )
        .with_column(
            "_S", F.greatest(F.col("INTERVAL_START"), F.col("YEAR_START"))
        )
        .with_column(
            "_E", F.least(F.col("INTERVAL_END"), F.col("YEAR_END"))
        )
    )

    keys = ["PATID", "YEAR"]
    geo_keys = [*keys, "CENSUS_BLOCK_GROUP", "CENSUS_TRACT"]
    by_geo = (
        _islands(overlap, geo_keys)
        .group_by(*geo_keys, "_ISLAND")
        .agg(
            (F.datediff("day", F.min("_S"), F.max("_E")) + F.lit(1)).alias("_SPAN"),
            F.max("INTERVAL_START").alias("_LAST_START"),
            F.max("_IMPUTED").alias("_IMPUTED"),
        )
        .group_by(*geo_keys)
        .agg(
            F.sum("_SPAN").alias("DAYS_ASSIGNED"),
            F.max("_LAST_START").alias("_LAST_START"),
            F.max("_IMPUTED").alias("_IMPUTED"),
        )
    )
    covered = (
        _islands(overlap, keys)
        .group_by(*keys, "_ISLAND")
        .agg((F.datediff("day", F.min("_S"), F.max("_E")) + F.lit(1)).alias("_SPAN"))
        .group_by(*keys)
        .agg(F.sum("_SPAN").alias("DAYS_COVERED"))
    )
    totals = by_geo.group_by(*keys).agg(
        F.count_distinct("CENSUS_BLOCK_GROUP").alias("N_BLOCK_GROUPS"),
    ).join(covered, keys, "inner")

    rank = Window.partition_by(*keys).order_by(
        F.when(F.col("CENSUS_BLOCK_GROUP").is_null(), 1).otherwise(0).asc(),
        F.when(F.col("CENSUS_TRACT").is_null(), 1).otherwise(0).asc(),
        F.col("DAYS_ASSIGNED").desc(),
        F.col("_LAST_START").desc(),
        F.col("CENSUS_BLOCK_GROUP").asc_nulls_last(),
        F.col("CENSUS_TRACT").asc_nulls_last(),
    )

    result = (
        by_geo
        .with_column("_RN", F.row_number().over(rank))
        .filter(F.col("_RN") == 1)
        .join(totals, keys, "inner")
        .select(
            "PATID",
            "YEAR",
            "CENSUS_BLOCK_GROUP",
            "CENSUS_TRACT",
            "DAYS_ASSIGNED",
            "DAYS_COVERED",
            "N_BLOCK_GROUPS",
            (F.col("_IMPUTED") == F.lit(1)).alias("DATE_IMPUTED"),
        )
    )

    result.write.mode("overwrite").save_as_table(out_tbl)
    logger.info("Created patient-year geography table %s.", out_tbl)

    return session.table(out_tbl)


def _build_pp_by_chem(
    session: Session, 
    pat_geo: str,
    period: int, 
    chem: str | int | None = None
) -> None:
    """
    Overwrite {pat_geo}_PP_{period} with patient-year chemical exposures.

    period is a calendar year from 2011 through 2020. Read exposures from
    EPA_DB.EPA_RSEI.RSEI_BG_AREAL_TOBLER_{period}, restricted to that YEAR.
    Restrict pat_geo (PATID, CENSUS_BLOCK_GROUP, YEAR) to the same year.
    When chem is supplied, select its integer CHEMICAL_NUMBER; otherwise
    include all available chemicals.

    Left-join on CENSUS_BLOCK_GROUP and YEAR, preserving block-group
    strings (including leading zeros). Each patient-geography row yields
    one row per matching chemical, or one row with NULL chemical/exposure
    fields if unmatched. Missing exposures are not replaced with zero.
    Include CONC, TOXCONC, SCORE, CCONC, NCCONC, and STATE_FIPS.
    The output name is the same for selected-chemical and all-chemical
    builds, so either build replaces any previous output for that year.
    """
    period = _validate_period(period)
    chemical_number = _chemical_number(chem)

    patients = (
        session.table(pat_geo)
        .filter(F.col("YEAR") == F.lit(period))
        .select("PATID", "CENSUS_BLOCK_GROUP", "YEAR")
    )
    exposures = session.table(
        f"EPA_DB.EPA_RSEI.RSEI_BG_AREAL_TOBLER_{period}"
    ).filter(F.col("YEAR") == F.lit(period))
    if chemical_number is not None:
        exposures = exposures.filter(
            F.col("CHEMICAL_NUMBER") == F.lit(chemical_number)
        )

    output = patients.join(
        exposures, on=["CENSUS_BLOCK_GROUP", "YEAR"], how="left"
    ).select(
        "PATID",
        "CENSUS_BLOCK_GROUP",
        "YEAR",
        "CHEMICAL_NUMBER",
        "CONC",
        "TOXCONC",
        "SCORE",
        "CCONC",
        "NCCONC",
        "STATE_FIPS",
    )
    out_tbl = f"{pat_geo}_PP_{period}"
    output.write.mode("overwrite").save_as_table(out_tbl)
    logger.info(
        "Created patient-year chemical exposure table %s for year %s (chemical=%s).",
        out_tbl,
        period,
        chemical_number if chemical_number is not None else "all",
    )


def _build_rsei_pp(
    session: Session,
    pat_geo: str,
    out_tbl: str,
    years: Iterable[int] = RSEI_YEARS,
    chem: Union[str, int, None] = None,
    keep_yearly: bool = False,
):
    """
    Overwrite out_tbl with patient-year chemical exposures across years.

    Runs _build_pp_by_chem for each year and stacks the yearly
    {pat_geo}_PP_{year} tables, which are dropped unless keep_yearly.
    chem=None includes all chemicals.
    """
    years = _validate_years(years)
    _chemical_number(chem)

    for i, year in enumerate(years):
        _build_pp_by_chem(session, pat_geo, year, chem)
        yearly = session.table(f"{pat_geo}_PP_{year}")
        yearly.write.mode("overwrite" if i == 0 else "append").save_as_table(out_tbl)
        if not keep_yearly:
            yearly.drop_table()
        logger.info("Added %s exposures to %s.", year, out_tbl)

    return session.table(out_tbl)


def _summarize_chem(
    session: Session,
    pp_tbl: str,
    geo_agg: str,
    chem: Union[str, int, None] = None,
    out_tbl: Optional[str] = None,
):
    '''
    Summarize chemical exposures for all or selected chemicals at the
    specified geographic aggregation level (CENSUS_TRACT, COUNTY, STATE).

    pp_tbl is a patient-year exposure table from _build_rsei_pp. Areas
    are the leading digits of CENSUS_BLOCK_GROUP; patient-years without
    a valid block group are excluded.

    Writes long-format rows to out_tbl (default {pp_tbl}_SUMM_{geo_agg}),
    one per GEOID x YEAR x CHEMICAL_NUMBER x MEASURE, where MEASURE is
    one of CCONC, CONC, NCCONC, SCORE, TOXCONC. Statistics use
    patient-years with a non-NULL value: N_PATIENTS, N_BLOCK_GROUPS,
    N_PATIENTS_EXPOSED (value > 0), TOTAL (sum over patient-years),
    MEAN, SD, MIN, P25, MEDIAN, P75, MAX. N_PATIENTS_GEO counts all
    patient-years in the area, including those unmatched to RSEI, and
    PCT_PATIENTS_EXPOSED = N_PATIENTS_EXPOSED / N_PATIENTS_GEO.
    '''
    level = str(geo_agg).strip().upper()
    level = GEO_LEVEL_ALIASES.get(level, level)
    if level not in GEO_LEVELS:
        raise ValueError(
            f"geo_agg must be one of {', '.join(GEO_LEVELS)}."
        )
    chemical_number = _chemical_number(chem)
    if out_tbl is None:
        out_tbl = f"{pp_tbl}_SUMM_{level}"

    pp = (
        session.table(pp_tbl)
        .filter(_digits(F.col("CENSUS_BLOCK_GROUP"), 12))
        .with_column(
            "GEOID", F.substring(F.col("CENSUS_BLOCK_GROUP"), 1, GEO_LEVELS[level])
        )
    )

    denominators = pp.group_by("GEOID", "YEAR").agg(
        F.count_distinct("PATID").alias("N_PATIENTS_GEO")
    )

    exposures = pp.filter(F.col("CHEMICAL_NUMBER").is_not_null())
    if chemical_number is not None:
        exposures = exposures.filter(
            F.col("CHEMICAL_NUMBER") == F.lit(chemical_number)
        )

    long = reduce(
        lambda x, y: x.union_all_by_name(y),
        [
            exposures.select(
                "PATID",
                "CENSUS_BLOCK_GROUP",
                "GEOID",
                "YEAR",
                "CHEMICAL_NUMBER",
                F.lit(measure).alias("MEASURE"),
                F.col(measure).cast("double").alias("VALUE"),
            ).filter(F.col("VALUE").is_not_null())
            for measure in RSEI_MEASURES
        ],
    )

    value = F.col("VALUE")
    summary = long.group_by(
        "GEOID", "YEAR", "CHEMICAL_NUMBER", "MEASURE"
    ).agg(
        F.count_distinct("PATID").alias("N_PATIENTS"),
        F.count_distinct("CENSUS_BLOCK_GROUP").alias("N_BLOCK_GROUPS"),
        F.count_distinct(F.when(value > 0, F.col("PATID"))).alias(
            "N_PATIENTS_EXPOSED"
        ),
        F.sum(value).alias("TOTAL"),
        F.avg(value).alias("MEAN"),
        F.stddev(value).alias("SD"),
        F.min(value).alias("MIN"),
        F.percentile_cont(0.25).within_group(value).alias("P25"),
        F.median(value).alias("MEDIAN"),
        F.percentile_cont(0.75).within_group(value).alias("P75"),
        F.max(value).alias("MAX"),
    )

    result = summary.join(denominators, ["GEOID", "YEAR"], "left").select(
        F.lit(level).alias("GEO_LEVEL"),
        "GEOID",
        "YEAR",
        "CHEMICAL_NUMBER",
        "MEASURE",
        "N_PATIENTS_GEO",
        "N_PATIENTS",
        "N_BLOCK_GROUPS",
        "N_PATIENTS_EXPOSED",
        (
            F.col("N_PATIENTS_EXPOSED") / F.col("N_PATIENTS_GEO")
        ).alias("PCT_PATIENTS_EXPOSED"),
        "TOTAL",
        "MEAN",
        "SD",
        "MIN",
        "P25",
        "MEDIAN",
        "P75",
        "MAX",
    )

    result.write.mode("overwrite").save_as_table(out_tbl)
    logger.info("Created %s exposure summary table %s.", level, out_tbl)

    return session.table(out_tbl)


def main(
    geo_hist_tbl: str = "CMS_GEO_HIST",
    pat_geo_tbl: str = "CMS_GEO",
    pp_tbl: str = "CMS_RSEI_PP",
    years: Iterable[int] = RSEI_YEARS,
) -> None:
    logging.basicConfig(level=logging.INFO)
    env.load_env()
    session = conn.get_snow_conn(conn.load_snowcfg("ID"))
    try:
        session.use_database(os.environ["ID_SNOW_DATABASE"])
        session.use_schema("SX_ALS_GPC")
        session.use_warehouse(os.environ["DEV_SNOW_WAREHOUSE"])
        logger.info(
            session.sql(
                "SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), "
                "CURRENT_SCHEMA(), CURRENT_WAREHOUSE()"
            ).collect()
        )

        # 1. CMS patient geography: reconciled address intervals, then
        #    one block group per patient-year.
        _build_pat_geo(session, geo_hist_tbl, [CMS_SITE])
        _build_pat_geo_year(session, geo_hist_tbl, pat_geo_tbl, years)

        # 2. Patient-year exposures for all chemicals.
        _build_rsei_pp(session, pat_geo_tbl, pp_tbl, years)

        # 3. Area-level exposure summaries.
        for level in GEO_LEVELS:
            _summarize_chem(session, pp_tbl, level)
    finally:
        session.close()


if __name__ == "__main__":
    main()
