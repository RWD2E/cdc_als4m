
"""Build analysis-ready ALS-RSEI tables in Snowflake (Snowpark Python).

Edit Config defaults to match the cohort and confounder tables in your environment.
Run from a Snowpark Python worksheet by calling main(session), or import and
call build(session, Config()). Direct execution uses the ID Snowflake
configuration from config.conn. Output tables are overwritten; source
tables are not changed. All configured inputs must already exist and the
output schema must be writable.

Cohort: ALS cases come from ALS_CASE_TABLE1, indexed at ALS1DX_DATE (initial
ALS diagnosis). Each case is risk-set matched to up to n_controls controls from
PAT_TABLE1 on birth year and sex. Eligible controls are never in
ALS_CASE_TABLE1, are under observation on the case's ALS1DX_DATE
(PAT_TABLE1 INDEX_DATE <= date <= CENSOR_DATE), and have residential
geography in the year before the index year. Each control is used at most once
and inherits the case's index date; MATCH_ID is the case PATID. Controls are
allocated in rounds: each unused eligible control is offered to one random case
still needing controls, and controls a case cannot accept are re-offered in the
next round. A case therefore ends with fewer than n_controls only when every
control eligible for it is already matched to another case. Matching is
reproducible for a given seed. Cases without geography in INDEX_YEAR - 1 are
excluded because they cannot have a complete pre-index window.

Covariates: static columns come from PAT_TABLE1. AGE is
INDEX_YEAR - 1 - birth year. DUAL_LIS is time-varying: in
ALS_RSEI_DML_GEO_YEAR it is 1 for each pre-index person-year overlapped by a
LDS_ENROLLMENT span with RAW_BASIS DUAL or LIS (a NULL ENR_END_DATE is
open-ended), else 0. BASE.DUAL_LIS is that flag in INDEX_YEAR - 1; each window
row adds DUAL_LIS_YEARS (years flagged in the window) and DUAL_LIS_ANY.
DUAL_LIS is not a matching criterion.

Design: one row per PATID x chemical x window, index-year excluded. Annual
chemical values are aggregated over complete pre-index windows. Missing annual
chemical records are NOT automatically reclassified as zero exposure.
Geographic identifiers must be 11-digit tracts or 12-digit block groups stored
as strings with leading zeros preserved. N_CBG counts distinct supplied
geographies, including tract-only identifiers. State FIPS comes from the
geography prefix; RUCA uses the tract and ADI requires a block group.
Repeated ADI rows with the same normalized geography and numeric ranks are
collapsed before joining. Different rank profiles for one geography remain an
error rather than being selected arbitrarily.
Within the pre-index window, malformed or conflicting geographies and duplicate
person-year-chemical keys stop the build before exposure outputs are written
(the matched cohort table is written first). Exposure rows without geography
are excluded with a warning. A write failure can leave output tables partially
replaced.
"""
import logging
from collections.abc import Iterable
from dataclasses import dataclass
from numbers import Integral
import re
import os
from config import conn, env

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    


@dataclass(frozen=True)
class Config:
    exposure_table: str = "GROUSE_ANALYTICS_DB.SX_ALS_GPC.CMS_RSEI_PP"
    # Requires PATID and ALS1DX_DATE (INDEX_DATE is used only if
    # ALS1DX_DATE is absent, as in the als_tbl1.py output).
    case_table: str = "GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_CASE_TABLE1"
    # Requires PATID, BIRTH_DATE, SEX, INDEX_DATE, CENSOR_DATE, static_cols.
    pat_table: str = "GROUSE_ANALYTICS_DB.SX_ALS_GPC.PAT_TABLE1"
    enrollment_table: str = "GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT"
    # RUCA requires TRACTFIPS20/PRIMARYRUCA; ADI requires GEOCODEID,
    # ADI_NATRANK, and ADI_STATERANK.
    ruca_table: str = "SDOH_DB.RUCA.RUCA_CT_2020"
    adi_table: str = "SDOH_DB.ADI.ADI_BG_2020"
    output_schema: str = "GROUSE_ANALYTICS_DB.SX_ALS_GPC"
    # Columns of pat_table. RUCA/ADI are joined from residence geography.
    static_cols: tuple[str, ...] = ("SEX", "RACE", "HISPANIC")
    n_controls: int = 10
    seed: int = 1234
    # Arbitrary positive integer lookbacks; e.g. tuple(range(1, 11)).
    windows: tuple[int, ...] = tuple(range(2, 5))
    exposure_metric: str = "SCORE"  # SCORE, TOXCONC, CONC, CCONC, NCCONC
    # A missing exposure row means unknown, unless ETL guarantees dense chemical
    # annual coverage with 0 for true non-exposure.


IDENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
# Scratch tables for control matching; created in output_schema and dropped.
MATCH_WORK_TABLES = (
    "ALS_RSEI_DML_MATCH_CASES", "ALS_RSEI_DML_MATCH_CANDIDATES",
    "ALS_RSEI_DML_MATCH_PAIRS", "ALS_RSEI_DML_MATCH_ROUND",
)
RESERVED_COLUMNS = {
    "PATID", "MATCH_ID", "INDEX_DATE", "INDEX_YEAR", "ALS", "AGE", "DUAL_LIS",
    "BASELINE_CBG", "RUCA", "ADI_NATRANK", "ADI_STATERANK", "RUCA_MATCHED",
    "ADI_MATCHED", "CHEMICAL_NUMBER", "EXPOSURE_MEAN", "EXPOSURE_SUM",
    "EXPOSURE_MIN", "EXPOSURE_MAX", "EXPOSURE_SD_WITHIN", "N_CHEM_YEARS",
    "N_GEO_YEARS", "N_CBG", "N_STATES", "MIN_CBG", "MIN_STATE_FIPS",
    "INDEX_MINUS_1_CBG", "INDEX_MINUS_1_STATE", "DUAL_LIS_YEARS",
    "DUAL_LIS_ANY", "WINDOW_YEARS",
}


def checked_name(value, dotted=False):
    if not isinstance(value, str):
        raise ValueError(f"Invalid identifier: {value!r}")
    parts = value.split(".") if dotted else [value]
    if len(parts) > (3 if dotted else 1) or not all(IDENT.fullmatch(p) for p in parts):
        raise ValueError(f"Invalid identifier: {value!r}")
    return ".".join(parts)


def _is_int(value):
    return not isinstance(value, bool) and isinstance(value, Integral)


def _validate_config(cfg):
    sources = (
        cfg.exposure_table, cfg.case_table, cfg.pat_table,
        cfg.enrollment_table, cfg.ruca_table, cfg.adi_table,
    )
    for name in (*sources, cfg.output_schema):
        checked_name(name, dotted=True)
    if len(cfg.output_schema.split(".")) > 2:
        raise ValueError("output_schema must be a schema or database.schema")
    if not isinstance(cfg.exposure_metric, str) or cfg.exposure_metric not in {
        "SCORE", "TOXCONC", "CONC", "CCONC", "NCCONC",
    }:
        raise ValueError("Unsupported exposure metric")
    if not _is_int(cfg.n_controls) or cfg.n_controls < 1:
        raise ValueError("n_controls must be a positive integer")
    if not _is_int(cfg.seed):
        raise ValueError("seed must be an integer")
    if isinstance(cfg.windows, (str, bytes)) or not isinstance(cfg.windows, Iterable):
        raise ValueError("Exposure windows must be positive integers")
    values = tuple(cfg.windows)
    if not values or any(not _is_int(w) or w < 1 for w in values):
        raise ValueError("Exposure windows must be positive integers")
    windows = tuple(sorted({int(w) for w in values}))

    if not isinstance(cfg.static_cols, (tuple, list)):
        raise ValueError("static_cols must be a sequence of column names")
    for name in cfg.static_cols:
        checked_name(name)
    normalized = [name.upper() for name in cfg.static_cols]
    if len(set(normalized)) != len(normalized):
        raise ValueError("Configured static columns must be unique")
    conflicts = sorted(set(normalized) & RESERVED_COLUMNS)
    if conflicts:
        raise ValueError(f"Configured covariates conflict with output columns: {', '.join(conflicts)}")

    output_names = {
        "ALS_RSEI_DML_COHORT", "ALS_RSEI_DML_GEO_YEAR", "ALS_RSEI_DML_BASE",
        "ALS_RSEI_DML_LONG", "ALS_RSEI_DML_QA", *MATCH_WORK_TABLES,
        *(f"ALS_RSEI_DML_{window}Y" for window in windows),
    }
    output_scope = cfg.output_schema.upper().split(".")
    for source in sources:
        parts = source.upper().split(".")
        source_scope = parts[:-1]
        overlap = min(len(source_scope), len(output_scope))
        if parts[-1] in output_names and (
            not source_scope or source_scope[-overlap:] == output_scope[-overlap:]
        ):
            raise ValueError(f"Source table could be overwritten by this build: {source}")
    return windows


def _columns(session, table):
    return {
        c[1:-1] if c.startswith('"') and c.endswith('"') else c.upper()
        for c in session.table(table).columns
    }


def _require_columns(session, table, required, available=None):
    available = _columns(session, table) if available is None else available
    missing = list(dict.fromkeys(name.upper() for name in required if name.upper() not in available))
    if missing:
        raise ValueError(f"{table} is missing required columns: {', '.join(missing)}")


def _case_date_column(session, table):
    available = _columns(session, table)
    _require_columns(session, table, ("PATID",), available)
    if "ALS1DX_DATE" in available:
        return "ALS1DX_DATE"
    if "INDEX_DATE" in available:
        logger.warning("%s has no ALS1DX_DATE; using INDEX_DATE as the ALS1DX date.", table)
        return "INDEX_DATE"
    raise ValueError(f"{table} is missing required columns: ALS1DX_DATE")


def run(session, sql):
    session.sql(sql).collect()


def scalar(session, sql):
    return session.sql(sql).collect()[0][0]


def _adi_lookup_sql(source):
    return f"""
        SELECT DISTINCT TRIM(GEOCODEID::VARCHAR) GEOCODEID,
               TRY_TO_DOUBLE(ADI_NATRANK::VARCHAR) ADI_NATRANK,
               TRY_TO_DOUBLE(ADI_STATERANK::VARCHAR) ADI_STATERANK
        FROM {source}
        WHERE REGEXP_LIKE(TRIM(GEOCODEID::VARCHAR), '^[0-9]{{12}}$')
    """


def build(session, cfg=Config()):
    """Validate inputs and overwrite the configured Snowflake analysis tables."""
    windows = _validate_config(cfg)
    max_window = max(windows)
    logger.info(
        "Validating DML inputs for windows %s, metric %s, %s controls per case.",
        windows, cfg.exposure_metric, cfg.n_controls,
    )
    case_date = _case_date_column(session, cfg.case_table)
    requirements = (
        (cfg.exposure_table, ("PATID", "YEAR", "CENSUS_BLOCK_GROUP",
                              "CHEMICAL_NUMBER", cfg.exposure_metric)),
        (cfg.pat_table, ("PATID", "BIRTH_DATE", "SEX", "INDEX_DATE",
                         "CENSOR_DATE", *cfg.static_cols)),
        (cfg.enrollment_table, ("PATID", "RAW_BASIS", "ENR_START_DATE", "ENR_END_DATE")),
        (cfg.ruca_table, ("TRACTFIPS20", "PRIMARYRUCA")),
        (cfg.adi_table, ("GEOCODEID", "ADI_NATRANK", "ADI_STATERANK")),
    )
    for table, columns in requirements:
        logger.info("Validating columns in %s.", table)
        _require_columns(session, table, columns)

    for source, key, digits in (
        (cfg.ruca_table, "TRACTFIPS20", 11),
        (cfg.adi_table, "GEOCODEID", 12),
    ):
        n_dup = scalar(session, f"""
            SELECT COUNT(*) FROM (
                SELECT TRIM({key}::VARCHAR) K FROM {source}
                WHERE REGEXP_LIKE(TRIM({key}::VARCHAR), '^[0-9]{{{digits}}}$')
                GROUP BY 1 HAVING COUNT(*) > 1
            )
        """)
        if n_dup and key == "GEOCODEID":
            n_conflicts = scalar(session, f"""
                SELECT COUNT(*) FROM (
                    SELECT GEOCODEID FROM ({_adi_lookup_sql(source)}) a
                    GROUP BY GEOCODEID HAVING COUNT(*) > 1
                )
            """)
            if n_conflicts:
                raise ValueError(
                    f"Conflicting ADI rank profiles in {source}: {n_conflicts} geography keys; "
                    "resolve the reference rows before building"
                )
            logger.warning(
                "Collapsing repeated ADI rows for %s geography keys in %s.",
                n_dup, source,
            )
        elif n_dup:
            raise ValueError(f"Duplicate geography keys in {source}: {n_dup}")
    n = scalar(session, f"""
        SELECT COUNT(*) FROM (
            SELECT PATID FROM {cfg.pat_table} GROUP BY PATID HAVING COUNT(*) > 1
        )
    """)
    if n:
        raise ValueError(f"Duplicate PATID records in {cfg.pat_table}: {n}")

    completed = False
    try:
        _build_cohort(session, cfg, case_date)
        _validate_exposure(session, cfg, max_window)
        logger.info("Input validation complete; building tables in %s.", cfg.output_schema)
        result = _build_tables(session, cfg, windows)
        completed = True
        return result
    finally:
        if not completed:
            logger.error("DML build did not complete; output tables may be partially replaced.")


def _build_cohort(session, cfg, case_date):
    out = cfg.output_schema
    logger.info(
        "Creating %s.ALS_RSEI_DML_COHORT (1:%s matching on birth year and sex, seed %s).",
        out, cfg.n_controls, cfg.seed,
    )
    cases, candidates, pairs, round_table = (
        f"{out}.{table}" for table in MATCH_WORK_TABLES
    )
    try:
        _match_controls(session, cfg, case_date, cases, candidates, pairs, round_table)
    finally:
        for table in MATCH_WORK_TABLES:
            run(session, f"DROP TABLE IF EXISTS {out}.{table}")
    _report_cohort(session, cfg, case_date)


def _match_controls(session, cfg, case_date, cases, candidates, pairs, round_table):
    out = cfg.output_schema
    cte = f"""
        WITH all_cases AS (
            SELECT c.PATID, MIN(CAST(c.{case_date} AS DATE)) INDEX_DATE
            FROM {cfg.case_table} c
            WHERE NULLIF(TRIM(c.PATID::VARCHAR), '') IS NOT NULL
              AND c.{case_date} IS NOT NULL
            GROUP BY c.PATID
        ), pat AS (
            SELECT p.PATID, YEAR(CAST(p.BIRTH_DATE AS DATE)) BIRTH_YEAR, p.SEX,
                   CAST(p.INDEX_DATE AS DATE) OBS_START,
                   CAST(p.CENSOR_DATE AS DATE) OBS_END
            FROM {cfg.pat_table} p
        ), geo AS (
            SELECT DISTINCT e.PATID, e.YEAR
            FROM {cfg.exposure_table} e
            WHERE e.CENSUS_BLOCK_GROUP IS NOT NULL
        )"""
    run(session, f"""
        CREATE OR REPLACE TABLE {cases} AS
        {cte}
        SELECT c.PATID MATCH_ID, c.INDEX_DATE, p.BIRTH_YEAR, p.SEX
        FROM all_cases c
        JOIN pat p ON c.PATID=p.PATID
        JOIN geo g ON g.PATID=c.PATID AND g.YEAR=YEAR(c.INDEX_DATE)-1
    """)
    run(session, f"""
        CREATE OR REPLACE TABLE {candidates} AS
        {cte}, eligible_cases AS (
            SELECT MATCH_ID PATID, INDEX_DATE, BIRTH_YEAR, SEX FROM {cases}
        ), control_pool AS (
            SELECT p.* FROM pat p
            WHERE p.BIRTH_YEAR IS NOT NULL AND p.SEX IS NOT NULL
              AND NOT EXISTS (SELECT 1 FROM all_cases c WHERE c.PATID=p.PATID)
        )
        SELECT ec.PATID MATCH_ID, cp.PATID,
               HASH({cfg.seed}, cp.PATID, ec.PATID) RAND_KEY
        FROM eligible_cases ec
        JOIN control_pool cp
          ON cp.BIRTH_YEAR=ec.BIRTH_YEAR AND cp.SEX=ec.SEX
        JOIN geo g ON g.PATID=cp.PATID AND g.YEAR=YEAR(ec.INDEX_DATE)-1
        WHERE (cp.OBS_START IS NULL OR cp.OBS_START <= ec.INDEX_DATE)
          AND (cp.OBS_END IS NULL OR cp.OBS_END >= ec.INDEX_DATE)
    """)
    run(session, f"""
        CREATE OR REPLACE TABLE {pairs} AS
        SELECT MATCH_ID, PATID, CAST(0 AS INTEGER) AS MATCH_ROUND
        FROM {candidates} WHERE 1=0
    """)
    # Each round, every unused candidate control proposes to one random case
    # that still needs controls; each case accepts up to its remaining need.
    # Rejected controls stay available, so rounds continue until no short case
    # has an unused eligible control. Every round with an open candidate
    # accepts at least one pair, so the loop terminates.
    n_round = 0
    while True:
        n_round += 1
        run(session, f"""
            CREATE OR REPLACE TABLE {round_table} AS
            WITH filled AS (
                SELECT MATCH_ID, COUNT(*) N FROM {pairs} GROUP BY MATCH_ID
            ), open_cases AS (
                SELECT c.MATCH_ID, {cfg.n_controls} - COALESCE(f.N, 0) NEED
                FROM {cases} c LEFT JOIN filled f ON f.MATCH_ID=c.MATCH_ID
                WHERE COALESCE(f.N, 0) < {cfg.n_controls}
            ), open_candidates AS (
                SELECT k.MATCH_ID, k.PATID, k.RAND_KEY, o.NEED
                FROM {candidates} k
                JOIN open_cases o ON o.MATCH_ID=k.MATCH_ID
                WHERE NOT EXISTS (SELECT 1 FROM {pairs} m WHERE m.PATID=k.PATID)
            ), proposals AS (
                SELECT * FROM (
                    SELECT oc.MATCH_ID, oc.PATID, oc.RAND_KEY, oc.NEED,
                           ROW_NUMBER() OVER (
                               PARTITION BY oc.PATID ORDER BY oc.RAND_KEY, oc.MATCH_ID
                           ) RN_CONTROL
                    FROM open_candidates oc
                ) WHERE RN_CONTROL=1
            )
            SELECT MATCH_ID, PATID FROM (
                SELECT p.MATCH_ID, p.PATID, p.NEED,
                       ROW_NUMBER() OVER (
                           PARTITION BY p.MATCH_ID ORDER BY p.RAND_KEY, p.PATID
                       ) RN_CASE
                FROM proposals p
            ) WHERE RN_CASE <= NEED
        """)
        n_accepted = scalar(session, f"SELECT COUNT(*) FROM {round_table}")
        if not n_accepted:
            break
        logger.info("Matching round %s: %s controls assigned.", n_round, n_accepted)
        run(session, f"""
            INSERT INTO {pairs} (MATCH_ID, PATID, MATCH_ROUND)
            SELECT MATCH_ID, PATID, {n_round} AS MATCH_ROUND FROM {round_table}
        """)

    run(session, f"""
        CREATE OR REPLACE TABLE {out}.ALS_RSEI_DML_COHORT AS
        SELECT c.MATCH_ID AS PATID, c.MATCH_ID, c.INDEX_DATE, 1 AS ALS
        FROM {cases} c
        UNION ALL
        SELECT m.PATID, m.MATCH_ID, c.INDEX_DATE, 0 AS ALS
        FROM {pairs} m JOIN {cases} c ON c.MATCH_ID=m.MATCH_ID
    """)


def _report_cohort(session, cfg, case_date):
    out = cfg.output_schema
    n_cases_src = scalar(session, f"""
        SELECT COUNT(DISTINCT PATID) FROM {cfg.case_table}
        WHERE NULLIF(TRIM(PATID::VARCHAR), '') IS NOT NULL AND {case_date} IS NOT NULL
    """)
    stats = session.sql(f"""
        SELECT COALESCE(SUM(IFF(ALS=1, 1, 0)), 0),
               COALESCE(SUM(IFF(ALS=0, 1, 0)), 0)
        FROM {out}.ALS_RSEI_DML_COHORT
    """).collect()[0]
    n_cases, n_controls = stats[0], stats[1]
    n_short = scalar(session, f"""
        SELECT COUNT(*) FROM (
            SELECT MATCH_ID FROM {out}.ALS_RSEI_DML_COHORT
            GROUP BY MATCH_ID HAVING SUM(IFF(ALS=0, 1, 0)) < {cfg.n_controls}
        )
    """)
    if n_cases_src > n_cases:
        logger.warning(
            "Excluding %s ALS cases not in %s or without geography in INDEX_YEAR-1.",
            n_cases_src - n_cases, cfg.pat_table,
        )
    if n_short:
        logger.warning(
            "%s cases matched fewer than %s controls (no unused eligible controls remained).",
            n_short, cfg.n_controls,
        )
    logger.info("Matched cohort: %s cases, %s controls.", n_cases, n_controls)
    if not n_cases:
        raise ValueError("No eligible ALS cases; check case, PAT_TABLE1, and exposure inputs")


def _validate_exposure(session, cfg, max_window):
    cohort = f"{cfg.output_schema}.ALS_RSEI_DML_COHORT"
    preindex = (
        f"e.YEAR BETWEEN YEAR(c.INDEX_DATE)-{max_window} AND YEAR(c.INDEX_DATE)-1"
    )
    n_invalid_geo = scalar(session, f"""
        SELECT COUNT(*) FROM (
            SELECT e.PATID, e.YEAR
            FROM {cfg.exposure_table} e JOIN {cohort} c ON e.PATID=c.PATID
            WHERE {preindex} AND e.CENSUS_BLOCK_GROUP IS NOT NULL
              AND NOT REGEXP_LIKE(TRIM(e.CENSUS_BLOCK_GROUP::VARCHAR), '^[0-9]{{11,12}}$')
            GROUP BY e.PATID, e.YEAR
        )
    """)
    if n_invalid_geo:
        raise ValueError(
            f"{n_invalid_geo} person-years have invalid CENSUS_BLOCK_GROUP values; "
            "expected 11 or 12 FIPS digits with leading zeros preserved"
        )
    n_multi_geo = scalar(session, f"""
        SELECT COUNT(*) FROM (
            SELECT e.PATID, e.YEAR
            FROM {cfg.exposure_table} e JOIN {cohort} c ON e.PATID=c.PATID
            WHERE {preindex} AND e.CENSUS_BLOCK_GROUP IS NOT NULL
            GROUP BY e.PATID, e.YEAR
            HAVING COUNT(DISTINCT TRIM(e.CENSUS_BLOCK_GROUP::VARCHAR)) > 1
        )
    """)
    if n_multi_geo:
        raise ValueError(
            f"Multiple CBGs in {n_multi_geo} person-years; "
            "resolve residential mobility within year upstream"
        )
    n_multi = scalar(session, f"""
        SELECT COUNT(*) FROM (
            SELECT e.PATID, e.YEAR, e.CHEMICAL_NUMBER
            FROM {cfg.exposure_table} e JOIN {cohort} c ON e.PATID=c.PATID
            WHERE {preindex} AND e.CHEMICAL_NUMBER IS NOT NULL
            GROUP BY e.PATID, e.YEAR, e.CHEMICAL_NUMBER HAVING COUNT(*) > 1
        )
    """)
    if n_multi:
        raise ValueError(
            f"{n_multi} duplicated person-year-chemical keys; "
            "resolve duplicate exposures upstream"
        )
    n_no_geo = scalar(session, f"""
        SELECT COUNT(*)
        FROM {cfg.exposure_table} e JOIN {cohort} c ON e.PATID=c.PATID
        WHERE {preindex} AND e.CHEMICAL_NUMBER IS NOT NULL
          AND e.{cfg.exposure_metric} IS NOT NULL
          AND e.CENSUS_BLOCK_GROUP IS NULL
    """)
    if n_no_geo:
        logger.warning(
            "Excluding %s exposure rows without a linked census geography.", n_no_geo
        )


def _build_tables(session, cfg, windows):
    out = cfg.output_schema
    cohort = f"{out}.ALS_RSEI_DML_COHORT"
    max_window = max(windows)
    logger.info("Creating %s.ALS_RSEI_DML_GEO_YEAR.", out)
    run(session, f"""
        CREATE OR REPLACE TABLE {out}.ALS_RSEI_DML_GEO_YEAR AS
        WITH unique_geo AS (
          SELECT e.PATID, e.YEAR,
                 MIN(TRIM(e.CENSUS_BLOCK_GROUP::VARCHAR)) CENSUS_BLOCK_GROUP,
                 COUNT(DISTINCT TRIM(e.CENSUS_BLOCK_GROUP::VARCHAR)) N_CBG_YEAR
          FROM {cfg.exposure_table} e
          JOIN {cohort} c ON e.PATID=c.PATID
          WHERE e.YEAR BETWEEN YEAR(c.INDEX_DATE)-{max_window}
                           AND YEAR(c.INDEX_DATE)-1
            AND e.CENSUS_BLOCK_GROUP IS NOT NULL
          GROUP BY e.PATID,e.YEAR
        ), dual_lis_years AS (
          SELECT DISTINCT g.PATID, g.YEAR
          FROM unique_geo g
          JOIN {cfg.enrollment_table} en ON en.PATID=g.PATID
          WHERE UPPER(TRIM(en.RAW_BASIS::VARCHAR)) IN ('DUAL', 'LIS')
            AND CAST(en.ENR_START_DATE AS DATE) <= DATE_FROM_PARTS(g.YEAR, 12, 31)
            AND (en.ENR_END_DATE IS NULL
                 OR CAST(en.ENR_END_DATE AS DATE) >= DATE_FROM_PARTS(g.YEAR, 1, 1))
        )
        SELECT g.PATID, g.YEAR, g.CENSUS_BLOCK_GROUP, g.N_CBG_YEAR,
               IFF(d.PATID IS NULL, 0, 1) DUAL_LIS,
               TRY_TO_DOUBLE(r.PRIMARYRUCA::VARCHAR) RUCA,
               TRY_TO_DOUBLE(a.ADI_NATRANK::VARCHAR) ADI_NATRANK,
               TRY_TO_DOUBLE(a.ADI_STATERANK::VARCHAR) ADI_STATERANK,
               IFF(r.TRACTFIPS20 IS NULL, 0, 1) RUCA_MATCHED,
               IFF(a.GEOCODEID IS NULL, 0, 1) ADI_MATCHED
        FROM unique_geo g
        LEFT JOIN dual_lis_years d ON d.PATID=g.PATID AND d.YEAR=g.YEAR
        LEFT JOIN {cfg.ruca_table} r
          ON TRIM(r.TRACTFIPS20::VARCHAR) = SUBSTR(g.CENSUS_BLOCK_GROUP,1,11)
         AND LENGTH(g.CENSUS_BLOCK_GROUP) IN (11,12)
        LEFT JOIN ({_adi_lookup_sql(cfg.adi_table)}) a
          ON a.GEOCODEID = g.CENSUS_BLOCK_GROUP
         AND LENGTH(g.CENSUS_BLOCK_GROUP) = 12
    """)

    static_select = "".join(f", s.{x} AS {x}" for x in cfg.static_cols)
    logger.info("Creating %s.ALS_RSEI_DML_BASE.", out)
    run(session, f"""
        CREATE OR REPLACE TABLE {out}.ALS_RSEI_DML_BASE AS
        SELECT c.PATID, c.MATCH_ID, c.INDEX_DATE,
               YEAR(c.INDEX_DATE) INDEX_YEAR, c.ALS
               {static_select},
               YEAR(c.INDEX_DATE)-1-YEAR(CAST(s.BIRTH_DATE AS DATE)) AS AGE,
               COALESCE(g.DUAL_LIS, 0) AS DUAL_LIS,
               g.CENSUS_BLOCK_GROUP AS BASELINE_CBG,
               g.RUCA, g.ADI_NATRANK, g.ADI_STATERANK,
               g.RUCA_MATCHED, g.ADI_MATCHED
        FROM {cohort} c
        LEFT JOIN {cfg.pat_table} s ON c.PATID=s.PATID
        LEFT JOIN {out}.ALS_RSEI_DML_GEO_YEAR g
          ON c.PATID=g.PATID AND g.YEAR=YEAR(c.INDEX_DATE)-1
    """)

    for k in windows:
        # Patient-year geography completeness is checked separately from
        # chemical coverage; one observed chemical isn't proof all are covered.
        # Coverage is computed from all source rows, not the selected chemical.
        logger.info("Creating %s.ALS_RSEI_DML_%sY.", out, k)
        run(session, f"""
            CREATE OR REPLACE TABLE {out}.ALS_RSEI_DML_{k}Y AS
            WITH geo_years AS (
                SELECT g.PATID, COUNT(DISTINCT g.YEAR) N_GEO_YEARS,
                       COUNT(DISTINCT g.CENSUS_BLOCK_GROUP) N_CBG,
                       COUNT(DISTINCT SUBSTR(g.CENSUS_BLOCK_GROUP,1,2)) N_STATES,
                       MIN(g.CENSUS_BLOCK_GROUP) MIN_CBG,
                       MIN(SUBSTR(g.CENSUS_BLOCK_GROUP,1,2)) MIN_STATE_FIPS,
                       MIN(IFF(g.YEAR=b.INDEX_YEAR-1, g.CENSUS_BLOCK_GROUP, NULL)) INDEX_MINUS_1_CBG,
                       MIN(IFF(g.YEAR=b.INDEX_YEAR-1,
                               SUBSTR(g.CENSUS_BLOCK_GROUP,1,2), NULL)) INDEX_MINUS_1_STATE,
                       SUM(g.DUAL_LIS) DUAL_LIS_YEARS,
                       MAX(g.DUAL_LIS) DUAL_LIS_ANY
                FROM {out}.ALS_RSEI_DML_GEO_YEAR g
                JOIN {out}.ALS_RSEI_DML_BASE b ON b.PATID=g.PATID
                WHERE g.YEAR BETWEEN b.INDEX_YEAR-{k} AND b.INDEX_YEAR-1
                GROUP BY g.PATID
            ), chemical_years AS (
                SELECT e.PATID, e.CHEMICAL_NUMBER,
                       COUNT(DISTINCT e.YEAR) N_CHEM_YEARS,
                       AVG(e.{cfg.exposure_metric}) EXPOSURE_MEAN,
                       SUM(e.{cfg.exposure_metric}) EXPOSURE_SUM,
                       MIN(e.{cfg.exposure_metric}) EXPOSURE_MIN,
                       MAX(e.{cfg.exposure_metric}) EXPOSURE_MAX,
                       STDDEV_SAMP(e.{cfg.exposure_metric}) EXPOSURE_SD_WITHIN
                FROM {cfg.exposure_table} e
                JOIN {out}.ALS_RSEI_DML_BASE b ON b.PATID=e.PATID
                JOIN {out}.ALS_RSEI_DML_GEO_YEAR g
                  ON e.PATID=g.PATID AND e.YEAR=g.YEAR
                 AND TRIM(e.CENSUS_BLOCK_GROUP::VARCHAR) = g.CENSUS_BLOCK_GROUP
                WHERE e.YEAR BETWEEN b.INDEX_YEAR-{k} AND b.INDEX_YEAR-1
                  AND e.CHEMICAL_NUMBER IS NOT NULL
                  AND e.{cfg.exposure_metric} IS NOT NULL
                GROUP BY 1,2
            )
            SELECT b.*, c.CHEMICAL_NUMBER, c.EXPOSURE_MEAN,
                   c.EXPOSURE_SUM, c.EXPOSURE_MIN, c.EXPOSURE_MAX,
                   c.EXPOSURE_SD_WITHIN, c.N_CHEM_YEARS,
                   g.N_GEO_YEARS, g.N_CBG, g.N_STATES,
                   g.MIN_CBG, g.MIN_STATE_FIPS,
                   g.INDEX_MINUS_1_CBG, g.INDEX_MINUS_1_STATE,
                   g.DUAL_LIS_YEARS, g.DUAL_LIS_ANY,
                   {k} AS WINDOW_YEARS
            FROM {out}.ALS_RSEI_DML_BASE b
            JOIN geo_years g ON b.PATID=g.PATID
            JOIN chemical_years c ON b.PATID=c.PATID
            WHERE g.N_GEO_YEARS={k} AND c.N_CHEM_YEARS={k}
        """)

    unions = " UNION ALL ".join(
        f"SELECT * FROM {out}.ALS_RSEI_DML_{k}Y" for k in windows)
    logger.info("Creating %s.ALS_RSEI_DML_LONG.", out)
    run(session, f"CREATE OR REPLACE TABLE {out}.ALS_RSEI_DML_LONG AS {unions}")

    logger.info("Creating %s.ALS_RSEI_DML_QA.", out)
    run(session, f"""
        CREATE OR REPLACE TABLE {out}.ALS_RSEI_DML_QA AS
        SELECT WINDOW_YEARS, CHEMICAL_NUMBER, COUNT(*) N,
               COALESCE(COUNT_IF(ALS=1),0) N_CASES,
               COALESCE(COUNT_IF(ALS=0),0) N_CONTROLS,
               COALESCE(COUNT_IF(EXPOSURE_MEAN=0),0) N_ZERO,
               COALESCE(COUNT_IF(EXPOSURE_MEAN>0),0) N_POSITIVE,
               COUNT(DISTINCT EXPOSURE_MEAN) N_UNIQUE_EXPOSURE,
               APPROX_PERCENTILE(EXPOSURE_MEAN,0.5) MEDIAN_EXPOSURE,
               APPROX_PERCENTILE(EXPOSURE_MEAN,0.99) P99_EXPOSURE,
               COUNT(DISTINCT INDEX_MINUS_1_CBG) N_CBG_GROUPS,
               COUNT(DISTINCT INDEX_MINUS_1_STATE) N_STATE_GROUPS,
               COALESCE(COUNT_IF(RUCA_MATCHED=1),0) N_RUCA_MATCHED,
               COALESCE(COUNT_IF(ADI_MATCHED=1),0) N_ADI_MATCHED,
               COALESCE(COUNT_IF(RUCA IS NULL),0) N_RUCA_MISSING,
               COALESCE(COUNT_IF(ADI_NATRANK IS NULL),0) N_ADI_NATRANK_MISSING,
               COALESCE(COUNT_IF(ADI_STATERANK IS NULL),0) N_ADI_STATERANK_MISSING,
               STDDEV_SAMP(EXPOSURE_MEAN) SD_EXPOSURE,
               MIN(EXPOSURE_MEAN) MIN_EXPOSURE,
               MAX(EXPOSURE_MEAN) MAX_EXPOSURE,
               COUNT(DISTINCT PATID) N_PATIENTS
        FROM {out}.ALS_RSEI_DML_LONG
        GROUP BY 1,2
    """)
    logger.info("DML output tables completed in %s.", out)
    return {
        "base": f"{out}.ALS_RSEI_DML_BASE",
        "long": f"{out}.ALS_RSEI_DML_LONG",
        "qa": f"{out}.ALS_RSEI_DML_QA",
    }


if __name__ == "__main__":
    env.load_env()
    session = conn.get_snow_conn(conn.load_snowcfg("ID"))
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")
    print(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_WAREHOUSE()").collect())
    
    try:
        build(session, Config())
    finally:
        logger.info("Closing Snowflake session.")
        session.close()
    
