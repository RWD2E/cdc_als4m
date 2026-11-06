import logging
import os
from snowflake.snowpark import DataFrame, Session
from snowflake.snowpark.functions import (
    col,
    coalesce,
    dateadd,
    datediff,
    listagg,
    lit,
    max as s_max,
    min as s_min,
    row_number,
    when,
    count_distinct,
    concat,
    round as s_round,
)
from snowflake.snowpark.window import Window
from config import env, conn

logger = logging.getLogger(__name__)


def _build_cms_enrollment_rollup(session: Session):
    """
    Build one CMS enrollment row per patient with earliest start and latest end.
    """
    cms_tbl = session.table("GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT")
    cms_cols = {c.upper() for c in cms_tbl.columns}

    if "PATID" not in cms_cols:
        raise ValueError("PATID column not found in GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT")

    start_col = next(
        (c for c in ["CMS_ENR_START_DT", "ENR_START_DATE", "ENR_START_DT"] if c in cms_cols),
        None,
    )
    end_col = next(
        (c for c in ["CMS_ENR_END_DT", "ENR_END_DATE", "ENR_END_DT"] if c in cms_cols),
        None,
    )

    if start_col is None or end_col is None:
        raise ValueError(
            "Enrollment start/end columns not found in GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT"
        )

    return (
        cms_tbl
        .select(
            col("PATID").alias("PATID"),
            col(start_col).cast("DATE").alias("CMS_ENR_START_DT"),
            col(end_col).cast("DATE").alias("CMS_ENR_END_DT"),
        )
        .filter(col("PATID").is_not_null())
        .group_by("PATID")
        .agg(
            s_min(col("CMS_ENR_START_DT")).alias("CMS_ENR_START_DT"),
            s_max(col("CMS_ENR_END_DT")).alias("CMS_ENR_END_DT"),
        )
    )

def _build_log_uniq(session, log_tbl):
    phemap = session.create_dataframe(
        [
            ("ALS","DX"),
            ("riluzole","RX"),
            ("edaravone","RX"),
            ("tofersen","RX")
        ],
        schema = ["PHE_TYPE","PHE_GRP"]
    )
    log_tbl = log_tbl.join(phemap, ["PHE_TYPE"], how="inner")
    return (
        log_tbl
            .select("PATID", "PHE_TYPE", "PHE_GRP", "CD_DATE", "PROVIDERID", "SITE")
            .drop_duplicates()
    )

def _build_init_dx(log_uniq):
    w_patid = Window.partition_by(col("PATID")).order_by(
        col("CD_DATE"),
        # Keep patients with missing provider IDs, but prefer a populated provider
        # when multiple ALS records exist on the same first diagnosis date.
        when(col("PROVIDERID").is_not_null(), lit(0)).otherwise(lit(1)),
        col("SITE"),
        col("PROVIDERID"),
    )

    return (
        log_uniq
        .filter(col("PHE_TYPE") == "ALS")
        .filter(col("CD_DATE").is_not_null())
        .with_column("rn", row_number().over(w_patid))
        .filter(col("rn") == 1)
        .select(
            col("PATID").alias("PATID"),
            col("CD_DATE").alias("ALS1DX_DATE"),
            col("SITE").alias("INDEX_SRC"),
            col("PROVIDERID").alias("ALS1DX_PROVIDERID"),
        )
    )

def _build_als_pt(log_uniq):
    """
    Build one ALS summary row per patient.

    Output includes:
      - earliest event type and date
      - total distinct event types
      - total distinct event dates
      - number of distinct dates for the earliest event type
      - compact event summary string, such as DX3|MED2
    """

    base = (
        log_uniq
        .select(
            col("PATID"),
            col("PHE_TYPE"),
            col("CD_DATE").cast("DATE").alias("CD_DATE"),
        )
        .filter(col("PATID").is_not_null())
        .filter(col("PHE_TYPE").is_not_null())
        .filter(col("CD_DATE").is_not_null())
        .distinct()
    )

    w_patid = Window.partition_by("PATID")
    w_patid_type = Window.partition_by( "PATID","PHE_TYPE")
    w_patid_order = Window.partition_by("PATID").order_by(col("CD_DATE"),col("PHE_TYPE"))

    event_rows = (
        base
        .with_column(
            "distinct_event_cnt",
            count_distinct(col("PHE_TYPE")).over(w_patid),
        )
        .with_column(
            "distinct_date_cnt",
            count_distinct(col("CD_DATE")).over(w_patid),
        )
        .with_column(
            "event_distinct_date_cnt",
            count_distinct(col("CD_DATE")).over(w_patid_type),
        )
        .with_column(
            "rn",
            row_number().over(w_patid_order),
        )
        .alias("events")
    )

    # Retain only patients identified by _build_init_dx().
    init_dx = _build_init_dx(log_uniq)
    grp_by_type = (
        event_rows
        .join(
            init_dx,
            event_rows["PATID"] == init_dx["PATID"],
            "inner",
        )
        .select(
            event_rows["PATID"].alias("PATID"),
            init_dx["ALS1DX_DATE"].alias("ALS1DX_DATE"),
            init_dx["INDEX_SRC"].alias("INDEX_SRC"),
            init_dx["ALS1DX_PROVIDERID"].alias("ALS1DX_PROVIDERID"),
            event_rows["PHE_TYPE"].alias("PHE_TYPE"),
            event_rows["CD_DATE"].alias("CD_DATE"),
            event_rows["distinct_event_cnt"].alias("distinct_event_cnt"),
            event_rows["distinct_date_cnt"].alias("distinct_date_cnt"),
            event_rows["event_distinct_date_cnt"].alias("event_distinct_date_cnt"),
            event_rows["rn"].alias("rn"),
        )
        .alias("grp")
    )

    # Generate exactly one row per patient/event type before LISTAGG.
    event_type_summary = (
        grp_by_type
        .select(
            col("PATID"),
            col("PHE_TYPE"),
            col("event_distinct_date_cnt"),
        )
        .distinct()
        .with_column(
            "event_summary",
            concat(
                col("PHE_TYPE"),
                col("event_distinct_date_cnt").cast("STRING"),
            ),
        )
    )

    event_str_df = (
        event_type_summary
        .group_by("PATID")
        .agg(
            listagg(col("event_summary"),"|")
            .within_group(col("event_summary"))
            .alias("event_str")
        )
        .alias("event_summary")
    )

    als_elig = (
        grp_by_type
        .filter(col("rn") == 1)
        .select(
            col("PATID"),
            col("ALS1DX_DATE"),
            col("INDEX_SRC"),
            col("ALS1DX_PROVIDERID"),
            col("PHE_TYPE").alias("PHE1_TYPE"),
            col("CD_DATE").alias("PHE1_DATE"),
            col("distinct_event_cnt"),
            col("distinct_date_cnt"),
            col("event_distinct_date_cnt"),
        )
        .filter(col("distinct_date_cnt") > 1)
        .alias("als_elig")
    )

    return (
        als_elig
        .join(
            event_str_df,
            als_elig["PATID"] == event_str_df["PATID"],
            "inner",
        )
        .select(
            als_elig["PATID"].alias("PATID"),
            als_elig["ALS1DX_DATE"],
            als_elig["INDEX_SRC"],
            als_elig["ALS1DX_PROVIDERID"],
            als_elig["PHE1_TYPE"],
            als_elig["PHE1_DATE"],
            als_elig["distinct_event_cnt"],
            als_elig["distinct_date_cnt"],
            als_elig["event_distinct_date_cnt"],
            event_str_df["event_str"],
        )
    )


def _add_mdt_base_n(
    als_cases: DataFrame,
    log_tbl: DataFrame,
    prvdr_long: DataFrame,
    mdt_lookback_days: int,
) -> DataFrame:
    """Flag any neurology event in the inclusive pre-diagnosis lookback."""
    neurology_providers = (
        prvdr_long
        .filter(col("MDT_DOMAIN") == "N")
        .select("PROVIDERID", "SITE")
        .distinct()
    )
    events = log_tbl.select(
        "PATID", "PROVIDERID", "SITE",
        col("CD_DATE").cast("DATE").alias("CD_DATE"),
    )
    patients = als_cases.select("PATID", "ALS1DX_DATE")
    baseline_events = (
        patients
        .join(
            events,
            (patients["PATID"] == events["PATID"]) &
            events["CD_DATE"].between(
                dateadd("day", lit(-mdt_lookback_days), patients["ALS1DX_DATE"]),
                patients["ALS1DX_DATE"],
            ),
            "inner",
        )
        .select(
            events["PATID"].alias("PATID"),
            events["PROVIDERID"].alias("PROVIDERID"),
            events["SITE"].alias("SITE"),
        )
    )
    evidence = (
        baseline_events
        .join(
            neurology_providers,
            (baseline_events["PROVIDERID"] == neurology_providers["PROVIDERID"]) &
            (baseline_events["SITE"] == neurology_providers["SITE"]),
            "inner",
        )
        .select(baseline_events["PATID"].alias("MDT_PATID"))
        .distinct()
        .with_column("MDT_BASE_N", lit(1))
    )
    return (
        als_cases
        .join(evidence, als_cases["PATID"] == evidence["MDT_PATID"], "left")
        .select(
            *[als_cases[name] for name in als_cases.columns],
            coalesce(evidence["MDT_BASE_N"], lit(0)).alias("MDT_BASE_N"),
        )
    )


def build_als_case_table(session: Session, mdt_lookback_days: int = 365):
    """
    Build ALS_CASE with MDT_BASE_N in place of the first provider's MDT_DOMAIN.

    MDT_BASE_N is 1 when any ALS_DXRXPX_LONG diagnosis, procedure, or
    prescription has a provider with MDT_DOMAIN = 'N' at the same SITE,
    within [ALS1DX_DATE - mdt_lookback_days, ALS1DX_DATE]; otherwise 0.
    A zero-day lookback includes only the diagnosis date.
    """
    if (
        isinstance(mdt_lookback_days, bool)
        or not isinstance(mdt_lookback_days, int)
        or mdt_lookback_days < 0
    ):
        raise ValueError("mdt_lookback_days must be a non-negative integer.")

    logger.info("Building ALS_CASE with MDT_BASE_N lookback of %s days.", mdt_lookback_days)

    log_tbl = (
        session.table("ALS_DXRXPX_LONG")
        # .filter(col("SITE") != 'CMS') # remove CMS
    )
    prvdr_long = session.table("PRVDR_LONG")
    log_uniq = _build_log_uniq(session, log_tbl)
    als_tbl1 = _add_mdt_base_n(
        _build_als_pt(log_uniq), log_tbl, prvdr_long, mdt_lookback_days,
    )
    w_patid = Window.partition_by("PATID").order_by(col("ALS1DX_DATE"))

    # ALS case table
    als_case_table1 = (
        als_tbl1
        .with_column("rn", row_number().over(w_patid))
        .filter(col("rn") == 1)
        .select(
            als_tbl1["PATID"].alias("PATID"),
            als_tbl1["ALS1DX_DATE"],
            als_tbl1["INDEX_SRC"],
            als_tbl1["ALS1DX_PROVIDERID"],
            als_tbl1["MDT_BASE_N"],
            als_tbl1["PHE1_TYPE"],
            als_tbl1["PHE1_DATE"],
            als_tbl1["distinct_event_cnt"],
            als_tbl1["distinct_date_cnt"],
            als_tbl1["event_distinct_date_cnt"],
            als_tbl1["event_str"],
        )
    )
    als_case_table1.write.mode("overwrite").save_as_table("ALS_CASE")
    logger.info("✅ ALS_CASE table created with {} rows.".format(als_case_table1.count()))


def main(mdt_lookback_days: int = 365):
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")

    print(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_WAREHOUSE()").collect())
    build_als_case_table(session, mdt_lookback_days=mdt_lookback_days)

    # attach demographic info
    pat_tbl1 = session.table("PAT_TABLE1")
    als_case_table1 = session.table("ALS_CASE")
    cms_enr_rollup = _build_cms_enrollment_rollup(session)
    als_w_demo = (
        als_case_table1
        .join(
            pat_tbl1, als_case_table1["PATID"] == pat_tbl1["PATID"], 
            "inner"
        )
        .join(
            cms_enr_rollup,
            als_case_table1["PATID"] == cms_enr_rollup["PATID"],
            "left",
        )
        .select(
            als_case_table1["PATID"].alias("PATID"),
            als_case_table1["ALS1DX_DATE"].alias("INDEX_DATE"),
            als_case_table1["INDEX_SRC"].alias("INDEX_SRC"),
            als_case_table1["MDT_BASE_N"].alias("MDT_BASE_N"),
            als_case_table1["event_str"].alias("ALS_CPHETY"),
            pat_tbl1["birth_date"].alias("BIRTH_DATE"),
            s_round(
                datediff(
                    "day",
                    pat_tbl1["BIRTH_DATE"].cast("DATE"),
                    als_case_table1["ALS1DX_DATE"].cast("DATE"),
                )
                / lit(365.25)
            ).cast("INTEGER").alias("AGE_AT_INDEX"),
            pat_tbl1["index_date"].alias("EHR_START_DATE"),
            pat_tbl1["age_at_index"].alias("AGE_AT_EHR_START"),
            pat_tbl1["agegrp_at_index"].alias("AGEGRP_AT_EHR_START"),
            pat_tbl1["sex"].alias("SEX"),
            pat_tbl1["race"].alias("RACE"),
            pat_tbl1["hispanic"].alias("HISPANIC"),
            pat_tbl1["index_enc_type"].alias("INDEX_ENC_TYPE"),
            pat_tbl1["censor_date"].alias("CENSOR_DATE"),
            pat_tbl1["censor_year"].alias("CENSOR_YEAR"),
            pat_tbl1["death_ind"].alias("DEATH_IND"),
            pat_tbl1["src"].alias("CENSOR_SRC"),
            cms_enr_rollup["CMS_ENR_START_DT"].alias("CMS_ENR_START_DT"),
            cms_enr_rollup["CMS_ENR_END_DT"].alias("CMS_ENR_END_DT"),
        )
        .filter(
            (als_case_table1["ALS1DX_DATE"] > pat_tbl1["birth_date"]) &
            (pat_tbl1["age_at_index"] >= 18)
        )
    )
    als_w_demo.write.mode("overwrite").save_as_table("ALS_CASE_TABLE1")
    logger.info("✅ ALS_CASE_TABLE1 table created with {} rows.".format(als_w_demo.count()))

if __name__ == "__main__":
    main(mdt_lookback_days = 730)


# create or replace table ALS_CASE_TABLE1_CCW as
# select * from ALS_CASE_TABLE1
# where CMS_ENR_START_DT is not null and MDT_BASE_N = 1
# ;