import logging
import os
from snowflake.snowpark import Session
from snowflake.snowpark.functions import (
    col,
    count,
    datediff,
    listagg,
    lit,
    max as s_max,
    min as s_min,
    regexp_replace,
    row_number,
    when,
    count_distinct,
    concat
)
from snowflake.snowpark.window import Window
from config import env, conn

logger = logging.getLogger(__name__)

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
            .select("PATID", "PHE_TYPE", "PHE_GRP", "CD_DATE", "SITE")
            .drop_duplicates()
    )

def _build_init_dx(log_uniq):
    w_patid = Window.partition_by(col("PATID"))
    return (
        log_uniq
        .filter(col("PHE_TYPE") == "ALS")
        .filter(col("CD_DATE").is_not_null())
        .with_column(
            "ALS1DX_DATE",
            s_min(col("CD_DATE")).over(w_patid),
        )
        .filter(col("CD_DATE") == col("ALS1DX_DATE"))
        .select(
            col("PATID").alias("PATID"),
            col("ALS1DX_DATE").alias("ALS1DX_DATE"),
            col("SITE").alias("INDEX_SRC")
        )
        .distinct()
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
            als_elig["PHE1_TYPE"],
            als_elig["PHE1_DATE"],
            als_elig["distinct_event_cnt"],
            als_elig["distinct_date_cnt"],
            als_elig["event_distinct_date_cnt"],
            event_str_df["event_str"],
        )
    )


def build_als_case_table(session: Session):
    """
    Build ALS case table
    """
    log_tbl = (
        session.table("ALS_DXRXPX_LONG")
        .filter(col("SITE") != 'CMS') # remove CMS
    )
    log_uniq = _build_log_uniq(session, log_tbl)
    als_tbl1 = _build_als_pt(log_uniq)
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


def main():
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")

    print(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_WAREHOUSE()").collect())
    build_als_case_table(session)

    # attach demographic info
    pat_tbl1 = session.table("PAT_TABLE1")
    als_case_table1 = session.table("ALS_CASE")
    als_w_demo = (
        als_case_table1.join(
            pat_tbl1, als_case_table1["PATID"] == pat_tbl1["PATID"], 
            "inner"
        )
        .select(
            als_case_table1["PATID"].alias("PATID"),
            als_case_table1["ALS1DX_DATE"].alias("ALS1DX_DATE"),
            als_case_table1["INDEX_SRC"].alias("INDEX_SRC"),
            als_case_table1["event_str"].alias("ALS_CPHETY"),
            pat_tbl1["index_date"].alias("ENTRY_DATE"),
            pat_tbl1["age_at_index"].alias("AGE_AT_ENTRY"),
            pat_tbl1["agegrp_at_index"].alias("AGEGRP_AT_ENTRY"),
            pat_tbl1["sex"].alias("SEX"),
            pat_tbl1["race"].alias("RACE"),
            pat_tbl1["hispanic"].alias("HISPANIC"),
            pat_tbl1["index_enc_type"].alias("INDEX_ENC_TYPE"),
            pat_tbl1["censor_date"].alias("CENSOR_DATE"),
            pat_tbl1["censor_year"].alias("CENSOR_YEAR"),
            pat_tbl1["death_ind"].alias("DEATH_IND"),
            pat_tbl1["src"].alias("SRC"),
        )
    )
    als_w_demo.write.mode("overwrite").save_as_table("ALS_CASE_TABLE1")
    logger.info("✅ ALS_CASE_TABLE1 table created with {} rows.".format(als_w_demo.count()))

if __name__ == "__main__":
    main()
