import logging
import os
from snowflake.snowpark import Session
from snowflake.snowpark.functions import (
    col,
    count,
    datediff,
    lit,
    min as s_min,
    row_number,
    when,
    coalesce
)
from snowflake.snowpark.window import Window
from config import env, conn

logger = logging.getLogger(__name__)

def _build_log_uniq(session, log_tbl):
    phemap = session.create_dataframe(
        [
            ("AML","RX"),
            ("INS","RX"),
            ("GLP1RA","RX"),
            ("DPP4","RX"),
            ("MEG","RX"),
            ("SGLT2","RX"),
            ("T2DM","DX"),
            ("T1DM","DX"),
            ("TZD","RX"),
            ("SU","RX"),
            ("COMBO","RX"),
            ("BIGU","RX"),
            ("AGI","RX"),
            ("RBG","LAB"),
            ("FBG","LAB"),
            ("HbA1C","LAB")
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
        .filter(col("PHE_TYPE").isin(["T1DM","T2DM"]))
        .filter(col("CD_DATE").is_not_null())
        .with_column(
            "DX1_DATE",
            s_min(col("CD_DATE")).over(w_patid),
        )
        .filter(col("CD_DATE") == col("DX1_DATE"))
        .select(
            col("PATID").alias("PATID"),
            col("DX1_DATE").alias("DX1_DATE"),
            col("SITE").alias("INDEX_SRC")
        )
        .distinct()
    )

def _build_t1dm_ind(log_uniq):
    return (
        log_uniq
            .filter(col("PHE_TYPE").isin(["T1DM"]))
            .select("PATID")
            .drop_duplicates()
            .with_column("T1DM_IND", lit(1))
    )

def _build_t1dm_rt(log_uniq):
    dm_counts = (
        log_uniq
            .group_by(col("PATID"))
            .agg(
                count(when(col("PHE_TYPE") == "T1DM", 1)).alias("T1DM_CNT"),
                count(when(col("PHE_TYPE") == "T2DM", 1)).alias("T2DM_CNT")
            )
            .with_column("DM_CNT", col("T1DM_CNT") + col("T2DM_CNT"))
    )
    return (
        dm_counts
            .with_column(
                "T1_RT",
                when(col("DM_CNT") > 0, col("T1DM_CNT") / col("DM_CNT")).otherwise(lit(0))
            )
    )

def _build_insl_ind(log_uniq):
    return (
        log_uniq
            .filter(col("PHE_TYPE").isin(["INS"]))
            .select("PATID")
            .drop_duplicates()
            .with_column("INSL_IND", lit(1))
    )

def _build_phepair(log_uniq):
    log_uniqa = log_uniq.select("PATID", "PHE_TYPE", "CD_DATE").distinct()
    log_uniqb = log_uniq.select("PATID", "PHE_TYPE", "CD_DATE").distinct()

    return (
        log_uniqa.join(log_uniqb, ["PATID"], how="inner")
            .filter(log_uniqa["CD_DATE"] < log_uniqb["CD_DATE"])
            .with_column("DELTA_DAYS", datediff("day", log_uniqa["CD_DATE"], log_uniqb["CD_DATE"]))
            .filter(col("DELTA_DAYS").between(2,730))
            .select(
                log_uniqa["PATID"], 
                log_uniqa["PHE_TYPE"].alias("PHE_TYPE1"), 
                log_uniqa["CD_DATE"].alias("CD_DATE1"),
                log_uniqb["PHE_TYPE"].alias("PHE_TYPE2"), 
                log_uniqb["CD_DATE"].alias("CD_DATE2"), 
                col("DELTA_DAYS")
            )
            .drop_duplicates()
    )

def _build_dm_pt(phepair, init_dx, t1dm_ind, t1dm_rt, insl_ind):
    w_patid = Window.partition_by("PATID").order_by(col("CD_DATE1"))
    return (
        phepair
            .select("*", row_number().over(w_patid).alias("RN"))
            .filter(col("RN") == 1)
            .select(
                phepair["PATID"],
                phepair["PHE_TYPE1"],
                phepair["PHE_TYPE2"],
                phepair["CD_DATE1"],
                phepair["DELTA_DAYS"],
            )
            .join(init_dx, ["PATID"], how="inner")
            .join(t1dm_ind, ["PATID"], how="left")
            .with_column("T1DM_IND",coalesce(col("T1DM_IND"), lit(0)))
            .join(insl_ind, ["PATID"], how="left")
            .with_column("INSL_IND",coalesce(col("INSL_IND"), lit(0)))
            .join(t1dm_rt, ["PATID"], how="left")
            .with_column(
                "T1DM_IND2",
                when((col("T1_RT") > 0.5) & (col("INSL_IND") == 1), lit(1))
                .otherwise(lit(0)),
            )
    )


def build_dm_tbl1(session: Session):
    log_dm_dxrx = session.table("DM_DXRX_LONG")
    log_dm_lab = session.table("DM_LAB_LONG")
    log_uniq = (
        _build_log_uniq(session, log_dm_dxrx)
            .union_all(_build_log_uniq(session, log_dm_lab))
            .filter(col("SITE") != 'CMS') # remove CMS
    )

    phepair = _build_phepair(log_uniq)
    init_dx = _build_init_dx(log_uniq)
    t1dm_ind = _build_t1dm_ind(log_uniq)
    t1dm_rt = _build_t1dm_rt(log_uniq)
    insl_ind = _build_insl_ind(log_uniq)

    final = (
        _build_dm_pt(phepair, init_dx, t1dm_ind, t1dm_rt, insl_ind)
        .select(
            col("PATID"),
            col("PHE_TYPE1"),
            col("PHE_TYPE2"),
            col("CD_DATE1").alias("PHE1_DATE"),
            col("DELTA_DAYS"),
            col("DX1_DATE").alias("DM1DX_DATE"),
            col("INDEX_SRC"),
            col("T1DM_IND"),
            col("INSL_IND"),
            col("T1_RT"),
            col("T1DM_IND2")
        )
    )
    final.write.mode("overwrite").save_as_table("DM_CASE")

def main():
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")
    session.use_warehouse(os.environ["DEV_SNOW_WAREHOUSE"]) #use bigger warehouse

    print(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_WAREHOUSE()").collect())
    build_dm_tbl1(session)

    # attach demographic info
    pat_tbl1 = session.table("PAT_TABLE1")
    dm_table1 = session.table("DM_CASE")
    dm_w_demo = (
        dm_table1.join(
            pat_tbl1, dm_table1["PATID"] == pat_tbl1["PATID"], 
            "inner"
        )
        .select(
            dm_table1["PATID"].alias("PATID"),
            dm_table1["DM1DX_DATE"].alias("DM1DX_DATE"),
            dm_table1["INDEX_SRC"].alias("INDEX_SRC"),
            dm_table1["T1DM_IND"].alias("T1DM_IND"),
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
    dm_w_demo.write.mode("overwrite").save_as_table("DM_CASE_TABLE1")
    logger.info("✅ DM_CASE_TABLE1 table created with {} rows.".format(dm_w_demo.count()))

if __name__ == "__main__":
    main()
