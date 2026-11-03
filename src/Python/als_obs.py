import os
from snowflake.snowpark import Session
from snowflake.snowpark.functions import (
    abs as s_abs,
    col,
    coalesce,
    datediff,
    lit,
    row_number,
    sql_expr,
    trim,
    when,
    round as s_round,
    upper as s_upper,
)
from snowflake.snowpark.window import Window
from config import env, conn


def _site_cdm(site: str) -> str:
    return "CMS_PCORNET_CDM" if site == "CMS" else f"PCORNET_CDM_{site}"


def _create_all_obs_table(session: Session, tgt_long_tbl: str) -> None:
    session.sql(
        f"""
        create or replace table {tgt_long_tbl} (
            PATID varchar(50) NOT NULL,
            OBS_DATE date,
            DAYS_SINCE_INDEX number,
            OBS_CODE_TYPE varchar(10),
            OBS_CODE varchar(100),
            OBS_NAME varchar(500),
            OBS_NUM number,
            OBS_UNIT varchar(50),
            OBS_REF_LOW varchar(100),
            OBS_REF_HIGH varchar(100),
            OBS_QUAL varchar(5000),
            OBS_TYPE varchar(105),
            OBS_SRC varchar(10)
        )
        """
    ).collect()


def _build_lab_obs(session: Session, ref_df, site: str):
    site_cdm = _site_cdm(site)
    lab_df = session.table(f"GROUSE_DB.{site_cdm}.LDS_LAB_RESULT_CM")
    loinc_df = session.table("ONTOLOGY.LOINC.LOINC_V2_17")

    return (
        ref_df
        .join(lab_df, ref_df["PATID"] == lab_df["PATID"])
        .join(loinc_df, lab_df["LAB_LOINC"] == loinc_df["LOINC_NUM"], "left")
        .select(
            ref_df["PATID"].alias("PATID"),
            coalesce(
                lab_df["SPECIMEN_DATE"],
                lab_df["LAB_ORDER_DATE"],
                lab_df["RESULT_DATE"],
            ).alias("OBS_DATE"),
            datediff(
                "day",
                ref_df["INDEX_DATE"],
                coalesce(
                    lab_df["SPECIMEN_DATE"],
                    lab_df["LAB_ORDER_DATE"],
                    lab_df["RESULT_DATE"],
                ),
            ).alias("DAYS_SINCE_INDEX"),
            lit("LC").alias("OBS_CODE_TYPE"),
            lab_df["LAB_LOINC"].alias("OBS_CODE"),
            s_upper(coalesce(loinc_df["COMPONENT"], lab_df["RAW_LAB_NAME"])).alias("OBS_NAME"),
            lab_df["RESULT_NUM"].alias("OBS_NUM"),
            lab_df["RESULT_UNIT"].alias("OBS_UNIT"),
            lab_df["NORM_RANGE_LOW"].alias("OBS_REF_LOW"),
            lab_df["NORM_RANGE_HIGH"].alias("OBS_REF_HIGH"),
            lab_df["RESULT_QUAL"].alias("OBS_QUAL"),
            lit("lab").alias("OBS_TYPE"),
            lit(site).alias("OBS_SRC"),
        )
    )

def _vital_num(vital_df, obs_name: str, col_name: str, unit: str):
    return (
        vital_df.select(
            col("PATID"),
            col("MEASURE_DATE"),
            col("MEASURE_TIME"),
            lit(obs_name).alias("OBS_NAME"),
            s_round(col(col_name)).alias("OBS_NUM"),
            lit("NI").alias("OBS_QUAL"),
            lit(unit).alias("OBS_UNIT"),
        )
        .filter(col("OBS_NUM").is_not_null())
    )

def _vital_qual(vital_df, obs_name: str, col_name: str):
    return (
        vital_df.select(
            col("PATID"),
            col("MEASURE_DATE"),
            col("MEASURE_TIME"),
            lit(obs_name).alias("OBS_NAME"),
            lit(None).cast("NUMBER").alias("OBS_NUM"),
            col(col_name).alias("OBS_QUAL"),
            lit(None).cast("STRING").alias("OBS_UNIT"),
        )
        .filter(
            col("OBS_QUAL").is_not_null()
            & (trim(col("OBS_QUAL")) != "")
            & (~col("OBS_QUAL").isin(["UN", "NI", "OT"]))
        )
    )


def _build_vital_obs(session: Session, ref_df, site: str):
    site_cdm = _site_cdm(site)
    vital_df = session.table(f"GROUSE_DB.{site_cdm}.LDS_VITAL")

    num_df = (
        _vital_num(vital_df, "SYSTOLIC", "SYSTOLIC", "mm[Hg]")
        .union_all(_vital_num(vital_df, "DIASTOLIC", "DIASTOLIC", "mm[Hg]"))
        .union_all(_vital_num(vital_df, "HT", "HT", "in_us"))
        .union_all(_vital_num(vital_df, "WT", "WT", "lb_av"))
        .union_all(_vital_num(vital_df, "ORIGINAL_BMI", "ORIGINAL_BMI", "kg/m2"))
    )

    qual_df = (
        _vital_qual(vital_df, "SMOKING", "SMOKING")
        .union_all(_vital_qual(vital_df, "TOBACCO", "TOBACCO"))
        .union_all(_vital_qual(vital_df, "TOBACCO_TYPE", "TOBACCO_TYPE"))
    )

    unpvt_df = num_df.union_all(qual_df)

    return (
        ref_df
        .join(unpvt_df, ref_df["PATID"] == unpvt_df["PATID"])
        .select(
            ref_df["PATID"].alias("PATID"),
            unpvt_df["MEASURE_DATE"].alias("OBS_DATE"),
            datediff("day", ref_df["INDEX_DATE"], unpvt_df["MEASURE_DATE"]).alias(
                "DAYS_SINCE_INDEX"
            ),
            lit("UD").alias("OBS_CODE_TYPE"),
            lit(None).cast("STRING").alias("OBS_CODE"),
            s_upper(unpvt_df["OBS_NAME"]).alias("OBS_NAME"),
            unpvt_df["OBS_NUM"].alias("OBS_NUM"),
            unpvt_df["OBS_UNIT"].alias("OBS_UNIT"),
            lit(None).cast("STRING").alias("OBS_REF_LOW"),
            lit(None).cast("STRING").alias("OBS_REF_HIGH"),
            unpvt_df["OBS_QUAL"].alias("OBS_QUAL"),
            lit("vital").alias("OBS_TYPE"),
            lit(site).alias("OBS_SRC"),
        )
    )


def _build_obsclin_obs(session: Session, ref_df, site: str):
    site_cdm = _site_cdm(site)
    obs_df = session.table(f"GROUSE_DB.{site_cdm}.LDS_OBS_CLIN")
    loinc_df = session.table("ONTOLOGY.LOINC.LOINC_V2_17")

    obs_qual = coalesce(trim(obs_df["OBSCLIN_RESULT_QUAL"]), trim(obs_df["OBSCLIN_RESULT_TEXT"]))

    return (
        ref_df
        .join(obs_df, ref_df["PATID"] == obs_df["PATID"])
        .join(
            loinc_df,
            (obs_df["OBSCLIN_CODE"] == loinc_df["LOINC_NUM"])
            & (obs_df["OBSCLIN_TYPE"] == lit("LC")),
            "left",
        )
        .filter(
            obs_df["OBSCLIN_RESULT_NUM"].is_not_null()
            | (
                obs_qual.is_not_null()
                & (obs_qual != "")
                & (~obs_qual.isin(["UN", "NI", "OT"]))
            )
        )
        .select(
            ref_df["PATID"].alias("PATID"),
            coalesce(obs_df["OBSCLIN_START_DATE"], obs_df["OBSCLIN_STOP_DATE"]).alias(
                "OBS_DATE"
            ),
            datediff(
                "day",
                ref_df["INDEX_DATE"],
                coalesce(obs_df["OBSCLIN_START_DATE"], obs_df["OBSCLIN_STOP_DATE"]),
            ).alias("DAYS_SINCE_INDEX"),
            obs_df["OBSCLIN_TYPE"].alias("OBS_CODE_TYPE"),
            obs_df["OBSCLIN_CODE"].alias("OBS_CODE"),
            s_upper(coalesce(loinc_df["COMPONENT"], obs_df["RAW_OBSCLIN_NAME"])).alias("OBS_NAME"),
            obs_df["OBSCLIN_RESULT_NUM"].alias("OBS_NUM"),
            obs_df["OBSCLIN_RESULT_UNIT"].alias("OBS_UNIT"),
            lit(None).cast("STRING").alias("OBS_REF_LOW"),
            lit(None).cast("STRING").alias("OBS_REF_HIGH"),
            obs_qual.alias("OBS_QUAL"),
            lit("obsclin").alias("OBS_TYPE"),
            lit(site).alias("OBS_SRC"),
        )
    )


def build_obs_long(
    session: Session,
    ref_cohort: str,
    tgt_long_tbl: str,
    sites: list[str]
):
    ref_df = session.table(ref_cohort)

    for site in sites:
        lab_df = _build_lab_obs(session, ref_df, site)
        vital_df = _build_vital_obs(session, ref_df, site)
        obsclin_df = _build_obsclin_obs(session, ref_df, site)
        all_obs_df = lab_df.union_all(vital_df).union_all(obsclin_df)
        all_obs_df.write.mode("append").save_as_table(tgt_long_tbl)
        print(f"✅ Observation data for site {site} appended.")


def main():
    env.load_env()
    cfg = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(cfg)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")

    ref_cohort = "ALS_CASE_TABLE1"
    tgt_long_tbl = "ALS_ALL_OBS"
    sites = [
        "ALLINA",
        "IHC",
        "KUMC",
        "MCRI",
        "MCW",
        "MU",
        "UCD",
        "UCLA",
        "UIOWA",
        "UNMC",
        "UTHOUSTON",
        "UTSW",
        "UU",
        "WASHU",
    ]

    _create_all_obs_table(session, tgt_long_tbl)
    build_obs_long(session, ref_cohort, tgt_long_tbl, sites)

if __name__ == "__main__":
    main()