import os
from snowflake.snowpark import Session
from snowflake.snowpark.functions import (
    col,
    coalesce,
    current_date,
    datediff,
    floor,
    lit,
    max as s_max,
    min as s_min,
    round as s_round,
    row_number,
    when,
    year,
    concat,
)
from snowflake.snowpark.window import Window
from config import env, conn

def _site_cdm(site: str) -> str:
    return "CMS_PCORNET_CDM" if site == "CMS" else f"PCORNET_CDM_{site}"


def _create_pat_demo_long(session: Session) -> None:
    session.sql(
        """
        create or replace table PAT_DEMO_LONG (
            PATID varchar(50) NOT NULL,
            BIRTH_DATE date,
            INDEX_DATE date,
            INDEX_ENC_TYPE varchar(3),
            AGE_AT_INDEX integer,
            SEX varchar(3),
            RACE varchar(6),
            HISPANIC varchar(20),
            CENSOR_DATE date,
            DEATH_IND integer,
            SRC varchar(20)
        )
        """
    ).collect()


def _build_pat_demo_long_site(session: Session, site: str):
    site_cdm = _site_cdm(site)
    demo = session.table(f"GROUSE_DB.{site_cdm}.LDS_DEMOGRAPHIC")
    enc = session.table(f"GROUSE_DB.{site_cdm}.LDS_ENCOUNTER")
    dth = session.table(f"GROUSE_DB.{site_cdm}.LDS_DEATH")

    censor_date = s_max(
        coalesce(enc["DISCHARGE_DATE"].cast("DATE"), enc["ADMIT_DATE"].cast("DATE"),)
    ).over(Window.partition_by(enc["PATID"]))

    death_date = s_max(dth["DEATH_DATE"]).over(Window.partition_by(enc["PATID"]))

    cte = (
        demo.join(enc, demo["PATID"] == enc["PATID"])
        .join(dth, enc["PATID"] == dth["PATID"], "left")
        .filter(demo["PATID"].is_not_null())
        .filter(
            coalesce(
                enc["DISCHARGE_DATE"].cast("DATE"),
                enc["ADMIT_DATE"].cast("DATE"),
            )
            <= current_date()
        )
        .filter(
            ~enc["ENC_TYPE"].isin(["NI","UN","OT"])
        )
        .select(
            enc["PATID"].alias("PATID"),
            demo["BIRTH_DATE"].alias("BIRTH_DATE"),
            enc["ADMIT_DATE"].cast("DATE").alias("INDEX_DATE"),
            enc["ENC_TYPE"].alias("INDEX_ENC_TYPE"),
            s_round(
                datediff(
                    "day",
                    demo["BIRTH_DATE"].cast("DATE"),
                    enc["ADMIT_DATE"].cast("DATE"),
                )
                / lit(365.25)
            ).cast("INTEGER").alias("AGE_AT_INDEX"),
            when(demo["SEX"].isin(["NI", "UN", "OT"]), lit(None)).otherwise(
                demo["SEX"]
            ).alias("SEX"),
            when(demo["RACE"].isin(["NI", "UN", "07", "OT"]), lit(None)).otherwise(
                demo["RACE"]
            ).alias("RACE"),
            when(demo["HISPANIC"].isin(["NI", "UN", "R", "OT"]), lit(None)).otherwise(
                demo["HISPANIC"]
            ).alias("HISPANIC"),
            lit(site).alias("SRC"),
            censor_date.alias("CENSOR_DATE"),
            death_date.alias("DEATH_DATE"),
        )
    )

    return (
        cte.select(
            col("PATID"),
            col("BIRTH_DATE"),
            col("INDEX_DATE"),
            col("INDEX_ENC_TYPE"),
            col("AGE_AT_INDEX"),
            col("SEX"),
            col("RACE"),
            col("HISPANIC"),
            coalesce(col("DEATH_DATE"), col("CENSOR_DATE")).alias("CENSOR_DATE"),
            when(col("DEATH_DATE").is_not_null(), lit(1)).otherwise(lit(0)).alias("DEATH_IND"),
            col("SRC")
        )
        .distinct()
    )


def build_pat_demo_long(session: Session, sites: list[str]):
    _create_pat_demo_long(session)

    for site in sites:
        _build_pat_demo_long_site(session, site).write.mode("append").save_as_table(
            "PAT_DEMO_LONG"
        )
        print(f"✅ PAT_DEMO_LONG data for site {site} appended.")


def build_pat_table1(session: Session):
    demo = session.table("PAT_DEMO_LONG")

    cte_ord = (
        demo.select("*")
        .with_column(
            "RN",
            row_number().over(
                Window.partition_by("PATID").order_by(col("INDEX_DATE"), col("DEATH_IND").desc())
            ),
        )
    )

    agegrp = (
        when(cte_ord["AGE_AT_INDEX"].is_null(), lit("NI"))
        .when(cte_ord["AGE_AT_INDEX"] < 19, lit("agegrp1"))
        .when(
            (cte_ord["AGE_AT_INDEX"] >= 19) & (cte_ord["AGE_AT_INDEX"] < 24),
            lit("agegrp2"),
        )
        .when(
            (cte_ord["AGE_AT_INDEX"] >= 25) & (cte_ord["AGE_AT_INDEX"] < 85),
            concat(
                lit("agegrp"),
                (floor((cte_ord["AGE_AT_INDEX"] - lit(25)) / lit(5)) + lit(3)).cast(
                    "STRING"
                ),
            ),
        )
        .otherwise(lit("agegrp15"))
    )

    race_grp = (
        when(cte_ord["RACE"] == lit("05"), lit("white"))
        .when(cte_ord["RACE"] == lit("03"), lit("black"))
        .when(cte_ord["RACE"] == lit("02"), lit("asian"))
        .when(cte_ord["RACE"] == lit("01"), lit("aian"))
        .when(cte_ord["RACE"] == lit("04"), lit("nhpi"))
        .when(cte_ord["RACE"] == lit("06"), lit("multi"))
        .when(cte_ord["RACE"] == lit("OT"), lit("other"))
        .otherwise(lit("NI"))
    )

    hispanic_grp = (
        when(cte_ord["HISPANIC"] == lit("Y"), lit("hispanic"))
        .when(cte_ord["HISPANIC"] == lit("N"), lit("non-hispanic"))
        .otherwise(lit("NI"))
    )

    pat_table1 = (
        cte_ord.filter(cte_ord["RN"] == 1)
        .select(
            cte_ord["PATID"].alias("PATID"),
            cte_ord["BIRTH_DATE"].alias("BIRTH_DATE"),
            cte_ord["INDEX_DATE"].alias("INDEX_DATE"),
            year(cte_ord["INDEX_DATE"]).alias("INDEX_YEAR"),
            cte_ord["AGE_AT_INDEX"].alias("AGE_AT_INDEX"),
            agegrp.alias("AGEGRP_AT_INDEX"),
            cte_ord["SEX"].alias("SEX"),
            race_grp.alias("RACE"),
            hispanic_grp.alias("HISPANIC"),
            cte_ord["INDEX_ENC_TYPE"].alias("INDEX_ENC_TYPE"),
            cte_ord["CENSOR_DATE"].alias("CENSOR_DATE"),
            year(cte_ord["CENSOR_DATE"]).alias("CENSOR_YEAR"),
            cte_ord["DEATH_IND"].alias("DEATH_IND"),
            cte_ord["SRC"].alias("SRC"),
        )
    )

    pat_table1.write.mode("overwrite").save_as_table("PAT_TABLE1")


def main():
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")
    print(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE()").collect())

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
        # "CMS"
    ]

    build_pat_demo_long(session, sites)
    build_pat_table1(session)


if __name__ == "__main__":
    main()