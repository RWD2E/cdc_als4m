"""Build patient demographics from global encounters and site-specific sources."""

import logging
import os

from snowflake.snowpark import DataFrame, Session
from snowflake.snowpark.functions import (
    col,
    coalesce,
    current_date,
    floor,
    lit,
    max as s_max,
    row_number,
    when,
    year,
    concat,
)
from snowflake.snowpark.window import Window
from config import env, conn

logger = logging.getLogger(__name__)


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
            SRC varchar(20),
            DEATH_DATE date,
            INDEX_SITE varchar(20)
        )
        """
    ).collect()


def _build_pat_enc_summary(session: Session):
    enc = (
        session.table("ENC_TABLE1")
        .filter(col("PATID").is_not_null())
        .filter(
            coalesce(col("DISCHARGE_DATE"), col("ADMIT_DATE")).cast("DATE")
            <= current_date()
        )
    )
    last_visit = enc.group_by("PATID").agg(
        s_max(coalesce(col("DISCHARGE_DATE"), col("ADMIT_DATE")).cast("DATE"))
        .alias("LAST_VISIT_DATE")
    )
    first_visit = (
        enc.filter(col("ADMIT_DATE").is_not_null())
        .with_column(
            "RN",
            row_number().over(
                Window.partition_by("PATID").order_by(
                    col("ADMIT_DATE"), col("SITE_ID"), col("ENCOUNTERID"), col("ENC_TYPE")
                )
            ),
        )
        .filter(col("RN") == 1)
        .select(
            "PATID",
            col("ADMIT_DATE").cast("DATE").alias("INDEX_DATE"),
            col("ENC_TYPE").alias("INDEX_ENC_TYPE"),
            col("AGE_AT_ENC").alias("AGE_AT_INDEX"),
            col("SITE_ID").alias("INDEX_SITE"),
        )
    )
    return first_visit.join(last_visit, on="PATID", how="inner")


def _build_pat_demo_long_site(
    session: Session, site: str, enc_summary: DataFrame | None = None,
):
    site_cdm = _site_cdm(site)
    demo = (
        session.table(f"GROUSE_DB.{site_cdm}.LDS_DEMOGRAPHIC")
        .select("PATID", "BIRTH_DATE", "SEX", "RACE", "HISPANIC")
        .filter(col("PATID").is_not_null())
    )
    enc = _build_pat_enc_summary(session) if enc_summary is None else enc_summary
    enc = enc.select(
        "PATID", "INDEX_DATE", "INDEX_ENC_TYPE", "AGE_AT_INDEX",
        "INDEX_SITE", "LAST_VISIT_DATE",
    )
    dth = session.table(f"GROUSE_DB.{site_cdm}.LDS_DEATH")
    dth = dth.group_by("PATID").agg(
        s_max(col("DEATH_DATE").cast("DATE")).alias("DEATH_DATE")
    )

    cte = (
        demo.join(enc, on="PATID", how="inner")
        .join(dth, on="PATID", how="left")
        .select(
            col("PATID"),
            col("BIRTH_DATE"),
            col("INDEX_DATE"),
            col("INDEX_ENC_TYPE"),
            col("AGE_AT_INDEX"),
            when(col("SEX").isin(["NI", "UN", "OT"]), lit(None)).otherwise(
                col("SEX")
            ).alias("SEX"),
            when(col("RACE").isin(["NI", "UN", "07"]), lit(None)).otherwise(
                col("RACE")
            ).alias("RACE"),
            when(col("HISPANIC").isin(["NI", "UN", "R", "OT"]), lit(None)).otherwise(
                col("HISPANIC")
            ).alias("HISPANIC"),
            lit(site).alias("SRC"),
            col("LAST_VISIT_DATE").alias("CENSOR_DATE"),
            col("DEATH_DATE"),
            col("INDEX_SITE"),
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
            col("CENSOR_DATE"),
            when(col("DEATH_DATE").is_not_null(), lit(1)).otherwise(lit(0)).alias("DEATH_IND"),
            col("SRC"),
            col("DEATH_DATE"),
            col("INDEX_SITE"),
        )
        .distinct()
    )


def build_pat_demo_long(session: Session, sites: list[str]):
    sites = list(dict.fromkeys(sites))
    if not sites:
        raise ValueError("At least one demographic/death source site is required")

    logger.info("Caching the global encounter summary from ENC_TABLE1.")
    enc_summary = _build_pat_enc_summary(session).cache_result()
    logger.info("Encounter summary cached; creating PAT_DEMO_LONG.")
    _create_pat_demo_long(session)
    logger.info("Created PAT_DEMO_LONG; processing %s source sites.", len(sites))

    completed = False
    try:
        for index, site in enumerate(sites, start=1):
            logger.info(
                "Site %s/%s (%s): preparing and appending demographics.",
                index, len(sites), site,
            )
            _build_pat_demo_long_site(
                session, site, enc_summary
            ).write.mode("append").save_as_table("PAT_DEMO_LONG")
            logger.info("Site %s/%s (%s): append complete.", index, len(sites), site)
        completed = True
    finally:
        if not completed:
            logger.error(
                "PAT_DEMO_LONG is incomplete; one or more source sites were not processed."
            )

    logger.info("Completed PAT_DEMO_LONG for %s source sites.", len(sites))


def build_pat_table1(session: Session):
    """Prefer non-CMS demographics and aggregate deaths in PAT_DEMO_LONG.

    A site's death record is included only when that site also has a
    demographic record for the patient.
    """
    logger.info("Preparing PAT_TABLE1 from PAT_DEMO_LONG; non-CMS demographics are preferred.")
    demo = (
        session.table("PAT_DEMO_LONG")
        .with_column(
            "ALL_DEATH_DATE", s_max("DEATH_DATE").over(Window.partition_by("PATID"))
        )
        .with_column(
            "CENSOR_DATE", coalesce(col("ALL_DEATH_DATE"), col("CENSOR_DATE"))
        )
        .with_column(
            "DEATH_IND", when(col("ALL_DEATH_DATE").is_not_null(), lit(1)).otherwise(lit(0))
        )
    )

    cte_ord = (
        demo.select("*")
        .with_column(
            "RN",
            row_number().over(
                Window.partition_by("PATID").order_by(
                    when(col("SRC") == lit("CMS"), lit(1)).otherwise(lit(0)),
                    col("INDEX_DATE"),
                    when(col("SRC") == col("INDEX_SITE"), lit(0)).otherwise(lit(1)),
                    col("SRC"), col("BIRTH_DATE"), col("SEX"), col("RACE"), col("HISPANIC"),
                )
            ),
        )
    )

    age = col("AGE_AT_INDEX")
    race = col("RACE")
    hispanic = col("HISPANIC")
    agegrp = (
        when(age.is_null(), lit("NI"))
        .when(age < 19, lit("agegrp1"))
        .when(
            (age >= 19) & (age < 25),
            lit("agegrp2"),
        )
        .when(
            (age >= 25) & (age < 85),
            concat(
                lit("agegrp"),
                (floor((age - lit(25)) / lit(5)) + lit(3)).cast(
                    "STRING"
                ),
            ),
        )
        .otherwise(lit("agegrp15"))
    )

    race_grp = (
        when(race == lit("05"), lit("white"))
        .when(race == lit("03"), lit("black"))
        .when(race == lit("02"), lit("asian"))
        .when(race == lit("01"), lit("aian"))
        .when(race == lit("04"), lit("nhpi"))
        .when(race == lit("06"), lit("multi"))
        .when(race == lit("OT"), lit("other"))
        .otherwise(lit("NI"))
    )

    hispanic_grp = (
        when(hispanic == lit("Y"), lit("hispanic"))
        .when(hispanic == lit("N"), lit("non-hispanic"))
        .otherwise(lit("NI"))
    )

    pat_table1 = (
        cte_ord.filter(col("RN") == 1)
        .select(
            col("PATID"),
            col("BIRTH_DATE"),
            col("INDEX_DATE"),
            year(col("INDEX_DATE")).alias("INDEX_YEAR"),
            col("AGE_AT_INDEX"),
            agegrp.alias("AGEGRP_AT_INDEX"),
            col("SEX"),
            race_grp.alias("RACE"),
            hispanic_grp.alias("HISPANIC"),
            col("INDEX_ENC_TYPE"),
            col("CENSOR_DATE"),
            year(col("CENSOR_DATE")).alias("CENSOR_YEAR"),
            col("DEATH_IND"),
            col("SRC"),
        )
    )

    logger.info("Writing PAT_TABLE1 with one selected demographic record per patient.")
    pat_table1.write.mode("overwrite").save_as_table("PAT_TABLE1")
    logger.info("Completed PAT_TABLE1.")


def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    logger.info("Loading Snowflake configuration.")
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    try:
        session.use_database(os.environ["ID_SNOW_DATABASE"])
        session.use_warehouse(os.environ["DEV_SNOW_WAREHOUSE"])
        session.use_schema("SX_ALS_GPC")
        context = session.sql(
            "SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), "
            "CURRENT_SCHEMA(), CURRENT_WAREHOUSE()"
        ).collect()
        logger.info("Snowflake context (user, role, database, schema, warehouse): %s", context)
        if not context or not context[0][3]:
            raise RuntimeError(
                "Snowflake has no current schema; configure a default schema before "
                "running the patient-table pipeline."
            )

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
            "CMS"
        ]

        logger.info("Starting patient demographic pipeline for %s sites.", len(sites))
        build_pat_demo_long(session, sites)
        build_pat_table1(session)
        logger.info("Patient demographic pipeline completed.")
    finally:
        logger.info("Closing Snowflake session.")
        session.close()


if __name__ == "__main__":
    main()