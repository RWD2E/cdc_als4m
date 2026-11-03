"""Stack PCORnet encounters with provider attributes and age into ENC_TABLE1.

Pass an explicit site list to build_enc_table1; no schema discovery is performed.
Sites map to PCORNET_CDM_<site>, except CMS maps to CMS_PCORNET_CDM.
SITE_ID retains the source site label on every encounter row.
Provider and demographic joins are left joins within each source schema.
AGE_AT_ENC is rounded days from birth to admission / 365.25.
Missing demographic matches or dates yield NULL age; BIRTH_DATE is not retained.
Missing source fields/tables or conflicting lookup records raise errors.
"""

import logging
import os

from snowflake.snowpark import DataFrame, Session
from snowflake.snowpark.functions import col, count, datediff, lit, round as s_round

from config import conn, env

logger = logging.getLogger(__name__)

ENCOUNTER_COLUMNS = (
    "PATID", "ENCOUNTERID", "ENC_TYPE", "ADMIT_DATE", "DISCHARGE_DATE",
    "DISCHARGE_DISPOSITION", "DISCHARGE_STATUS", "DRG", "PROVIDERID",
    "FACILITY_LOCATION", "FACILITY_TYPE", "PAYER_TYPE_PRIMARY", "ADMITTING_SOURCE",
)
PROVIDER_COLUMNS = (
    "PROVIDER_SPECIALTY_PRIMARY", "PROVIDER_SEX", "PROVIDER_NPI",
)
ENC_TYPES = ("AV", "OA", "IP", "EI", "ED", "IS", "TH")


def _require_columns(data: DataFrame, table: str, required: tuple[str, ...]) -> None:
    available = {name.upper().strip('"') for name in data.columns}
    missing = [name for name in required if name not in available]
    if missing:
        raise ValueError(f"{table} is missing required columns: {', '.join(missing)}")


def _build_enc_long_site(session: Session, site: str) -> DataFrame:
    """Build encounters enriched with provider attributes and age for one site."""
    logger.info("Site %s: starting encounter preparation.", site)
    site_cdm = "CMS_PCORNET_CDM" if site == "CMS" else f"PCORNET_CDM_{site}"
    quoted_schema = site_cdm.replace('"', '""')
    source = f'GROUSE_DB."{quoted_schema}"'
    enc_table = f"{source}.LDS_ENCOUNTER"
    provider_table = f"{source}.LDS_PROVIDER"
    demo_table = f"{source}.LDS_DEMOGRAPHIC"
    enc = session.table(enc_table)
    provider = session.table(provider_table)
    demo = session.table(demo_table)
    _require_columns(enc, enc_table, ENCOUNTER_COLUMNS)
    _require_columns(provider, provider_table, ("PROVIDERID", *PROVIDER_COLUMNS))
    _require_columns(demo, demo_table, ("PATID", "BIRTH_DATE"))
    logger.info("Site %s: source columns validated; checking demographic records.", site)
    demo = demo.select(
        col("PATID").alias("DEMO_PATID"),
        col("BIRTH_DATE").cast("DATE").alias("BIRTH_DATE"),
    ).distinct()
    demo_conflicts = (
        demo.filter(col("DEMO_PATID").is_not_null())
        .group_by("DEMO_PATID").agg(count("*").alias("N"))
        .filter(col("N") > 1).limit(1).collect()
    )
    if demo_conflicts:
        raise ValueError(f"{demo_table} has conflicting birth dates for a PATID")
    logger.info("Site %s: demographic records validated; checking provider records.", site)
    provider = provider.select("PROVIDERID", *PROVIDER_COLUMNS).distinct()
    conflicts = (
        provider.filter(col("PROVIDERID").is_not_null())
        .group_by("PROVIDERID").agg(count("*").alias("N"))
        .filter(col("N") > 1).limit(1).collect()
    )
    if conflicts:
        raise ValueError(f"{provider_table} has conflicting attributes for a PROVIDERID")
    logger.info("Site %s: provider records validated; preparing encounter filters and joins.", site)

    enc = enc.filter(col("ENC_TYPE").isin(*ENC_TYPES))
    joined = enc.join(provider, enc["PROVIDERID"] == provider["PROVIDERID"], "left")
    enriched = joined.select(
        *[
            enc[name].cast("DATE" if name in ("ADMIT_DATE", "DISCHARGE_DATE") else "STRING")
            .alias(name)
            for name in ENCOUNTER_COLUMNS
        ],
        *[provider[name].cast("STRING").alias(name) for name in PROVIDER_COLUMNS],
    )
    result = enriched.join(
        demo, enriched["PATID"] == demo["DEMO_PATID"], "left",
    ).select(
        *[enriched[name] for name in (*ENCOUNTER_COLUMNS, *PROVIDER_COLUMNS)],
        s_round(
            datediff("day", demo["BIRTH_DATE"], enriched["ADMIT_DATE"]) / lit(365.25)
        ).cast("INTEGER").alias("AGE_AT_ENC"),
        lit(site).alias("SITE_ID"),
    )
    logger.info("Site %s: enriched encounter query prepared; execution occurs at table write.", site)
    return result


def build_enc_table1(
    session: Session, sites: list[str], output_table: str = "ENC_TABLE1",
) -> DataFrame:
    """Build one stacked encounter table; preserve rows and source-local joins."""
    sites = list(dict.fromkeys(sites))
    if not sites:
        raise ValueError("At least one LDS_ENCOUNTER source site is required")
    frames = []
    for index, site in enumerate(sites, start=1):
        logger.info("Preparing site %s/%s: %s.", index, len(sites), site)
        frames.append(_build_enc_long_site(session, site))
    stacked = frames[0]
    for frame in frames[1:]:
        stacked = stacked.union_all(frame)
    logger.info("Writing %s from %s prepared sites.", output_table, len(sites))
    stacked.write.mode("overwrite").save_as_table(output_table)
    logger.info("%s created from %s source sites.", output_table, len(sites))
    return session.table(output_table)


def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")
    # session.use_warehouse(os.environ["DEV_SNOW_WAREHOUSE"])
    logger.info(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_WAREHOUSE()").collect())

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
    build_enc_table1(session, sites)


if __name__ == "__main__":
    main()