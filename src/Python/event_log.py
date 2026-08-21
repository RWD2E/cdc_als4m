import os
import logging
from config import env, conn
from utils import QueryFromJson
from snowflake.snowpark import Session
from snowflake.snowpark.functions import lit
from snowflake.snowpark.types import (
    StructType,
    StructField,
    StringType,
    DateType,
    DoubleType,
)

logger = logging.getLogger(__name__)


def _table_exists(session: Session, table_name: str) -> bool:
    parts = table_name.split(".")

    try:
        parts = table_name.split(".")

        if len(parts) == 3:
            database, schema, table = parts
            sql = (
                f"SHOW TABLES LIKE '{table}' "
                f"IN SCHEMA {database}.{schema}"
            )
        elif len(parts) == 2:
            schema, table = parts
            sql = (
                f"SHOW TABLES LIKE '{table}' "
                f"IN SCHEMA {schema}"
            )
        else:
            table = table_name
            sql = f"SHOW TABLES LIKE '{table}'"

        return len(session.sql(sql).collect()) > 0

    except Exception:
        logger.exception(
            "Unable to determine whether table %s exists.",
            table_name,
        )
        return False


KD_LOG_SCHEMA = StructType([
    StructField("PATID", StringType()),
    StructField("ENCOUNTERID", StringType()),
    StructField("CD", StringType()),
    StructField("CD_TYPE", StringType()),
    StructField("CD_DATE", DateType()),
    StructField("PHE_TYPE", StringType()),
    StructField("SITE", StringType()),
])

def _create_log_tbl_kd(session: Session, log_tbl:str) -> None:
    (
        session.create_dataframe([], schema=KD_LOG_SCHEMA)
        .write.mode("overwrite")
        .save_as_table(log_tbl)
    )


KVD_LOG_SCHEMA = StructType([
    StructField("PATID", StringType()),
    StructField("ENCOUNTERID", StringType()),
    StructField("CD_VAL", DoubleType()),
    StructField("CD_UNIT", StringType()),
    StructField("CD", StringType()),
    StructField("CD_TYPE", StringType()),
    StructField("CD_DATE", DateType()),
    StructField("PHE_TYPE", StringType()),
    StructField("SITE", StringType()),
])

def _create_log_tbl_kvd(session: Session, log_tbl: str) -> None:
     (
        session.create_dataframe([], schema=KVD_LOG_SCHEMA)
        .write.mode("overwrite")
        .save_as_table(log_tbl)
    )


def _set_site_schema(site: str) -> str:
    if site == 'CMS':
        return f"GROUSE_DB.{site}_PCORNET_CDM"
    else:
        return f"GROUSE_DB.PCORNET_CDM_{site}"
    

def _build_dm_dxrx(session: Session, site: str):
    vs_dm_dx = QueryFromJson(
        url = './ref/vs-cde-dm.json',
        sqlty = 'snow',
        cd_field = 'DX',
        cdtype_field = 'DX_TYPE',
        date_fields = ["DX_DATE",'ADMIT_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_DIAGNOSIS",
        other_fields=["PATID","ENCOUNTERID"],
        sel_keys = ['T2DM','T1DM'],
        sel_domain = "dx"
    )
    vs_dm_prx = QueryFromJson(
        url = './ref/vs-cde-dm.json',
        sqlty = 'snow',
        cd_field = 'RXNORM_CUI',
        date_fields = ["RX_START_DATE",'RX_ORDER_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_PRESCRIBING",
        other_fields=["PATID","ENCOUNTERID"],
        sel_keys = ['SU','AGI','GLP1RA','DPP4','MEG','AML','INS','SGLT2','COMBO','BIGU','TZD'],
        sel_domain = "rx"
    )
    vs_dm_erx = QueryFromJson(
        url = './ref/vs-cde-dm.json',
        sqlty = 'snow',
        cd_field = 'RXNORM_CUI',
        date_fields = ["EXT_PAT_START_DATE",'EXT_RECORD_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_EXTERNAL_MEDS",
        other_fields=["PATID","NULL AS ENCOUNTERID"],
        sel_keys = ['SU','AGI','GLP1RA','DPP4','MEG','AML','INS','SGLT2','COMBO','BIGU','TZD'],
        sel_domain = "rx"
    )
    vs_dm_drx = QueryFromJson(
        url = './ref/vs-cde-dm.json',
        sqlty = 'snow',
        cd_field = 'NDC',
        date_fields = ['DISPENSE_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_DISPENSING",
        other_fields=["PATID","DISPENSINGID"],
        sel_keys = ['SU','AGI','GLP1RA','DPP4','MEG','AML','INS','SGLT2','COMBO','BIGU','TZD'],
        sel_domain = "rx"
    )

    dm_dxrx_df = session.create_dataframe([],schema=KD_LOG_SCHEMA)

    if _table_exists(session, f"{_set_site_schema(site)}.LDS_DIAGNOSIS"):
        dm_dxrx_df = dm_dxrx_df.union_all(
            session.sql(vs_dm_dx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_DIAGNOSIS for site %s because the table does not exist.",
            site,
        )

    if _table_exists(session, f"{_set_site_schema(site)}.LDS_PRESCRIBING"):
        dm_dxrx_df = dm_dxrx_df.union_all(
            session.sql(vs_dm_prx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_PRESCRIBING for site %s because the table does not exist.",
            site,
        )
    
    if _table_exists(session, f"{_set_site_schema(site)}.LDS_EXTERNAL_MEDS"):
        dm_dxrx_df = dm_dxrx_df.union_all(
            session.sql(vs_dm_erx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_EXTERNAL_MEDS for site %s because the table does not exist.",
            site,
        )
    
    if _table_exists(session, f"{_set_site_schema(site)}.LDS_DISPENSING"):
        dm_dxrx_df = dm_dxrx_df.union_all(
            session.sql(vs_dm_drx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_DISPENSING for site %s because the table does not exist.",
            site,
        )

    return dm_dxrx_df


def _build_dm_lab(session: Session, site: str):
    vs_dm_lab = QueryFromJson(
        url = './ref/vs-cde-dm.json',
        sqlty = 'snow',
        cd_field = 'LAB_LOINC',
        date_fields = ["SPECIMEN_DATE",'LAB_ORDER_DATE','RESULT_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_LAB_RESULT_CM",
        other_fields=["PATID","ENCOUNTERID","RESULT_NUM","RESULT_UNIT"],
        sel_keys = ['HbA1C','FBG','RBG'],
        sel_domain = "lab",
        val_field = "RESULT_NUM"
    )

    dm_lab_df = session.create_dataframe([],schema=KVD_LOG_SCHEMA)

    if _table_exists(session, f"{_set_site_schema(site)}.LDS_LAB_RESULT_CM"):
        dm_lab_df = dm_lab_df.union_all(
            session.sql(vs_dm_lab.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_LAB_RESULT_CM for site %s because the table does not exist.",
            site,
        )
    
    return dm_lab_df


def _build_als_dxrxpx(session: Session, site: str):
    vs_als_dx = QueryFromJson(
        url = './ref/vs-cde-als.json',
        sqlty = 'snow',
        cd_field = 'DX',
        cdtype_field = 'DX_TYPE',
        date_fields = ["DX_DATE",'ADMIT_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_DIAGNOSIS",
        other_fields=["PATID","ENCOUNTERID"],
        sel_keys = ['ALS','SxSpeech','SxSwallowing','SxMuscleWeakness','SxMuscleMovement','SxPainJointLimb','SxWalking','PBP','PLS','FTD','SMA','PBA'],
        sel_domain = "dx"
    )
    vs_als_px = QueryFromJson(
        url = './ref/vs-cde-als.json',
        sqlty = 'snow',
        cd_field = 'PX',
        cdtype_field= 'PX_TYPE',
        date_fields = ["PX_DATE",'ADMIT_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_PROCEDURES",
        other_fields=["PATID","ENCOUNTERID"],
        sel_keys = ["MobilityDeviceWheelchair","AssistiveDeviceCommunication","EnteralFeedingEquipment","RespiratoryDeviceNIV","RespiratoryDeviceMV"],
        sel_domain = "px"
    )
    vs_als_prx = QueryFromJson(
        url = './ref/vs-cde-als.json',
        sqlty = 'snow',
        cd_field = 'RXNORM_CUI',
        date_fields = ["RX_START_DATE",'RX_ORDER_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_PRESCRIBING",
        other_fields=["PATID","ENCOUNTERID"],
        sel_keys = ["riluzole","edaravone","tofersen"],
        sel_domain = "rx"
    )
    vs_als_erx = QueryFromJson(
        url = './ref/vs-cde-als.json',
        sqlty = 'snow',
        cd_field = 'RXNORM_CUI',
        date_fields = ["EXT_PAT_START_DATE",'EXT_RECORD_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_EXTERNAL_MEDS",
        other_fields=["PATID","NULL AS ENCOUNTERID"],
        sel_keys = ["riluzole","edaravone","tofersen"],
        sel_domain = "rx"
    )
    vs_als_drx = QueryFromJson(
        url = './ref/vs-cde-als.json',
        sqlty = 'snow',
        cd_field = 'NDC',
        date_fields = ['DISPENSE_DATE'],
        srctbl_name = f"{_set_site_schema(site)}.LDS_DISPENSING",
        other_fields=["PATID","DISPENSINGID"],
        sel_keys = ["riluzole","edaravone","tofersen"],
        sel_domain = "rx"
    )

    als_dxrxpx_df = session.create_dataframe([],schema=KD_LOG_SCHEMA)

    if _table_exists(session, f"{_set_site_schema(site)}.LDS_DIAGNOSIS"):
        als_dxrxpx_df = als_dxrxpx_df.union_all(
            session.sql(vs_als_dx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_DIAGNOSIS for site %s because the table does not exist.",
            site,
        )

    if _table_exists(session, f"{_set_site_schema(site)}.LDS_PRESCRIBING"):
        als_dxrxpx_df = als_dxrxpx_df.union_all(
            session.sql(vs_als_prx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_PRESCRIBING for site %s because the table does not exist.",
            site,
        )

    if _table_exists(session, f"{_set_site_schema(site)}.LDS_EXTERNAL_MEDS"):
        als_dxrxpx_df = als_dxrxpx_df.union_all(
            session.sql(vs_als_erx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_EXTERNAL_MEDS for site %s because the table does not exist.",
            site,
        )
    
    if _table_exists(session, f"{_set_site_schema(site)}.LDS_DISPENSING"):
        als_dxrxpx_df = als_dxrxpx_df.union_all(
            session.sql(vs_als_drx.gen_qry())
            .with_column("SITE", lit(site))
        )
    else:
        logger.warning(
            "Skipping LDS_DISPENSING for site %s because the table does not exist.",
            site,
        )

    return als_dxrxpx_df


def build_event_logs(
    session: Session,
    site_lst: list[str],
    log_dm_dxrx: str = "DM_DXRX_LONG",
    log_dm_lab: str = "DM_LAB_LONG",
    log_als_dxrxpx: str = "ALS_DXRXPX_LONG"
):
    _create_log_tbl_kd(session, log_dm_dxrx)
    _create_log_tbl_kvd(session, log_dm_lab)
    _create_log_tbl_kd(session, log_als_dxrxpx)

    for site in site_lst:
        _build_dm_dxrx(session, site).write.mode("append").save_as_table(log_dm_dxrx)
        _build_dm_lab(session, site).write.mode("append").save_as_table(log_dm_lab)
        _build_als_dxrxpx(session, site).write.mode("append").save_as_table(log_als_dxrxpx)
        logger.info(f"✅ Event log data for site {site} appended.")


def main():
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")
    logger.info(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_WAREHOUSE()").collect())

    site_lst = [
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

    build_event_logs(session, site_lst)

if __name__ == "__main__":
    main()
