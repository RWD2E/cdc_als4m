import logging
import os

from snowflake.snowpark import Session
from snowflake.snowpark.functions import col, row_number, when, lit
from snowflake.snowpark.window import Window

from config import conn, env


logger = logging.getLogger(__name__)


def _build_glp1ra_case(log_tbl):
	w_patid = Window.partition_by("PATID").order_by(
		col("CD_DATE"),
		col("SITE"),
	)

	return (
		log_tbl
		.filter(col("PHE_TYPE") == "GLP1RA")
		.filter(col("PATID").is_not_null())
		.filter(col("CD_DATE").is_not_null())
		.filter(col("SITE") != "CMS")
		.select("PATID", "CD_DATE", "SITE")
		.distinct()
		.with_column("RN", row_number().over(w_patid))
		.filter(col("RN") == 1)
		.select(
			col("PATID"),
			col("CD_DATE").alias("GLP1RA1RX_DATE"),
			col("SITE").alias("INDEX_SRC"),
		)
	)


def build_glp1_tbl1(session: Session):
	log_tbl = session.table("DM_DXRX_LONG")
	glp1ra_case = _build_glp1ra_case(log_tbl)
	glp1ra_case.write.mode("overwrite").save_as_table("GLP1RA_CASE")
	logger.info("GLP1RA_CASE table created with %s rows.", glp1ra_case.count())


def main():
	env.load_env()
	connect_params = conn.load_snowcfg("ID")
	session = conn.get_snow_conn(connect_params)
	session.use_database(os.environ["ID_SNOW_DATABASE"])
	session.use_schema("SX_ALS_GPC")
	session.use_warehouse(os.environ["DEV_SNOW_WAREHOUSE"])

	print(
		session.sql(
			"SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_WAREHOUSE()"
		).collect()
	)
	build_glp1_tbl1(session)

	pat_tbl1 = session.table("PAT_TABLE1")
	glp1ra_case = session.table("GLP1RA_CASE")
	glp1ra_w_demo = (
		glp1ra_case
		.join(
			pat_tbl1,
			glp1ra_case["PATID"] == pat_tbl1["PATID"],
			"inner",
		)
		.select(
			glp1ra_case["PATID"].alias("PATID"),
			glp1ra_case["GLP1RA1RX_DATE"].alias("INDEX_DATE"),
			glp1ra_case["INDEX_SRC"].alias("INDEX_SRC"),
			pat_tbl1["index_date"].alias("ENTRY_DATE"),
			pat_tbl1["birth_date"].alias("BIRTH_DATE"),
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
		.with_column(
			"GLP1RA_BEFORE_ENTRY_IND",
			when(col("INDEX_DATE") < col("ENTRY_DATE"), lit(1)).otherwise(lit(0)),
		)
	)
	glp1ra_w_demo.write.mode("overwrite").save_as_table("GLP1RA_CASE_TABLE1")
	logger.info(
		"GLP1RA_CASE_TABLE1 table created with %s rows.",
		glp1ra_w_demo.count(),
	)


if __name__ == "__main__":
	main()
