"""Single-cohort Table 1 summaries for a patient-level Snowflake table.

Configure index_col explicitly when deriving follow-up. Set age_col to an age
measured at the intended anchor; the script
does not assume that AGE_AT_ENTRY is age at diagnosis.

Examples:
    ALS_CASE_TABLE1: Table1Columns(index_col="INDEX_DATE",
                                 age_col="AGE_AT_INDEX",
                                 ethnicity_col="HISPANIC", death_col="DEATH_IND")
    DM_CASE_TABLE1:  Table1Columns(index_col="DM1DX_DATE",
                                 age_col="AGE_AT_ENTRY",
                                 ethnicity_col="HISPANIC", death_col="DEATH_IND")
The DM example reports enrollment age but survival from DM diagnosis.

Input must contain one row per patient. All supplied rows are summarized without
ALS/DM flags or implicit cohort filters. Categories accept labels or PCORnet codes.
Missing categories are reported as Unknown; invalid follow-up rows are excluded
from KM with a warning. No valid survival observations yield blank summaries.
"""

import os
import logging
import math
from dataclasses import dataclass

from snowflake.snowpark import DataFrame, Session, functions as F
from snowflake.snowpark.types import IntegerType, StringType, StructField, StructType
from snowflake.snowpark.window import Window

from config import conn, env

logger = logging.getLogger(__name__)

# SOURCE_TABLE = "PAT_TABLE1_WINDOW"
# SOURCE_TABLE = "T2DM_CASE_TABLE1_WINDOW"
# SOURCE_TABLE = "ALS_CASE_TABLE1_WINDOW"
SOURCE_TABLE = "ALS_T2DM_CASE_TABLE1_WINDOW"
OUTPUT_TABLE = SOURCE_TABLE + "_SUMMARY"


@dataclass(frozen=True)
class Table1Columns:
    person_col: str = "PATID"
    index_col: str | None = None
    age_col: str | None = None
    sex_col: str = "SEX"
    race_col: str = "RACE"
    ethnicity_col: str = "ETHNICITY"
    followup_col: str | None = None
    censor_col: str = "CENSOR_DATE"
    death_col: str = "DEATH_FLAG"


SOURCE_COLUMNS = Table1Columns(
    index_col="INDEX_DATE",
    age_col="AGE_AT_INDEX",
    ethnicity_col="HISPANIC",
    death_col="DEATH_IND",
)


def format_n(n):
    return f"{int(n):,}" if n is not None else ""


def format_mean_sd(mean, sd, digits=1):
    if mean is None:
        return ""
    if sd is None:
        return f"{mean:.{digits}f}"
    return f"{mean:.{digits}f} ({sd:.{digits}f})"


def format_n_pct(n, denominator, digits=1):
    if not denominator:
        return ""
    n = 0 if n is None else n
    return f"{int(n):,} ({100.0 * n / denominator:.{digits}f}%)"


def format_survival(s):
    return "" if s is None else f"{100.0 * s:.1f}%"


def _category(column, mapping):
    value = F.upper(F.trim(column.cast("string")))
    expr = F.when(column.is_null(), F.lit("Unknown"))
    for label, codes in mapping.items():
        expr = expr.when(value.isin(*codes), F.lit(label))
    return expr.otherwise(F.lit("Unknown"))


def _patient_data(data: DataFrame, columns: Table1Columns) -> DataFrame:
    if not columns.age_col:
        raise ValueError("Configure an explicit age_col")
    if not columns.followup_col and not columns.index_col:
        raise ValueError("Configure followup_col, or index_col and censor_col")

    required = [
        columns.person_col, columns.sex_col, columns.race_col,
        columns.ethnicity_col, columns.death_col, columns.age_col,
    ]
    required += (
        [columns.followup_col]
        if columns.followup_col else [columns.index_col, columns.censor_col]
    )
    available = {name.upper().strip('"') for name in data.columns}
    missing = [name for name in dict.fromkeys(required) if name and name.upper() not in available]
    if missing:
        raise ValueError(f"Patient table is missing configured columns: {', '.join(missing)}")

    invalid_ids = (
        data.group_by(columns.person_col).agg(F.count("*").alias("_N"))
        .filter(F.col(columns.person_col).is_null() | (F.col("_N") > 1))
        .limit(1).collect()
    )
    if invalid_ids:
        raise ValueError("Patient table must have a non-null, unique patient ID per row")

    age = F.col(columns.age_col).cast("double")
    followup = (
        F.col(columns.followup_col).cast("double")
        if columns.followup_col else
        F.datediff("day", F.col(columns.index_col), F.col(columns.censor_col)).cast("double")
    )
    return data.select(
        age.alias("AGE"),
        _category(F.col(columns.sex_col), {
            "Female": ("F", "FEMALE"), "Male": ("M", "MALE"),
        }).alias("SEX"),
        _category(F.col(columns.race_col), {
            "White": ("05", "WHITE"), "Black": ("03", "BLACK", "BLACK OR AFRICAN AMERICAN"),
            "Asian": ("02", "ASIAN"),
            "Other": ("01", "04", "06", "07", "OTHER", "MULTIPLE RACE",
                      "AMERICAN INDIAN OR ALASKA NATIVE",
                      "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"),
        }).alias("RACE"),
        _category(F.col(columns.ethnicity_col), {
            "Hispanic/Latino": ("Y", "HISPANIC", "HISPANIC/LATINO"),
            "Non-Hispanic": ("N", "NON-HISPANIC"),
        }).alias("ETHNICITY"),
        followup.alias("FOLLOWUP_DAYS"),
        F.col(columns.death_col).cast("integer").alias("DEATH_FLAG"),
    )


def km_survival(data, time_points=(365, 1095, 1825)):
    """KM survival from configured index; ties include deaths before censoring."""
    surv = data.filter(
        F.col("FOLLOWUP_DAYS").is_not_null()
        & (F.col("FOLLOWUP_DAYS") >= 0)
        & F.col("DEATH_FLAG").isin(0, 1)
    ).select(
        F.col("FOLLOWUP_DAYS").cast("double").alias("TIME"),
        F.col("DEATH_FLAG").cast("integer").alias("EVENT"),
    )
    total_n = int(surv.count())
    excluded = data.count() - total_n
    if excluded:
        logger.warning("Excluded %s patients with invalid follow-up/death status from KM", excluded)
    if total_n == 0:
        return {t: None for t in time_points}

    by_time = surv.group_by("TIME").agg(
        F.count("*").alias("N_EXIT"), F.sum("EVENT").alias("D"),
    )
    w = Window.order_by("TIME").rows_between(Window.UNBOUNDED_PRECEDING, -1)
    by_time = by_time.with_column(
        "EXIT_BEFORE", F.coalesce(F.sum("N_EXIT").over(w), F.lit(0)),
    ).with_column("N_RISK", F.lit(total_n) - F.col("EXIT_BEFORE"))
    km_terms = by_time.filter(F.col("D") > 0).with_column(
        "KM_FACTOR", F.lit(1.0) - F.col("D") / F.nullif(F.col("N_RISK"), F.lit(0)),
    )
    results = {}
    for t in time_points:
        result = km_terms.filter(F.col("TIME") <= t).agg(
            F.sum(F.when(F.col("KM_FACTOR") <= 0, 1).otherwise(0)).alias("ZERO_COUNT"),
            F.sum(F.ln(
                F.when(F.col("KM_FACTOR") > 0, F.col("KM_FACTOR")).otherwise(F.lit(1.0))
            )).alias("LOG_SUM"),
        ).collect()[0]
        if result["ZERO_COUNT"] and result["ZERO_COUNT"] > 0:
            results[t] = 0.0
        elif result["LOG_SUM"] is None:
            results[t] = 1.0
        else:
            results[t] = math.exp(result["LOG_SUM"])
    return results


def generate_table1(
    session: Session,
    source_table: str | DataFrame,
    output_table: str | None = None,
    *,
    columns: Table1Columns,
) -> DataFrame:
    """Summarize one cohort; optionally overwrite output_table with the result."""
    data = session.table(source_table) if isinstance(source_table, str) else source_table
    df = _patient_data(data, columns)
    summary = df.agg(
        F.count("*").alias("N_TOTAL"), F.avg("AGE").alias("MEAN_AGE"),
        F.stddev_samp("AGE").alias("SD_AGE"),
    ).collect()[0]
    n_total = summary["N_TOTAL"]
    records = [
        [1, "N", format_n(n_total)],
        [2, "Age, years", format_mean_sd(summary["MEAN_AGE"], summary["SD_AGE"])],
    ]
    categories = (
        ("Sex", "SEX", ("Female", "Male", "Unknown")),
        ("Race", "RACE", ("White", "Black", "Asian", "Other", "Unknown")),
        ("Ethnicity", "ETHNICITY", ("Hispanic/Latino", "Non-Hispanic", "Unknown")),
    )
    for label, var, levels in categories:
        records.append([len(records) + 1, label, ""])
        lookup = {
            row[var]: row["N"]
            for row in df.group_by(var).agg(F.count("*").alias("N")).collect()
        }
        for level in levels:
            records.append([
                len(records) + 1, f"    {level}",
                format_n_pct(lookup.get(level, 0), n_total),
            ])
    survival = km_survival(df)
    records.append([len(records) + 1, "Survival", ""])
    for years, days in ((1, 365), (3, 1095), (5, 1825)):
        records.append([len(records) + 1, f"    {years}-year", format_survival(survival[days])])
    result = session.create_dataframe(records, schema=StructType([
        StructField("SORT_ORDER", IntegerType()),
        StructField("CHARACTERISTIC", StringType()),
        StructField("SUMMARY", StringType()),
    ]))
    if output_table is not None:
        result.write.mode("overwrite").save_as_table(output_table)
    return result.sort("SORT_ORDER")


if __name__ == "__main__":
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")

    generate_table1(
        session, SOURCE_TABLE, OUTPUT_TABLE, columns=SOURCE_COLUMNS,
    ).show(30)
