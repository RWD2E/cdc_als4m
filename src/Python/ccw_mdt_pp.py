import re
import uuid
import os
import logging
from typing import Optional, Sequence, Dict, List

from snowflake.snowpark import Session, DataFrame
from config import env, conn


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =====================================================================
# MDT COMPONENT DEFINITIONS
# =====================================================================

DEFAULT_MDT_COMPONENTS = {

    "REHAB": [
        "PhysicalTherapy",
        "OccupationalTherapy",
        "FunctionalCare",
        "MobilityDeviceWheelchair",
    ],

    "SLP": [
        "SpeechLanguageCare",
        "AssistiveDeviceCommunication",
    ],

    "RESPIRATORY": [
        "RespiratoryDeviceNIV",
    ],

    "NUTRITION": [
        "DietitianCare",
        "EnteralFeedingEquipment",
    ],

    "PSYCHOSOCIAL": [
        "PsychologicalCare",
    ],
}


# =====================================================================
# MDT STRATEGIES
#
# Minimum required composition:
#
# A = Neurology-centered
# B = A + Rehab
# C = A + SLP
# D = A + respiratory
# E = B + SLP
# F = B + respiratory
# G = E + nutrition
# H = F + nutrition
#
# These are nested minimum-composition strategies.
# =====================================================================

DEFAULT_STRATEGY_COMPONENTS = {

    "A": [],

    "B": [
        "REHAB",
    ],

    "C": [
        "SLP",
    ],

    "D": [
        "RESPIRATORY",
    ],

    "E": [
        "REHAB",
        "SLP",
    ],

    "F": [
        "REHAB",
        "RESPIRATORY",
    ],

    "G": [
        "REHAB",
        "SLP",
        "NUTRITION",
    ],

    "H": [
        "REHAB",
        "RESPIRATORY",
        "NUTRITION",
    ],
}


DEFAULT_STRATEGY_NAMES = {

    "A":
        "Neurology-centered care",

    "B":
        "Neurology + rehabilitation",

    "C":
        "Neurology + SLP",

    "D":
        "Neurology + respiratory",

    "E":
        "Neurology + rehabilitation + SLP",

    "F":
        "Neurology + rehabilitation + respiratory",

    "G":
        "Neurology + rehabilitation + SLP + nutrition",

    "H":
        "Neurology + rehabilitation + respiratory + nutrition",
}


# =====================================================================
# PHE_TYPE-BASED TIME-VARYING COVARIATES FOR IPCW
# =====================================================================

DEFAULT_IPCW_PHE_TYPES = [

    # Symptoms / function
    "SxMuscleMovement",
    "SxMuscleWeakness",
    "SxPainJointLimb",
    "SxSpeech",
    "SxSwallowing",
    "SxWalking",

    # ALS treatments
    "edaravone",
    "riluzole",
    "tofersen",

    # ALS phenotypes / related conditions
    "FTD",
    "PBA",
    "PBP",
    "PLS",
]


# =====================================================================
# BASELINE IPCW COVARIATES
# =====================================================================

DEFAULT_BASELINE_COVARIATES = [
    "AGE_AT_INDEX",
    "SEX",
    "RACE",
    "HISPANIC",
]


# =====================================================================
# LAB SOURCE TABLES
# =====================================================================

DEFAULT_LAB_TABLES = {

    "BMI":
        "GROUSE_ANALYTICS_DB.SX_ALS_GPC.BMI_LONG",

    "CBC":
        "GROUSE_ANALYTICS_DB.SX_ALS_GPC.CBC_LONG",

    "METABOLIC":
        "GROUSE_ANALYTICS_DB.SX_ALS_GPC.METABOLIC_LONG",
}


# =====================================================================
# MAIN FUNCTION
# =====================================================================

def create_mdt_ccw_table_fast(
    session: Session,

    # -----------------------------------------------------------------
    # INPUT / OUTPUT
    # -----------------------------------------------------------------

    patient_table: str = (
        "GROUSE_ANALYTICS_DB."
        "SX_ALS_GPC."
        "ALS_CASE_TABLE1"
    ),

    event_table: str = (
        "GROUSE_ANALYTICS_DB."
        "SX_ALS_GPC."
        "ALS_DXRXPX_LONG"
    ),

    output_table: str = (
        "GROUSE_ANALYTICS_DB."
        "SX_ALS_GPC."
        "ALS_MDT_CCW"
    ),

    # -----------------------------------------------------------------
    # LAB TABLES
    # -----------------------------------------------------------------

    lab_tables: Optional[
        Dict[str, str]
    ] = None,

    # Example:
    #
    # {
    #     "BMI": ["39156-5"],
    #     "CBC": ["718-7", "6690-2", "777-3"],
    #     "METABOLIC": ["2160-0", "1751-7"]
    # }
    #
    # None:
    #     automatically retrieve all OBS_CODE values in each table.
    #
    # {"BMI": None, "CBC": [...], ...}:
    #     all BMI codes, selected CBC codes, etc.
    #
    lab_obs_codes: Optional[
        Dict[str, Optional[Sequence[str]]]
    ] = None,

    # -----------------------------------------------------------------
    # CCW DESIGN
    # -----------------------------------------------------------------

    grace_period_days: int = 180,

    interval_days: int = 30,

    # None = follow through source CENSOR_DATE
    max_followup_days: Optional[int] = None,

    # -----------------------------------------------------------------
    # MDT / IPCW DEFINITIONS
    # -----------------------------------------------------------------

    mdt_components: Optional[
        Dict[str, Sequence[str]]
    ] = None,

    strategy_components: Optional[
        Dict[str, Sequence[str]]
    ] = None,

    strategy_names: Optional[
        Dict[str, str]
    ] = None,

    ipcw_phe_types: Optional[
        Sequence[str]
    ] = None,

    baseline_covariates: Optional[
        Sequence[str]
    ] = None,

    # -----------------------------------------------------------------
    # PATIENT TABLE COLUMN NAMES
    # -----------------------------------------------------------------

    patient_id_col: str = "PATID",

    index_date_col: str = "INDEX_DATE",

    censor_date_col: str = "CENSOR_DATE",

    death_col: str = "DEATH_IND",

    # -----------------------------------------------------------------
    # ALS_DXRXPX_LONG COLUMN NAMES
    # -----------------------------------------------------------------

    event_patient_id_col: str = "PATID",

    event_date_col: str = "CD_DATE",

    phe_type_col: str = "PHE_TYPE",

    # -----------------------------------------------------------------
    # LAB TABLE COLUMN NAMES
    # -----------------------------------------------------------------

    lab_patient_id_col: str = "PATID",

    lab_date_col: str = "OBS_DATE",

    lab_days_since_index_col: str = "DAYS_SINCE_INDEX",

    lab_code_col: str = "OBS_NAME",

    lab_value_col: str = "LAB_VALUE",

    # -----------------------------------------------------------------
    # OUTPUT / DEBUG
    # -----------------------------------------------------------------

    overwrite: bool = True,

    keep_staging_tables: bool = False,

) -> DataFrame:
    """
    Build an optimized clone-censor-weight person-period dataset
    comparing multiple MDT strategies in ALS.

    ================================================================
    STRATEGIES
    ================================================================

    A:
        Neurology-centered care.

    B:
        Rehabilitation + SLP.

    C:
        B + respiratory.

    D:
        C + nutrition.

    E:
        D + psychosocial/palliative support.

    Strategies are treated as minimum required compositions.

    ================================================================
    TIME ZERO
    ================================================================

    ALS1DX_DATE.

    ================================================================
    OUTCOME
    ================================================================

    Overall survival.

        DEATH_IND = 1:
            CENSOR_DATE is treated as death date.

        DEATH_IND = 0:
            CENSOR_DATE is treated as right-censoring date.

    ================================================================
    GRACE PERIOD
    ================================================================

    Each patient is cloned into strategies A-H.

    For B-E, all required MDT components must occur within the
    configurable grace period.

    Failure results in artificial censoring at GRACE_END_DATE.

    ================================================================
    PHENOTYPE IPCW FEATURES
    ================================================================

    From ALS_DXRXPX_LONG:

        IPCW_<PHE>_PRIOR

    These use only records strictly before the current interval.

    ================================================================
    LAB IPCW FEATURES
    ================================================================

    From:

        BMI_LONG
        CBC_LONG
        METABOLIC_LONG

    For each selected OBS_CODE:

        IPCW_BMI_<CODE>_PRIOR_LAST

        IPCW_CBC_<CODE>_PRIOR_LAST

        IPCW_METABOLIC_<CODE>_PRIOR_LAST

    Each value is the MOST RECENT measurement strictly before
    INTERVAL_START.

    Current-interval measurements are intentionally excluded.

    ================================================================
    PERFORMANCE
    ================================================================

    Raw longitudinal data are reduced BEFORE cloning:

        raw phenotype events
             ↓
        PATID × interval counts

        raw labs
             ↓
        last PATID × interval × OBS_CODE value

             ↓

        PATID × interval feature table

             ↓

        clone into A-E

    Thus the large source tables are never joined separately to all
    five strategy clones.
    """

    # =================================================================
    # DEFAULTS
    # =================================================================

    if mdt_components is None:
        mdt_components = DEFAULT_MDT_COMPONENTS

    if strategy_components is None:
        strategy_components = DEFAULT_STRATEGY_COMPONENTS

    if strategy_names is None:
        strategy_names = DEFAULT_STRATEGY_NAMES

    if ipcw_phe_types is None:
        ipcw_phe_types = DEFAULT_IPCW_PHE_TYPES

    if baseline_covariates is None:
        baseline_covariates = DEFAULT_BASELINE_COVARIATES

    if lab_tables is None:
        lab_tables = DEFAULT_LAB_TABLES

    logger.info(
        "Starting CCW build: patient_table=%s event_table=%s output_table=%s grace_days=%s interval_days=%s max_followup_days=%s",
        patient_table,
        event_table,
        output_table,
        grace_period_days,
        interval_days,
        max_followup_days,
    )


    mdt_components = {
        k: list(v)
        for k, v in mdt_components.items()
    }

    strategy_components = {
        k: list(v)
        for k, v in strategy_components.items()
    }

    ipcw_phe_types = list(
        ipcw_phe_types
    )

    baseline_covariates = list(
        baseline_covariates
    )


    # =================================================================
    # HELPERS
    # =================================================================

    def validate_identifier(
        identifier: str
    ) -> None:

        pattern = re.compile(
            r"^[A-Za-z_][A-Za-z0-9_$]*$"
        )

        for part in identifier.split("."):

            if not pattern.match(part):

                raise ValueError(
                    "Invalid Snowflake identifier: "
                    f"{identifier}"
                )


    def sql_literal(
        value: str
    ) -> str:

        return (
            "'"
            + value.replace("'", "''")
            + "'"
        )


    def safe_alias(
        value: str
    ) -> str:

        value = re.sub(
            r"[^A-Za-z0-9]+",
            "_",
            str(value)
        )

        value = value.strip("_").upper()

        if not value:
            value = "UNKNOWN"

        if value[0].isdigit():
            value = "_" + value

        return value[:100]


    # =================================================================
    # VALIDATION
    # =================================================================

    identifiers = [

        patient_table,
        event_table,
        output_table,

        patient_id_col,
        index_date_col,
        censor_date_col,
        death_col,

        event_patient_id_col,
        event_date_col,
        phe_type_col,

        lab_patient_id_col,
        lab_date_col,
        lab_days_since_index_col,
        lab_code_col,
        lab_value_col,
    ]


    identifiers.extend(
        lab_tables.values()
    )

    identifiers.extend(
        baseline_covariates
    )


    for identifier in identifiers:
        validate_identifier(identifier)

    logger.info("Identifier validation complete for %s fields.", len(identifiers))


    if grace_period_days < 0:

        raise ValueError(
            "grace_period_days must be >= 0."
        )


    if interval_days <= 0:

        raise ValueError(
            "interval_days must be > 0."
        )


    if (
        max_followup_days is not None
        and max_followup_days <= 0
    ):

        raise ValueError(
            "max_followup_days must be > 0 or None."
        )


    strategy_order = [
        "A",
        "B",
        "C",
        "D",
        "E",
        "F",
        "G",
        "H",
    ]


    for strategy in strategy_order:

        if strategy not in strategy_components:

            raise ValueError(
                f"Missing strategy: {strategy}"
            )


    # =================================================================
    # RELEVANT PHE_TYPE VALUES
    # =================================================================

    mdt_phe_types = []

    for phe_list in mdt_components.values():

        mdt_phe_types.extend(
            phe_list
        )


    mdt_phe_types = list(
        dict.fromkeys(
            mdt_phe_types
        )
    )


    all_phe_types = list(
        dict.fromkeys(
            mdt_phe_types
            + ipcw_phe_types
        )
    )


    all_phe_sql = ", ".join(
        sql_literal(x)
        for x in all_phe_types
    )


    # =================================================================
    # TEMPORARY TABLE NAMES
    # =================================================================

    run_id = (
        uuid.uuid4()
        .hex[:10]
        .upper()
    )


    if output_table.count(".") == 2:

        output_db, output_schema, _ = (
            output_table.split(".")
        )

        temp_prefix = (
            f"{output_db}."
            f"{output_schema}."
            f"TMP_MDT_CCW_{run_id}"
        )

    else:

        temp_prefix = (
            f"TMP_MDT_CCW_{run_id}"
        )


    tmp_patients = (
        f"{temp_prefix}_PATIENTS"
    )

    tmp_events = (
        f"{temp_prefix}_EVENTS"
    )

    tmp_event_intervals = (
        f"{temp_prefix}_EVENT_INTERVALS"
    )

    tmp_mdt_dates = (
        f"{temp_prefix}_MDT_DATES"
    )

    tmp_patient_intervals = (
        f"{temp_prefix}_PATIENT_INTERVALS"
    )

    tmp_interval_features = (
        f"{temp_prefix}_FEATURES"
    )

    tmp_clones = (
        f"{temp_prefix}_CLONES"
    )

    tmp_bmi_baseline = (
        f"{temp_prefix}_BMI_BASELINE"
    )

    logger.info("Using temporary table prefix: %s", temp_prefix)


    staging_tables = [
        tmp_patients,
        tmp_events,
        tmp_event_intervals,
        tmp_mdt_dates,
        tmp_patient_intervals,
        tmp_interval_features,
        tmp_clones,
        tmp_bmi_baseline,
    ]


    # =================================================================
    # ANALYTIC FOLLOW-UP END
    # =================================================================

    if max_followup_days is None:

        analytic_end_expression = (
            f"p.{censor_date_col}"
        )

    else:

        analytic_end_expression = f"""
            LEAST(
                p.{censor_date_col},

                DATEADD(
                    day,
                    {int(max_followup_days)},
                    p.{index_date_col}
                )
            )
        """


    # =================================================================
    # CREATE PATIENT TABLE FIRST
    # =================================================================

    session.sql(
        f"""
        CREATE OR REPLACE TEMPORARY TABLE
            {tmp_patients}
        AS

        SELECT

            p.*,

            {analytic_end_expression}
                AS ANALYTIC_END_DATE,

            DATEADD(
                day,
                {grace_period_days},
                p.{index_date_col}
            ) AS GRACE_END_DATE

        FROM {patient_table} p

        WHERE
            p.{index_date_col} IS NOT NULL

            AND p.{censor_date_col} IS NOT NULL

            AND p.{censor_date_col}
                >= p.{index_date_col}
        """
    ).collect()

    logger.info("Created patient staging table: %s", tmp_patients)


    # =================================================================
    # DETERMINE LAB OBS_CODE LISTS
    # =================================================================

    resolved_lab_codes = {}


    for source_name, table_name in lab_tables.items():

        # -------------------------------------------------------------
        # Explicitly provided list
        # -------------------------------------------------------------

        if (
            lab_obs_codes is not None
            and source_name in lab_obs_codes
            and lab_obs_codes[source_name] is not None
        ):

            codes = list(
                lab_obs_codes[source_name]
            )

        else:

            if source_name.upper() == "BMI":
                codes = ["BMI"]
                resolved_lab_codes[
                    source_name
                ] = codes

                logger.info(
                    "Resolved %s lab codes for source %s (%s)",
                    len(codes),
                    source_name,
                    table_name,
                )
                continue

            # ---------------------------------------------------------
            # Auto-discover codes among eligible patients.
            #
            # Explicit lists are preferable for production if there
            # are very many distinct OBS_CODE values.
            # ---------------------------------------------------------

            code_df = session.sql(
                f"""
                SELECT DISTINCT
                    l.{lab_code_col} AS OBS_CODE

                FROM {table_name} l

                INNER JOIN {tmp_patients} p

                    ON l.{lab_patient_id_col}
                        = p.{patient_id_col}

                WHERE
                    l.{lab_code_col} IS NOT NULL

                    AND l.{lab_value_col} IS NOT NULL

                    AND l.{lab_date_col}
                        <= p.ANALYTIC_END_DATE

                ORDER BY OBS_CODE
                """
            )


            codes = [
                row["OBS_CODE"]
                for row
                in code_df.collect()
            ]


        resolved_lab_codes[
            source_name
        ] = codes

        logger.info(
            "Resolved %s lab codes for source %s (%s)",
            len(codes),
            source_name,
            table_name,
        )


    # =================================================================
    # CHECK LAB COLUMN ALIAS COLLISIONS
    # =================================================================

    seen_lab_aliases = set()


    for source_name, codes in (
        resolved_lab_codes.items()
    ):

        for code in codes:

            alias = (
                f"{safe_alias(source_name)}_"
                f"{safe_alias(code)}"
            )

            if alias in seen_lab_aliases:

                raise ValueError(
                    "Duplicate generated lab alias: "
                    f"{alias}"
                )

            seen_lab_aliases.add(
                alias
            )

    logger.info("Lab alias collision check complete: %s aliases.", len(seen_lab_aliases))

    # -------------------------------------------------------------
    # Baseline BMI nearest to index, used for BMI_CHGPCT covariate.
    # -------------------------------------------------------------
    if "BMI" in lab_tables:
        bmi_table = lab_tables["BMI"]
        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_bmi_baseline}
            AS

            SELECT
                x.{lab_patient_id_col} AS {patient_id_col},
                x.BMI AS BMI_BASELINE
            FROM (
                SELECT
                    l.{lab_patient_id_col},
                    l.BMI,
                    l.{lab_days_since_index_col},
                    l.{lab_date_col},
                    ROW_NUMBER() OVER (
                        PARTITION BY l.{lab_patient_id_col}
                        ORDER BY ABS(l.{lab_days_since_index_col}), l.{lab_date_col}
                    ) AS rn
                FROM {bmi_table} l
                INNER JOIN {tmp_patients} p
                    ON l.{lab_patient_id_col} = p.{patient_id_col}
                WHERE l.BMI IS NOT NULL
                    AND l.{lab_date_col} <= p.ANALYTIC_END_DATE
            ) x
            WHERE x.rn = 1
            """
        ).collect()
        logger.info("Created BMI baseline table: %s", tmp_bmi_baseline)


    # =================================================================
    # DYNAMIC EVENT AGGREGATION EXPRESSIONS
    # =================================================================

    event_count_expressions = []


    for phe in ipcw_phe_types:

        alias = safe_alias(phe)

        literal = sql_literal(phe)

        event_count_expressions.append(
            f"""
            COUNT_IF(
                PHE_TYPE = {literal}
            ) AS N_{alias}
            """
        )


    for component, phe_list in (
        mdt_components.items()
    ):

        component_alias = safe_alias(
            component
        )

        values = ", ".join(
            sql_literal(x)
            for x in phe_list
        )

        event_count_expressions.append(
            f"""
            COUNT_IF(
                PHE_TYPE IN ({values})
            ) AS N_MDT_{component_alias}
            """
        )


    event_count_sql = ",\n".join(
        event_count_expressions
    )


    # =================================================================
    # CUMULATIVE PHE_TYPE FEATURES
    # =================================================================

    event_prior_expressions = []


    for phe in ipcw_phe_types:

        alias = safe_alias(phe)


        # -------------------------------------------------------------
        # Ever occurred strictly before interval
        # -------------------------------------------------------------

        event_prior_expressions.append(
            f"""
            IFF(
                COALESCE(
                    SUM(
                        N_{alias}
                    ) OVER (

                        PARTITION BY
                            {patient_id_col}

                        ORDER BY
                            INTERVAL

                        ROWS BETWEEN
                            UNBOUNDED PRECEDING
                            AND 1 PRECEDING
                    ),
                    0
                ) > 0,
                1,
                0
            ) AS IPCW_{alias}_PRIOR
            """
        )


    # =================================================================
    # MDT PRIOR HISTORY FEATURES
    # =================================================================

    mdt_prior_expressions = []


    for component in mdt_components:

        alias = safe_alias(
            component
        )


        mdt_prior_expressions.append(
            f"""
            IFF(
                COALESCE(
                    SUM(
                        N_MDT_{alias}
                    ) OVER (

                        PARTITION BY
                            {patient_id_col}

                        ORDER BY
                            INTERVAL

                        ROWS BETWEEN
                            UNBOUNDED PRECEDING
                            AND 1 PRECEDING
                    ),
                    0
                ) > 0,
                1,
                0
            ) AS MDT_{alias}_PRIOR
            """
        )

    # =================================================================
    # MDT FIRST-DATE EXPRESSIONS
    # =================================================================

    mdt_first_date_expressions = []


    for component, phe_list in (
        mdt_components.items()
    ):

        alias = safe_alias(
            component
        )

        values = ", ".join(
            sql_literal(x)
            for x in phe_list
        )


        mdt_first_date_expressions.append(
            f"""
            MIN(
                IFF(
                    PHE_TYPE IN ({values})

                    AND CD_DATE
                        BETWEEN
                            {index_date_col}
                            AND GRACE_END_DATE,

                    CD_DATE,

                    NULL
                )
            ) AS {alias}_FIRST_DATE
            """
        )


    mdt_first_date_sql = ",\n".join(
        mdt_first_date_expressions
    )


    # =================================================================
    # STRATEGY TABLE
    # =================================================================

    strategy_rows = []


    for level, strategy in enumerate(
        strategy_order,
        start=1
    ):

        strategy_rows.append(
            f"""
            SELECT

                '{strategy}'
                    AS STRATEGY,

                {level}
                    AS STRATEGY_LEVEL,

                {sql_literal(strategy_names[strategy])}
                    AS STRATEGY_NAME
            """
        )


    strategy_sql = (
        "\nUNION ALL\n"
        .join(strategy_rows)
    )


    # =================================================================
    # STRATEGY ADHERENCE / COMPLETION
    # =================================================================

    adherence_cases = []

    completion_cases = []


    for strategy in strategy_order:

        components = (
            strategy_components[
                strategy
            ]
        )


        # -------------------------------------------------------------
        # Strategy A assumed satisfied by cohort construction
        # -------------------------------------------------------------

        if not components:

            adherence_cases.append(
                f"""
                WHEN '{strategy}'
                    THEN 1
                """
            )

            completion_cases.append(
                f"""
                WHEN '{strategy}'
                    THEN p.{index_date_col}
                """
            )

            continue


        date_columns = [

            (
                "m."
                + safe_alias(component)
                + "_FIRST_DATE"
            )

            for component in components
        ]


        all_present = " AND ".join(

            f"{column} IS NOT NULL"

            for column in date_columns
        )


        completion_date = (
            "GREATEST("
            + ", ".join(date_columns)
            + ")"
        )


        adherence_cases.append(
            f"""
            WHEN '{strategy}'
                THEN IFF(
                    {all_present},
                    1,
                    0
                )
            """
        )


        completion_cases.append(
            f"""
            WHEN '{strategy}'
                THEN CASE

                    WHEN {all_present}
                        THEN {completion_date}

                    ELSE NULL

                END
            """
        )


    adherence_case_sql = (
        "\n".join(
            adherence_cases
        )
    )


    completion_case_sql = (
        "\n".join(
            completion_cases
        )
    )


    # =================================================================
    # NUMBER OF FOLLOW-UP INTERVALS
    # =================================================================

    if max_followup_days is not None:

        max_intervals = (
            int(max_followup_days)
            // interval_days
        ) + 2

    else:

        max_interval_query = f"""
            SELECT

                COALESCE(
                    MAX(
                        FLOOR(
                            DATEDIFF(
                                day,
                                {index_date_col},
                                {censor_date_col}
                            )
                            / {float(interval_days)}
                        )
                    ),
                    0
                ) + 2 AS N

            FROM {patient_table}

            WHERE
                {index_date_col} IS NOT NULL

                AND {censor_date_col} IS NOT NULL

                AND {censor_date_col}
                    >= {index_date_col}
        """


        max_intervals = int(

            session
            .sql(max_interval_query)
            .collect()[0]["N"]

        )


    # =================================================================
    # START STAGED PIPELINE
    # =================================================================

    try:

        # =============================================================
        # 1. FILTER ALS_DXRXPX_LONG ONCE
        # =============================================================

        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_events}
            AS

            SELECT

                e.{event_patient_id_col}
                    AS {patient_id_col},

                e.{event_date_col}
                    AS CD_DATE,

                e.{phe_type_col}
                    AS PHE_TYPE,

                p.{index_date_col},

                p.GRACE_END_DATE,


                /* --------------------------------------------------
                   Interval -1 contains ALL pre-index history.
                   -------------------------------------------------- */

                GREATEST(
                    -1,

                    FLOOR(
                        DATEDIFF(
                            day,
                            p.{index_date_col},
                            e.{event_date_col}
                        )
                        / {float(interval_days)}
                    )
                )::INTEGER
                    AS EVENT_INTERVAL


            FROM {event_table} e


            INNER JOIN {tmp_patients} p

                ON e.{event_patient_id_col}
                    = p.{patient_id_col}


            WHERE

                e.{event_date_col}
                    IS NOT NULL

                AND e.{phe_type_col}
                    IN ({all_phe_sql})

                AND e.{event_date_col}
                    <= p.ANALYTIC_END_DATE
            """
        ).collect()

        logger.info("Stage 1 complete: filtered events table %s", tmp_events)


        # =============================================================
        # 2. COLLAPSE EVENT DATA TO PATID × INTERVAL
        # =============================================================

        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_event_intervals}
            AS

            SELECT

                {patient_id_col},

                EVENT_INTERVAL
                    AS INTERVAL,

                {event_count_sql}

            FROM {tmp_events}

            GROUP BY
                {patient_id_col},
                EVENT_INTERVAL
            """
        ).collect()

        logger.info("Stage 2 complete: event intervals table %s", tmp_event_intervals)


        # =============================================================
        # 3. DETERMINE MDT COMPONENT DATES
        # =============================================================

        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_mdt_dates}
            AS

            SELECT

                {patient_id_col},

                {mdt_first_date_sql}

            FROM {tmp_events}

            GROUP BY
                {patient_id_col}
            """
        ).collect()

        logger.info("Stage 3 complete: MDT first-date table %s", tmp_mdt_dates)


        # =============================================================
        # 4. PREPARE LAB TABLES
        #
        # One staging table per lab source.
        #
        # Raw:
        #
        # PATID × many observations
        #
        # becomes:
        #
        # PATID × INTERVAL × OBS_CODE × LAST VALUE
        # =============================================================

        lab_interval_tables = {}

        lab_pivot_tables = {}


        for (
            source_name,
            source_table
        ) in lab_tables.items():

            source_alias = safe_alias(
                source_name
            )

            tmp_lab_interval = (
                f"{temp_prefix}_"
                f"{source_alias}_INTERVAL"
            )

            tmp_lab_pivot = (
                f"{temp_prefix}_"
                f"{source_alias}_PIVOT"
            )


            lab_interval_tables[
                source_name
            ] = tmp_lab_interval

            lab_pivot_tables[
                source_name
            ] = tmp_lab_pivot


            staging_tables.extend([
                tmp_lab_interval,
                tmp_lab_pivot,
            ])


            codes = (
                resolved_lab_codes[
                    source_name
                ]
            )


            # ---------------------------------------------------------
            # No eligible codes -> create empty-compatible staging table
            # ---------------------------------------------------------

            if not codes:

                session.sql(
                    f"""
                    CREATE OR REPLACE TEMPORARY TABLE
                        {tmp_lab_interval}
                    AS

                    SELECT

                        CAST(NULL AS VARCHAR)
                            AS {patient_id_col},

                        CAST(NULL AS INTEGER)
                            AS INTERVAL,

                        CAST(NULL AS VARCHAR)
                            AS OBS_CODE,

                        CAST(NULL AS FLOAT)
                            AS LAB_VALUE,

                        CAST(NULL AS DATE)
                            AS OBS_DATE

                    WHERE 1 = 0
                    """
                ).collect()


                session.sql(
                    f"""
                    CREATE OR REPLACE TEMPORARY TABLE
                        {tmp_lab_pivot}
                    AS

                    SELECT
                        {patient_id_col},
                        INTERVAL

                    FROM {tmp_lab_interval}
                    """
                ).collect()

                logger.info(
                    "Stage 4 source %s: no codes resolved, created empty lab interval/pivot tables.",
                    source_name,
                )

                continue


            code_sql = ", ".join(
                sql_literal(x)
                for x in codes
            )

            is_bmi_source = source_name.upper() == "BMI"


            # ---------------------------------------------------------
            # Last observation WITHIN each interval.
            #
            # All pre-index history maps to interval -1.
            #
            # QUALIFY keeps exactly one latest value per
            # PATID × INTERVAL × OBS_CODE.
            # ---------------------------------------------------------

            session.sql(
                f"""
                CREATE OR REPLACE TEMPORARY TABLE
                    {tmp_lab_interval}
                AS

                WITH source AS (

                    SELECT

                        l.{lab_patient_id_col}
                            AS {patient_id_col},

                        l.{lab_date_col}
                            AS OBS_DATE,

                        {"'BMI'" if is_bmi_source else f"l.{lab_code_col}"}
                            AS OBS_CODE,

                        {"l.BMI" if is_bmi_source else f"l.{lab_value_col}"}
                            AS LAB_VALUE,


                        GREATEST(
                            -1,

                            FLOOR(
                                l.{lab_days_since_index_col}
                                / {float(interval_days)}
                            )
                        )::INTEGER
                            AS INTERVAL


                    FROM {source_table} l


                    INNER JOIN {tmp_patients} p

                        ON l.{lab_patient_id_col}
                            = p.{patient_id_col}


                    WHERE

                        l.{lab_date_col}
                            IS NOT NULL

                        {"" if is_bmi_source else f"AND l.{lab_code_col} IS NOT NULL"}

                        AND {"l.BMI" if is_bmi_source else f"l.{lab_value_col}"}
                            IS NOT NULL

                        {"" if is_bmi_source else f"AND l.{lab_code_col} IN ({code_sql})"}

                        AND l.{lab_date_col}
                            <= p.ANALYTIC_END_DATE

                )


                SELECT

                    {patient_id_col},

                    INTERVAL,

                    OBS_CODE,

                    LAB_VALUE,

                    OBS_DATE


                FROM source


                QUALIFY

                    ROW_NUMBER() OVER (

                        PARTITION BY
                            {patient_id_col},
                            INTERVAL,
                            OBS_CODE

                        ORDER BY
                            OBS_DATE DESC

                    ) = 1
                """
            ).collect()


            # ---------------------------------------------------------
            # Pivot selected codes AFTER reducing to one observation
            # per interval.
            #
            # Columns remain sparse here.
            # ---------------------------------------------------------

            pivot_expressions = []


            for code in codes:

                code_alias = (
                    f"{source_alias}_"
                    f"{safe_alias(code)}"
                )

                literal = sql_literal(
                    code
                )


                pivot_expressions.append(
                    f"""
                    MAX(
                        IFF(
                            OBS_CODE = {literal},
                            LAB_VALUE,
                            NULL
                        )
                    ) AS CUR_{code_alias}
                    """
                )


            pivot_sql = ",\n".join(
                pivot_expressions
            )


            session.sql(
                f"""
                CREATE OR REPLACE TEMPORARY TABLE
                    {tmp_lab_pivot}
                AS

                SELECT

                    {patient_id_col},

                    INTERVAL,

                    {pivot_sql}

                FROM {tmp_lab_interval}

                GROUP BY
                    {patient_id_col},
                    INTERVAL
                """
            ).collect()

            logger.info(
                "Stage 4 source %s complete: interval table %s and pivot table %s",
                source_name,
                tmp_lab_interval,
                tmp_lab_pivot,
            )


        # =============================================================
        # 5. CREATE ONE DENSE PATIENT × INTERVAL SCAFFOLD
        #
        # Do this BEFORE cloning.
        # =============================================================

        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_patient_intervals}
            AS

            WITH interval_sequence AS (

                SELECT

                    ROW_NUMBER()
                        OVER (
                            ORDER BY SEQ4()
                        ) - 2
                        AS INTERVAL

                FROM TABLE(
                    GENERATOR(
                        ROWCOUNT => {max_intervals}
                    )
                )
            )


            SELECT

                p.{patient_id_col},

                s.INTERVAL,

                CASE

                    WHEN s.INTERVAL = -1

                    THEN DATEADD(
                        day,
                        -{interval_days},
                        p.{index_date_col}
                    )

                    ELSE DATEADD(
                        day,
                        s.INTERVAL
                            * {interval_days},
                        p.{index_date_col}
                    )

                END AS INTERVAL_START,

                p.ANALYTIC_END_DATE


            FROM {tmp_patients} p

            CROSS JOIN interval_sequence s


            WHERE

                s.INTERVAL = -1

                OR DATEADD(
                    day,
                    s.INTERVAL
                        * {interval_days},
                    p.{index_date_col}
                )
                <= p.ANALYTIC_END_DATE
            """
        ).collect()

        logger.info("Stage 5 complete: patient interval scaffold %s", tmp_patient_intervals)


        # =============================================================
        # 6. BUILD DENSE INTERVAL JOIN
        #
        # Join:
        #   event aggregates
        #   BMI pivot
        #   CBC pivot
        #   metabolic pivot
        #
        # still BEFORE cloning.
        # =============================================================

        dense_select_expressions = []

        dense_join_sql = []


        # -------------------------------------------------------------
        # Phenotype/event interval counts
        # -------------------------------------------------------------

        for phe in ipcw_phe_types:

            alias = safe_alias(
                phe
            )

            dense_select_expressions.append(
                f"""
                COALESCE(
                    ev.N_{alias},
                    0
                ) AS N_{alias}
                """
            )


        for component in mdt_components:

            alias = safe_alias(
                component
            )

            dense_select_expressions.append(
                f"""
                COALESCE(
                    ev.N_MDT_{alias},
                    0
                ) AS N_MDT_{alias}
                """
            )


        # -------------------------------------------------------------
        # Lab current-interval sparse columns
        # -------------------------------------------------------------

        for source_name in lab_tables:

            source_alias = safe_alias(
                source_name
            )

            codes = (
                resolved_lab_codes[
                    source_name
                ]
            )

            pivot_table = (
                lab_pivot_tables[
                    source_name
                ]
            )

            join_alias = (
                f"L_{source_alias}"
            )


            dense_join_sql.append(
                f"""
                LEFT JOIN {pivot_table}
                    {join_alias}

                    ON i.{patient_id_col}
                        = {join_alias}.{patient_id_col}

                    AND i.INTERVAL
                        = {join_alias}.INTERVAL
                """
            )


            for code in codes:

                code_alias = (
                    f"{source_alias}_"
                    f"{safe_alias(code)}"
                )

                dense_select_expressions.append(
                    f"""
                    {join_alias}.CUR_{code_alias}
                        AS CUR_{code_alias}
                    """
                )


        dense_join_clause = "\n".join(
            dense_join_sql
        )

        # Add baseline BMI to dense table for BMI_CHGPCT derivation.
        if "BMI" in lab_tables:
            dense_select_expressions.append(
                """
                b.BMI_BASELINE AS BMI_BASELINE
                """
            )
            dense_join_clause += f"""
                LEFT JOIN {tmp_bmi_baseline} b
                    ON i.{patient_id_col} = b.{patient_id_col}
            """

        dense_select_sql = ",\n".join(
            dense_select_expressions
        )


        # =============================================================
        # 7. MOST-RECENT PRIOR LAB EXPRESSIONS
        #
        # LAST_VALUE ... IGNORE NULLS
        #
        # frame ends at 1 PRECEDING.
        #
        # Thus:
        #
        # current interval value is NEVER used for the current
        # interval's IPCW model.
        # =============================================================

        lab_prior_expressions = []


        for source_name in lab_tables:

            source_alias = safe_alias(
                source_name
            )

            codes = (
                resolved_lab_codes[
                    source_name
                ]
            )


            for code in codes:

                code_alias = (
                    f"{source_alias}_"
                    f"{safe_alias(code)}"
                )


                lab_prior_expressions.append(
                    f"""
                    LAST_VALUE(
                        CUR_{code_alias}
                    ) IGNORE NULLS

                    OVER (

                        PARTITION BY
                            {patient_id_col}

                        ORDER BY
                            INTERVAL

                        ROWS BETWEEN
                            UNBOUNDED PRECEDING
                            AND 1 PRECEDING

                    )
                    AS IPCW_{code_alias}_PRIOR_LAST
                    """
                )

        if "BMI" in lab_tables:
            lab_prior_expressions.append(
                """
                IFF(
                    BMI_BASELINE IS NOT NULL
                    AND BMI_BASELINE != 0
                    AND LAST_VALUE(
                        CUR_BMI_BMI
                    ) IGNORE NULLS
                    OVER (
                        PARTITION BY
                            PATID
                        ORDER BY
                            INTERVAL
                        ROWS BETWEEN
                            UNBOUNDED PRECEDING
                            AND 1 PRECEDING
                    ) IS NOT NULL,
                    (
                        (
                            BMI_BASELINE
                            - LAST_VALUE(
                                CUR_BMI_BMI
                              ) IGNORE NULLS
                              OVER (
                                  PARTITION BY PATID
                                  ORDER BY INTERVAL
                                  ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                              )
                        )
                        / BMI_BASELINE
                    ) * 100,
                    NULL
                ) AS BMI_CHGPCT
                """
            )

        # -------------------------------------------------------------
        # Panel-level prior existence indicators
        # -------------------------------------------------------------
        for panel_name, panel_indicator in [
            ("CBC", "IPCW_CBC_ANY_PRIOR"),
            ("METABOLIC", "IPCW_METABOLIC_ANY_PRIOR"),
        ]:
            panel_source = next(
                (k for k in lab_tables.keys() if k.upper() == panel_name),
                None,
            )

            if panel_source is None:
                lab_prior_expressions.append(
                    f"""
                    0 AS {panel_indicator}
                    """
                )
                continue

            panel_codes = resolved_lab_codes.get(panel_source, [])
            if len(panel_codes) == 0:
                lab_prior_expressions.append(
                    f"""
                    0 AS {panel_indicator}
                    """
                )
                continue

            panel_alias = safe_alias(panel_source)
            prior_any_condition = " OR ".join(
                [
                    f"CUR_{panel_alias}_{safe_alias(code)} IS NOT NULL"
                    for code in panel_codes
                ]
            )

            lab_prior_expressions.append(
                f"""
                IFF(
                    COALESCE(
                        SUM(
                            IFF(
                                {prior_any_condition},
                                1,
                                0
                            )
                        ) OVER (
                            PARTITION BY
                                {patient_id_col}
                            ORDER BY
                                INTERVAL
                            ROWS BETWEEN
                                UNBOUNDED PRECEDING
                                AND 1 PRECEDING
                        ),
                        0
                    ) > 0,
                    1,
                    0
                ) AS {panel_indicator}
                """
            )


        # =============================================================
        # 8. CREATE PATIENT-INTERVAL FEATURE TABLE
        # =============================================================

        all_feature_expressions = (

            event_prior_expressions
            + mdt_prior_expressions
            + lab_prior_expressions

        )


        feature_sql = ",\n".join(
            all_feature_expressions
        )


        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_interval_features}
            AS

            WITH dense AS (

                SELECT

                    i.{patient_id_col},

                    i.INTERVAL,

                    i.INTERVAL_START,

                    i.ANALYTIC_END_DATE,

                    {dense_select_sql}


                FROM {tmp_patient_intervals} i


                LEFT JOIN {tmp_event_intervals} ev

                    ON i.{patient_id_col}
                        = ev.{patient_id_col}

                    AND i.INTERVAL
                        = ev.INTERVAL


                {dense_join_clause}
            )


            SELECT

                {patient_id_col},

                INTERVAL,

                INTERVAL_START,

                ANALYTIC_END_DATE,

                {feature_sql}


            FROM dense
            """
        ).collect()

        logger.info("Stage 8 complete: interval feature table %s", tmp_interval_features)


        # =============================================================
        # 9. CREATE A-H CLONES
        #
        # Cloning happens only now, after all longitudinal features
        # have been calculated.
        # =============================================================

        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_clones}
            AS

            WITH strategies AS (

                {strategy_sql}

            ),


            clones0 AS (

                SELECT

                    p.*,

                    s.STRATEGY,

                    s.STRATEGY_LEVEL,

                    s.STRATEGY_NAME,


                    CONCAT(
                        p.{patient_id_col},
                        '_',
                        s.STRATEGY
                    ) AS CLONE_ID,


                    /* ----------------------------------------------
                       Strategy adherence by grace end
                       ---------------------------------------------- */

                    CASE s.STRATEGY

                        {adherence_case_sql}

                    END
                        AS ADHERENT_AT_GRACE_END,


                    /* ----------------------------------------------
                       Date at which all required components had
                       occurred
                       ---------------------------------------------- */

                    CASE s.STRATEGY

                        {completion_case_sql}

                    END
                        AS STRATEGY_COMPLETION_DATE


                FROM {tmp_patients} p


                LEFT JOIN {tmp_mdt_dates} m

                    ON p.{patient_id_col}
                        = m.{patient_id_col}


                CROSS JOIN strategies s
            ),


            clones1 AS (

                SELECT

                    *,

                    CASE

                        WHEN STRATEGY = 'A'
                            THEN NULL


                        WHEN ADHERENT_AT_GRACE_END = 0
                            THEN GRACE_END_DATE


                        ELSE NULL

                    END AS RAW_ARTIFICIAL_CENSOR_DATE


                FROM clones0
            )


            SELECT

                *,


                /* ----------------------------------------------
                   Resolve natural end versus artificial censoring
                   ---------------------------------------------- */

                CASE

                    WHEN RAW_ARTIFICIAL_CENSOR_DATE
                            IS NULL
                        THEN NULL


                    /* Natural follow-up ends first */
                    WHEN ANALYTIC_END_DATE
                            < RAW_ARTIFICIAL_CENSOR_DATE
                        THEN NULL


                    /* Death wins ties */
                    WHEN ANALYTIC_END_DATE
                            = RAW_ARTIFICIAL_CENSOR_DATE

                         AND {death_col} = 1
                        THEN NULL


                    ELSE RAW_ARTIFICIAL_CENSOR_DATE

                END
                    AS ARTIFICIAL_CENSOR_DATE


            FROM clones1
            """
        ).collect()

        logger.info("Stage 9 complete: clone table %s", tmp_clones)


        # =============================================================
        # 10. CREATE FINAL CCW PERSON-PERIOD TABLE
        # =============================================================

        create_mode = (

            "CREATE OR REPLACE TABLE"

            if overwrite

            else "CREATE TABLE"
        )


        session.sql(
            f"""
            {create_mode}
                {output_table}
            AS


            WITH analytic0 AS (

                SELECT

                    c.*,

                    f.INTERVAL,

                    f.INTERVAL_START,


                    /* ----------------------------------------------
                       Nominal interval endpoint
                       ---------------------------------------------- */

                    DATEADD(
                        day,
                        (
                            (f.INTERVAL + 1)
                            * {interval_days}
                        ) - 1,
                        c.{index_date_col}
                    )
                        AS SCHEDULED_INTERVAL_END,


                    /* ----------------------------------------------
                       Exact clone-period endpoint

                       Earliest of:
                           scheduled interval end
                           natural/admin end
                           artificial censor
                       ---------------------------------------------- */

                    LEAST(

                        DATEADD(
                            day,
                            (
                                (f.INTERVAL + 1)
                                * {interval_days}
                            ) - 1,
                            c.{index_date_col}
                        ),

                        c.ANALYTIC_END_DATE,

                        COALESCE(
                            c.ARTIFICIAL_CENSOR_DATE,
                            c.ANALYTIC_END_DATE
                        )

                    ) AS INTERVAL_END,


                    /* ----------------------------------------------
                       Precomputed IPCW predictors

                       Note:
                           These contain PRIOR features only.
                       ---------------------------------------------- */

                    f.* EXCLUDE (
                        {patient_id_col},
                        INTERVAL,
                        INTERVAL_START,
                        ANALYTIC_END_DATE
                    )


                FROM {tmp_clones} c


                INNER JOIN {tmp_interval_features} f

                    ON c.{patient_id_col}
                        = f.{patient_id_col}


                WHERE

                    f.INTERVAL >= 0


                    AND f.INTERVAL_START
                        <= LEAST(

                            c.ANALYTIC_END_DATE,

                            COALESCE(
                                c.ARTIFICIAL_CENSOR_DATE,
                                c.ANALYTIC_END_DATE
                            )
                        )
            ),


            analytic AS (

                SELECT

                    *,


                    /* ==============================================
                       SURVIVAL COUNTING-PROCESS TIME
                       ============================================== */

                    DATEDIFF(
                        day,
                        {index_date_col},
                        INTERVAL_START
                    )
                        AS TSTART,


                    DATEDIFF(
                        day,
                        {index_date_col},
                        INTERVAL_END
                    ) + 1
                        AS TSTOP,


                    /* ==============================================
                       PERSON-TIME
                       ============================================== */

                    DATEDIFF(
                        day,
                        INTERVAL_START,
                        INTERVAL_END
                    ) + 1
                        AS PERSON_DAYS,


                    /* ==============================================
                       GRACE PERIOD
                       ============================================== */

                    IFF(
                        INTERVAL_START
                            <= GRACE_END_DATE,
                        1,
                        0
                    )
                        AS IN_GRACE_PERIOD,


                    /* ==============================================
                       MDT STRATEGY ALREADY ACHIEVED
                       ============================================== */

                    IFF(

                        STRATEGY_COMPLETION_DATE
                            IS NOT NULL

                        AND STRATEGY_COMPLETION_DATE
                            < INTERVAL_START,

                        1,
                        0

                    )
                        AS STRATEGY_COMPLETED_PRIOR,


                    /* ==============================================
                       ARTIFICIAL CENSORING
                       ============================================== */

                    IFF(

                        ARTIFICIAL_CENSOR_DATE
                            IS NOT NULL

                        AND INTERVAL_END
                            = ARTIFICIAL_CENSOR_DATE,

                        1,
                        0

                    )
                        AS ARTIFICIAL_CENSOR,


                    /* ==============================================
                       OVERALL SURVIVAL EVENT
                       ============================================== */

                    IFF(

                        {death_col} = 1

                        AND {censor_date_col}
                            <= ANALYTIC_END_DATE

                        AND (
                            ARTIFICIAL_CENSOR_DATE
                                IS NULL

                            OR {censor_date_col}
                                <= ARTIFICIAL_CENSOR_DATE
                        )

                        AND {censor_date_col}
                            BETWEEN
                                INTERVAL_START
                                AND INTERVAL_END,

                        1,
                        0

                    )
                        AS EVENT,


                    /* ==============================================
                       OBSERVED RIGHT CENSORING
                       ============================================== */

                    IFF(

                        {death_col} = 0

                        AND {censor_date_col}
                            = ANALYTIC_END_DATE

                        AND INTERVAL_END
                            = ANALYTIC_END_DATE

                        AND (
                            ARTIFICIAL_CENSOR_DATE
                                IS NULL

                            OR ANALYTIC_END_DATE
                                < ARTIFICIAL_CENSOR_DATE
                        ),

                        1,
                        0

                    )
                        AS OBSERVED_CENSOR,


                    /* ==============================================
                       ADMINISTRATIVE CENSORING
                       ============================================== */

                    IFF(

                        ANALYTIC_END_DATE
                            < {censor_date_col}

                        AND INTERVAL_END
                            = ANALYTIC_END_DATE

                        AND (
                            ARTIFICIAL_CENSOR_DATE
                                IS NULL

                            OR ANALYTIC_END_DATE
                                < ARTIFICIAL_CENSOR_DATE
                        ),

                        1,
                        0

                    )
                        AS ADMIN_CENSOR,


                    /* ==============================================
                       TOTAL AVAILABLE FOLLOW-UP
                       ============================================== */

                    DATEDIFF(
                        day,
                        {index_date_col},
                        ANALYTIC_END_DATE
                    )
                        AS OBSERVED_FOLLOWUP_DAYS,


                    DATEDIFF(
                        day,
                        {index_date_col},
                        ANALYTIC_END_DATE
                    ) / 365.25
                        AS OBSERVED_FOLLOWUP_YEARS,


                    /* ==============================================
                       TIME TO MDT STRATEGY COMPLETION
                       ============================================== */

                    CASE

                        WHEN STRATEGY_COMPLETION_DATE
                                IS NOT NULL

                        THEN DATEDIFF(
                            day,
                            {index_date_col},
                            STRATEGY_COMPLETION_DATE
                        )

                        ELSE NULL

                    END
                        AS DAYS_TO_STRATEGY_COMPLETION


                FROM analytic0
            )


            SELECT *

            FROM analytic


            ORDER BY

                {patient_id_col},

                STRATEGY_LEVEL,

                INTERVAL
            """
        ).collect()

        logger.info("Stage 10 complete: final table materialized at %s", output_table)


        # =============================================================
        # RETURN SNOWPARK DATAFRAME
        # =============================================================

        return session.table(
            output_table
        )


    # =================================================================
    # CLEANUP
    # =================================================================

    finally:

        if not keep_staging_tables:

            for table_name in reversed(
                staging_tables
            ):

                try:

                    session.sql(
                        f"""
                        DROP TABLE IF EXISTS
                            {table_name}
                        """
                    ).collect()

                except Exception:

                    # Do not mask the primary exception if cleanup
                    # of a staging table fails.
                    pass
                    logger.info("Dropped staging tables for run prefix %s", temp_prefix)


def main() -> None:
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")
    session.use_warehouse(os.environ["DEV_SNOW_WAREHOUSE"])  

    logger.info(
        session.sql(
            "SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE(), CURRENT_SCHEMA(), CURRENT_WAREHOUSE()"
        ).collect()
    )

    logger.info("Executing create_mdt_ccw_table_fast...")
    out_df = create_mdt_ccw_table_fast(session=session)
    logger.info("ALS_MDT_CCW built with %s rows.", out_df.count())


if __name__ == "__main__":
    main()