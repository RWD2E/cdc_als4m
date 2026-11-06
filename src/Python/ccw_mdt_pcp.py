import re
import uuid
import os
import logging
from typing import Optional, Sequence, Dict

from snowflake.snowpark import Session, DataFrame
from config import env, conn


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =====================================================================
# MDT COMPONENT DEFINITIONS
# =====================================================================

DEFAULT_MDT_COMPONENTS = {

    "NEUROLOGY": [],

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


MDT_VISIT_PHE_TYPES = ("OfficeVisit", "MDT")

MDT_PROVIDER_DOMAINS = {
    "NEUROLOGY": ("N",),
    "REHAB": ("M", "P"),
    "SLP": ("S",),
    "RESPIRATORY": ("R",),
    "NUTRITION": ("D",),
}


# =====================================================================
# MDT STRATEGIES
#
# Minimum required composition:
#
# A = Neurology-only
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

# Components in addition to the optional neurology requirement.
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
        "Neurology-only",

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
# ADDITIONAL PHE_TYPE VALUES INCLUDED IN EVENT STAGING
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
# LEGACY LAB SOURCE TABLES
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
    # INPUT
    # -----------------------------------------------------------------

    patient_table: str = (
        "GROUSE_ANALYTICS_DB."
        "SX_ALS_GPC."
        "ALS_CASE_TABLE1_CCW"
    ),

    event_table: str = (
        "GROUSE_ANALYTICS_DB."
        "SX_ALS_GPC."
        "ALS_DXRXPX_LONG"
    ),

    # -----------------------------------------------------------------
    # LEGACY LAB CONFIGURATION
    # -----------------------------------------------------------------

    lab_tables: Optional[
        Dict[str, str]
    ] = None,

    lab_obs_codes: Optional[
        Dict[str, Optional[Sequence[str]]]
    ] = None,

    # -----------------------------------------------------------------
    # CCW DESIGN
    # -----------------------------------------------------------------

    grace_period_days: int = 180,

    interval_days: int = 60,

    # None = follow through source CENSOR_DATE
    max_followup_days: Optional[int] = None,

    # -----------------------------------------------------------------
    # OUTPUT
    # -----------------------------------------------------------------
    output_table: Optional[str] = None,

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

    # -----------------------------------------------------------------
    # PROVIDER-BASED MDT EVIDENCE
    # -----------------------------------------------------------------

    provider_table: Optional[str] = (
        "GROUSE_ANALYTICS_DB."
        "SX_ALS_GPC."
        "PRVDR_LONG"
    ),

    event_provider_id_col: str = "PROVIDERID",

    event_site_col: str = "SITE",

    provider_id_col: str = "PROVIDERID",

    provider_site_col: str = "SITE",

    provider_mdt_domain_col: str = "MDT_DOMAIN",

    require_neurology: bool = False,

    # -----------------------------------------------------------------
    # DUAL / LIS ENROLLMENT (TIME-VARYING IPCW COVARIATES)
    # -----------------------------------------------------------------

    # None = disable DUAL/LIS covariates
    enrollment_table: Optional[str] = (
        "GROUSE_DB."
        "CMS_PCORNET_CDM."
        "LDS_ENROLLMENT"
    ),

    enrollment_patient_id_col: str = "PATID",

    enrollment_basis_col: str = "RAW_BASIS",

    enrollment_start_col: str = "ENR_START_DATE",

    enrollment_end_col: str = "ENR_END_DATE",

) -> DataFrame:
    """
    Build an optimized clone-censor-weight person-period survival dataset
    comparing multiple MDT strategies in ALS.

    ================================================================
    STRATEGIES
    ================================================================

    A: Neurology-only minimum requirement.
    B: Neurology + rehabilitation.
    C: Neurology + SLP.
    D: Neurology + respiratory.
    E: Neurology + rehabilitation + SLP.
    F: Neurology + rehabilitation + respiratory.
    G: Neurology + rehabilitation + SLP + nutrition.
    H: Neurology + rehabilitation + respiratory + nutrition.

    Strategies are treated as minimum required compositions.

    Existing component-specific phenotypes count as before. OfficeVisit
    and MDT events also count when their provider's MDT_DOMAIN maps to
    the component: N = neurology, M/P = rehabilitation, S = SLP,
    R = respiratory, and D = nutrition. Providers are matched by both
    PROVIDERID and SITE. Unknown or missing domains do not count.

    By default, neurology is assumed to be satisfied by cohort
    construction; with the default components, strategy A is satisfied
    at index. Set require_neurology=True to require documented neurology
    for every strategy. Custom strategy components still enforce their
    listed requirements. Set
    provider_table=None to disable provider-based evidence; when
    neurology is required, a custom NEUROLOGY phenotype list must then
    be supplied.

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

    For A-H, all required MDT components must occur within the
    configurable grace period.

    Failure results in artificial censoring at GRACE_END_DATE.

    ================================================================
    PERSON-PERIOD OUTPUT
    ================================================================

    The output has one row per clone per interval_days-day follow-up
    interval, starting at index date, including time-varying phenotype,
    MDT, and lab history. The index date is in interval 0.

    Time-varying IPCW covariates use only intervals before the current
    one. IPCW_CBC_ANY_PRIOR and IPCW_METABOLIC_ANY_PRIOR are 1 when any
    lab from the panel had a recorded value in an earlier interval.
    When enrollment_table is supplied, IPCW_DUAL_LIS_PRIOR is 1 if an
    enrollment span with RAW_BASIS 'DUAL' or 'LIS' overlaps the previous
    interval, and 0 otherwise. A NULL ENR_END_DATE is treated as open-ended.
    Unless output_table is supplied, it is written to
    GROUSE_ANALYTICS_DB.SX_ALS_GPC.ALS_MDT_CCW_<interval_days>_<grace_period_days>.

    With keep_staging_tables=True, patient, event, interval, feature,
    MDT first-date, and clone tables are retained for inspection.
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

    if output_table is None:
        output_table = (
            "GROUSE_ANALYTICS_DB."
            "SX_ALS_GPC."
            f"ALS_MDT_CCW_{interval_days}_{grace_period_days}"
        )

    logger.info(
        "Starting CCW build: patient_table=%s event_table=%s provider_table=%s output_table=%s interval_days=%s grace_days=%s max_followup_days=%s require_neurology=%s",
        patient_table,
        event_table,
        provider_table,
        output_table,
        interval_days,
        grace_period_days,
        max_followup_days,
        require_neurology,
    )


    mdt_components = {
        k: list(v)
        for k, v in mdt_components.items()
    }

    strategy_components = {
        k: list(v)
        for k, v in strategy_components.items()
    }

    if provider_table is not None or require_neurology:
        neurology_component = next(
            (name for name in mdt_components if name.upper() == "NEUROLOGY"),
            None,
        )
        if neurology_component is None:
            neurology_component = "NEUROLOGY"
            mdt_components[neurology_component] = []

        if require_neurology:
            for components in strategy_components.values():
                if not any(name.upper() == "NEUROLOGY" for name in components):
                    components.insert(0, neurology_component)

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

    identifiers.extend(lab_tables.values())
    identifiers.extend(
        baseline_covariates
    )

    if provider_table is not None:
        identifiers.extend([
            provider_table,
            event_provider_id_col,
            event_site_col,
            provider_id_col,
            provider_site_col,
            provider_mdt_domain_col,
        ])

    if enrollment_table is not None:
        identifiers.extend([
            enrollment_table,
            enrollment_patient_id_col,
            enrollment_basis_col,
            enrollment_start_col,
            enrollment_end_col,
        ])


    for identifier in identifiers:
        validate_identifier(identifier)

    logger.info("Identifier validation complete for %s fields.", len(identifiers))

    component_aliases = [safe_alias(component) for component in mdt_components]
    if len(component_aliases) != len(set(component_aliases)):
        raise ValueError("Duplicate generated MDT component alias.")
    components_by_alias = {
        safe_alias(component): component for component in mdt_components
    }


    if grace_period_days < 0:

        raise ValueError(
            "grace_period_days must be >= 0."
        )

    if (
        isinstance(interval_days, bool)
        or not isinstance(interval_days, int)
        or interval_days <= 0
    ):
        raise ValueError("interval_days must be a positive integer.")


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

        for component in strategy_components[strategy]:
            alias = safe_alias(component)
            if alias not in components_by_alias:
                raise ValueError(
                    f"Unknown MDT component {component!r} in strategy {strategy}."
                )
            component_name = components_by_alias[alias]
            if not mdt_components[component_name] and (
                provider_table is None
                or component_name.upper() not in MDT_PROVIDER_DOMAINS
            ):
                raise ValueError(
                    f"MDT component {component!r} in strategy {strategy} "
                    "has no phenotype or provider-domain source."
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
            + (list(MDT_VISIT_PHE_TYPES) if provider_table is not None else [])
        )
    )


    all_phe_sql = ", ".join(
        sql_literal(x)
        for x in all_phe_types
    ) or "'__NO_MATCH__'"

    provider_components = {
        component: MDT_PROVIDER_DOMAINS[component.upper()]
        for component in mdt_components
        if provider_table is not None and component.upper() in MDT_PROVIDER_DOMAINS
    }

    component_match_sql = {}
    for component, phe_list in mdt_components.items():
        conditions = []
        if phe_list:
            values = ", ".join(sql_literal(phe) for phe in phe_list)
            conditions.append(f"PHE_TYPE IN ({values})")
        if component in provider_components:
            conditions.append(f"PROVIDER_MDT_{safe_alias(component)} = 1")
        component_match_sql[component] = (
            "(" + " OR ".join(conditions) + ")" if conditions else "FALSE"
        )

    provider_select_sql = ""
    provider_join_sql = ""
    if provider_table is not None and provider_components:
        provider_flags = []
        provider_selects = []
        provider_domains = set()
        for component, domains in provider_components.items():
            alias = safe_alias(component)
            domain_values = ", ".join(sql_literal(domain) for domain in domains)
            provider_domains.update(domains)
            provider_flags.append(
                f"""
                MAX(
                    IFF(
                        UPPER(TRIM({provider_mdt_domain_col})) IN ({domain_values}),
                        1,
                        0
                    )
                ) AS PROVIDER_MDT_{alias}
                """
            )
            provider_selects.append(
                f"COALESCE(prv.PROVIDER_MDT_{alias}, 0) AS PROVIDER_MDT_{alias}"
            )

        domain_sql = ", ".join(sql_literal(domain) for domain in sorted(provider_domains))
        visit_sql = ", ".join(sql_literal(phe) for phe in MDT_VISIT_PHE_TYPES)
        provider_select_sql = ",\n" + ",\n".join(provider_selects)
        provider_flag_sql = ",\n".join(provider_flags)
        provider_join_sql = f"""
            LEFT JOIN (
                SELECT
                    {provider_id_col} AS MDT_PROVIDER_ID,
                    {provider_site_col} AS MDT_PROVIDER_SITE,
                    {provider_flag_sql}
                FROM {provider_table}
                WHERE {provider_id_col} IS NOT NULL
                    AND {provider_site_col} IS NOT NULL
                    AND UPPER(TRIM({provider_mdt_domain_col})) IN ({domain_sql})
                GROUP BY {provider_id_col}, {provider_site_col}
            ) prv
                ON e.{event_provider_id_col} = prv.MDT_PROVIDER_ID
                AND e.{event_site_col} = prv.MDT_PROVIDER_SITE
                AND e.{phe_type_col} IN ({visit_sql})
        """


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

    tmp_mdt_dates = (
        f"{temp_prefix}_MDT_DATES"
    )

    tmp_event_intervals = (
        f"{temp_prefix}_EVENT_INTERVALS"
    )

    tmp_patient_intervals = (
        f"{temp_prefix}_PATIENT_INTERVALS"
    )

    tmp_interval_features = (
        f"{temp_prefix}_FEATURES"
    )

    tmp_bmi_baseline = (
        f"{temp_prefix}_BMI_BASELINE"
    )

    tmp_enrollment_intervals = (
        f"{temp_prefix}_ENROLLMENT_INTERVALS"
    )

    tmp_clones = (
        f"{temp_prefix}_CLONES"
    )

    logger.info("Using temporary table prefix: %s", temp_prefix)

    lab_interval_tables = {}
    lab_pivot_tables = {}
    lab_source_aliases = [safe_alias(source_name) for source_name in lab_tables]
    if len(lab_source_aliases) != len(set(lab_source_aliases)):
        raise ValueError("Duplicate generated lab source alias.")
    for source_name in lab_tables:
        source_alias = safe_alias(source_name)
        lab_interval_tables[source_name] = (
            f"{temp_prefix}_{source_alias}_INTERVAL"
        )
        lab_pivot_tables[source_name] = (
            f"{temp_prefix}_{source_alias}_PIVOT"
        )

    staging_tables = [
        tmp_patients,
        tmp_events,
        tmp_mdt_dates,
        tmp_event_intervals,
        tmp_patient_intervals,
        tmp_interval_features,
        tmp_clones,
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
    # MDT FIRST-DATE EXPRESSIONS
    # =================================================================

    mdt_first_date_expressions = []


    for component in mdt_components:

        alias = safe_alias(
            component
        )

        mdt_first_date_expressions.append(
            f"""
            MIN(
                IFF(
                    {component_match_sql[component]}

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
        # A strategy with no required components is satisfied at index.
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
                    AS PHE_TYPE

                {provider_select_sql},

                p.{index_date_col},

                p.GRACE_END_DATE


            FROM {event_table} e


            INNER JOIN {tmp_patients} p

                ON e.{event_patient_id_col}
                    = p.{patient_id_col}

            {provider_join_sql}


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
        # 2. DETERMINE MDT COMPONENT DATES
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

        logger.info("Stage 2 complete: MDT first-date table %s", tmp_mdt_dates)

        event_count_pairs = []
        for phenotype in ipcw_phe_types:
            event_count_pairs.append(
                (f"N_{safe_alias(phenotype)}", f"PHE_TYPE = {sql_literal(phenotype)}")
            )
        for component in mdt_components:
            event_count_pairs.append(
                (f"N_MDT_{safe_alias(component)}", component_match_sql[component])
            )
        if len(event_count_pairs) != len({alias for alias, _ in event_count_pairs}):
            raise ValueError("Duplicate generated interval event column.")
        event_count_fields = dict(event_count_pairs)

        event_count_sql = ",\n".join(
            f"SUM(IFF({condition}, 1, 0)) AS {alias}"
            for alias, condition in event_count_fields.items()
        ) or "COUNT(*) AS EVENT_COUNT"
        event_interval_expression = f"""
            GREATEST(
                -1,
                FLOOR(
                    DATEDIFF(day, {index_date_col}, CD_DATE)
                    / {interval_days}
                )
            )::INTEGER
        """

        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_event_intervals}
            AS
            SELECT
                {patient_id_col},
                {event_interval_expression} AS INTERVAL,
                {event_count_sql}
            FROM {tmp_events}
            GROUP BY
                {patient_id_col},
                {event_interval_expression}
            """
        ).collect()
        logger.info("Built interval event counts table %s", tmp_event_intervals)

        max_interval_rows = session.sql(
            f"""
            SELECT
                COALESCE(
                    MAX(
                        CEIL(
                            DATEDIFF(
                                day,
                                {index_date_col},
                                ANALYTIC_END_DATE
                            ) / {interval_days}
                        )
                    ),
                    0
                ) + 2 AS MAX_INTERVALS
            FROM {tmp_patients}
            """
        ).collect()
        max_intervals = max(
            2,
            int(max_interval_rows[0][0]) if max_interval_rows else 2,
        )

        session.sql(
            f"""
            CREATE OR REPLACE TEMPORARY TABLE
                {tmp_patient_intervals}
            AS
            WITH interval_sequence AS (
                SELECT
                    ROW_NUMBER() OVER (ORDER BY SEQ4()) - 2 AS INTERVAL
                FROM TABLE(GENERATOR(ROWCOUNT => {max_intervals}))
            )
            SELECT
                p.{patient_id_col},
                s.INTERVAL,
                CASE
                    WHEN s.INTERVAL = -1 THEN DATEADD(
                        day, -{interval_days}, p.{index_date_col}
                    )
                    ELSE DATEADD(
                        day, s.INTERVAL * {interval_days}, p.{index_date_col}
                    )
                END AS INTERVAL_START,
                p.ANALYTIC_END_DATE
            FROM {tmp_patients} p
            CROSS JOIN interval_sequence s
            WHERE
                s.INTERVAL = -1
                OR DATEADD(
                    day, s.INTERVAL * {interval_days}, p.{index_date_col}
                ) <= p.ANALYTIC_END_DATE
            """
        ).collect()
        logger.info("Built patient interval scaffold %s", tmp_patient_intervals)

        # Reduce lab observations to the latest observation per interval and
        # code before pivoting to keep interval joins bounded.
        resolved_lab_codes = {}
        for source_name, source_table in lab_tables.items():
            source_alias = safe_alias(source_name)
            configured_codes = None
            if lab_obs_codes is not None:
                configured_codes = next(
                    (
                        values
                        for key, values in lab_obs_codes.items()
                        if key.upper() == source_name.upper()
                    ),
                    None,
                )
            if source_name.upper() == "BMI":
                codes = ["BMI"]
            elif configured_codes is not None:
                codes = list(dict.fromkeys(str(code) for code in configured_codes))
            else:
                code_rows = session.sql(
                    f"""
                    SELECT DISTINCT {lab_code_col}
                    FROM {source_table}
                    WHERE {lab_code_col} IS NOT NULL
                    ORDER BY {lab_code_col}
                    """
                ).collect()
                codes = list(dict.fromkeys(str(row[0]) for row in code_rows))
            resolved_lab_codes[source_name] = codes

            if not codes:
                logger.info("No lab codes selected for source %s", source_name)
                continue

            code_aliases = [safe_alias(code) for code in codes]
            if len(code_aliases) != len(set(code_aliases)):
                raise ValueError(
                    f"Duplicate generated lab code alias in source {source_name!r}."
                )
            tmp_lab_interval = lab_interval_tables[source_name]
            tmp_lab_pivot = lab_pivot_tables[source_name]
            if source_name.upper() == "BMI":
                lab_code_expression = "'BMI'"
                lab_value_expression = "l.BMI"
                lab_code_filter = ""
            else:
                lab_code_expression = f"l.{lab_code_col}"
                lab_value_expression = f"l.{lab_value_col}"
                code_literals = ", ".join(sql_literal(code) for code in codes)
                lab_code_filter = f"AND l.{lab_code_col} IN ({code_literals})"

            session.sql(
                f"""
                CREATE OR REPLACE TEMPORARY TABLE
                    {tmp_lab_interval}
                AS
                SELECT
                    l.{lab_patient_id_col} AS {patient_id_col},
                    GREATEST(
                        -1,
                        FLOOR(
                            l.{lab_days_since_index_col} / {interval_days}
                        )
                    )::INTEGER AS INTERVAL,
                    {lab_code_expression} AS OBS_CODE,
                    {lab_value_expression} AS LAB_VALUE,
                    l.{lab_date_col} AS OBS_DATE
                FROM {source_table} l
                INNER JOIN {tmp_patients} p
                    ON l.{lab_patient_id_col} = p.{patient_id_col}
                WHERE l.{lab_date_col} IS NOT NULL
                    AND l.{lab_days_since_index_col} IS NOT NULL
                    AND {lab_value_expression} IS NOT NULL
                    {lab_code_filter}
                    AND l.{lab_date_col} <= p.ANALYTIC_END_DATE
                QUALIFY ROW_NUMBER() OVER (
                    PARTITION BY
                        l.{lab_patient_id_col},
                        GREATEST(
                            -1,
                            FLOOR(
                                l.{lab_days_since_index_col} / {interval_days}
                            )
                        ),
                        {lab_code_expression}
                    ORDER BY l.{lab_date_col} DESC
                ) = 1
                """
            ).collect()
            staging_tables.extend([tmp_lab_interval, tmp_lab_pivot])

            pivot_expressions = []
            for code in codes:
                code_alias = f"{source_alias}_{safe_alias(code)}"
                pivot_expressions.append(
                    f"""
                    MAX(IFF(
                        OBS_CODE = {sql_literal(code)},
                        LAB_VALUE,
                        NULL
                    )) AS CUR_{code_alias}
                    """
                )
            session.sql(
                f"""
                CREATE OR REPLACE TEMPORARY TABLE
                    {tmp_lab_pivot}
                AS
                SELECT
                    {patient_id_col},
                    INTERVAL,
                    {", ".join(pivot_expressions)}
                FROM {tmp_lab_interval}
                GROUP BY {patient_id_col}, INTERVAL
                """
            ).collect()

        if any(name.upper() == "BMI" for name in lab_tables):
            bmi_source = next(
                name for name in lab_tables if name.upper() == "BMI"
            )
            if resolved_lab_codes[bmi_source]:
                session.sql(
                    f"""
                    CREATE OR REPLACE TEMPORARY TABLE
                        {tmp_bmi_baseline}
                    AS
                    SELECT
                        l.{lab_patient_id_col} AS {patient_id_col},
                        l.BMI AS BMI_BASELINE
                    FROM {lab_tables[bmi_source]} l
                    INNER JOIN {tmp_patients} p
                        ON l.{lab_patient_id_col} = p.{patient_id_col}
                    WHERE l.{lab_date_col} IS NOT NULL
                        AND l.BMI IS NOT NULL
                        AND l.{lab_date_col} <= p.{index_date_col}
                    QUALIFY ROW_NUMBER() OVER (
                        PARTITION BY l.{lab_patient_id_col}
                        ORDER BY l.{lab_date_col} DESC
                    ) = 1
                    """
                ).collect()
                staging_tables.append(tmp_bmi_baseline)

        if enrollment_table is not None:
            session.sql(
                f"""
                CREATE OR REPLACE TEMPORARY TABLE
                    {tmp_enrollment_intervals}
                AS
                SELECT
                    i.{patient_id_col},
                    i.INTERVAL,
                    1 AS CUR_DUAL_LIS
                FROM {tmp_patient_intervals} i
                INNER JOIN {enrollment_table} e
                    ON i.{patient_id_col} = e.{enrollment_patient_id_col}
                    AND e.{enrollment_start_col} <= DATEADD(
                        day, {interval_days - 1}, i.INTERVAL_START
                    )
                    AND (
                        e.{enrollment_end_col} IS NULL
                        OR e.{enrollment_end_col} >= i.INTERVAL_START
                    )
                WHERE UPPER(TRIM(e.{enrollment_basis_col})) IN ('DUAL', 'LIS')
                    AND e.{enrollment_start_col} IS NOT NULL
                GROUP BY i.{patient_id_col}, i.INTERVAL
                """
            ).collect()
            staging_tables.append(tmp_enrollment_intervals)
            logger.info(
                "Built DUAL/LIS enrollment intervals %s",
                tmp_enrollment_intervals,
            )

        dense_select_expressions = [
            f"COALESCE(ev.{alias}, 0) AS {alias}"
            for alias in event_count_fields
        ]
        dense_join_sql = [
            f"""
            LEFT JOIN {tmp_event_intervals} ev
                ON i.{patient_id_col} = ev.{patient_id_col}
                AND i.INTERVAL = ev.INTERVAL
            """
        ]
        lab_prior_expressions = []
        lab_prior_column_names = []
        for source_name, codes in resolved_lab_codes.items():
            if not codes:
                continue
            source_alias = safe_alias(source_name)
            pivot_table = lab_pivot_tables[source_name]
            join_alias = f"L_{source_alias}"
            dense_join_sql.append(
                f"""
                LEFT JOIN {pivot_table} {join_alias}
                    ON i.{patient_id_col} = {join_alias}.{patient_id_col}
                    AND i.INTERVAL = {join_alias}.INTERVAL
                """
            )
            for code in codes:
                code_alias = f"{source_alias}_{safe_alias(code)}"
                current_alias = f"CUR_{code_alias}"
                dense_select_expressions.append(
                    f"{join_alias}.{current_alias} AS {current_alias}"
                )
                lab_prior_expressions.append(
                    f"""
                    LAST_VALUE({current_alias}) IGNORE NULLS OVER (
                        PARTITION BY {patient_id_col}
                        ORDER BY INTERVAL
                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                    ) AS IPCW_{code_alias}_PRIOR_LAST
                    """
                )
                lab_prior_column_names.append(
                    f"IPCW_{code_alias}_PRIOR_LAST"
                )

        bmi_source = next(
            (name for name in lab_tables if name.upper() == "BMI"),
            None,
        )
        if bmi_source is not None and resolved_lab_codes[bmi_source]:
            dense_select_expressions.append("b.BMI_BASELINE AS BMI_BASELINE")
            dense_join_sql.append(
                f"""
                LEFT JOIN {tmp_bmi_baseline} b
                    ON i.{patient_id_col} = b.{patient_id_col}
                """
            )
            lab_prior_expressions.append(
                f"""
                IFF(
                    BMI_BASELINE IS NOT NULL
                    AND BMI_BASELINE != 0
                    AND LAST_VALUE(CUR_BMI_BMI) IGNORE NULLS OVER (
                        PARTITION BY {patient_id_col}
                        ORDER BY INTERVAL
                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                    ) IS NOT NULL,
                    (
                        BMI_BASELINE - LAST_VALUE(CUR_BMI_BMI) IGNORE NULLS
                        OVER (
                            PARTITION BY {patient_id_col}
                            ORDER BY INTERVAL
                            ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                        )
                    ) / BMI_BASELINE * 100,
                    NULL
                ) AS BMI_CHGPCT
                """
            )
            lab_prior_column_names.append("BMI_CHGPCT")

        enrollment_prior_expressions = []
        enrollment_prior_column_names = []
        if enrollment_table is not None:
            dense_join_sql.append(
                f"""
                LEFT JOIN {tmp_enrollment_intervals} en
                    ON i.{patient_id_col} = en.{patient_id_col}
                    AND i.INTERVAL = en.INTERVAL
                """
            )
            dense_select_expressions.append(
                "COALESCE(en.CUR_DUAL_LIS, 0) AS CUR_DUAL_LIS"
            )
            enrollment_prior_expressions.append(
                f"""
                COALESCE(LAG(CUR_DUAL_LIS) OVER (
                    PARTITION BY {patient_id_col}
                    ORDER BY INTERVAL
                ), 0) AS IPCW_DUAL_LIS_PRIOR
                """
            )
            enrollment_prior_column_names.append("IPCW_DUAL_LIS_PRIOR")

        dense_join_clause = "\n".join(dense_join_sql)
        dense_select_sql = ",\n".join(dense_select_expressions)
        event_prior_expressions = []
        event_prior_column_names = []
        for phenotype in ipcw_phe_types:
            alias = f"N_{safe_alias(phenotype)}"
            event_prior_expressions.extend([
                f"""
                IFF(
                    COALESCE(SUM({alias}) OVER (
                        PARTITION BY {patient_id_col}
                        ORDER BY INTERVAL
                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                    ), 0) > 0,
                    1,
                    0
                ) AS IPCW_{safe_alias(phenotype)}_PRIOR
                """,
                f"""
                COALESCE(SUM({alias}) OVER (
                    PARTITION BY {patient_id_col}
                    ORDER BY INTERVAL
                    ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                ), 0) AS IPCW_{safe_alias(phenotype)}_PRIOR_N
                """,
            ])
            event_prior_column_names.extend([
                f"IPCW_{safe_alias(phenotype)}_PRIOR",
                f"IPCW_{safe_alias(phenotype)}_PRIOR_N",
            ])
        mdt_prior_expressions = []
        mdt_prior_column_names = []
        for component in mdt_components:
            alias = f"N_MDT_{safe_alias(component)}"
            mdt_prior_expressions.append(
                f"""
                IFF(
                    COALESCE(SUM({alias}) OVER (
                        PARTITION BY {patient_id_col}
                        ORDER BY INTERVAL
                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                    ), 0) > 0,
                    1,
                    0
                ) AS MDT_{safe_alias(component)}_PRIOR
                """
            )
            mdt_prior_column_names.append(
                f"MDT_{safe_alias(component)}_PRIOR"
            )

        panel_prior_expressions = []
        panel_prior_column_names = []
        for panel_name, panel_indicator in [
            ("CBC", "IPCW_CBC_ANY_PRIOR"),
            ("METABOLIC", "IPCW_METABOLIC_ANY_PRIOR"),
        ]:
            panel_source = next(
                (name for name in lab_tables if name.upper() == panel_name),
                None,
            )
            panel_codes = (
                resolved_lab_codes.get(panel_source, [])
                if panel_source is not None else []
            )
            if not panel_codes:
                continue
            panel_alias = safe_alias(panel_source)
            any_prior = " OR ".join(
                f"CUR_{panel_alias}_{safe_alias(code)} IS NOT NULL"
                for code in panel_codes
            )
            panel_prior_expressions.append(
                f"""
                IFF(
                    COALESCE(SUM(IFF({any_prior}, 1, 0)) OVER (
                        PARTITION BY {patient_id_col}
                        ORDER BY INTERVAL
                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
                    ), 0) > 0,
                    1,
                    0
                ) AS {panel_indicator}
                """
            )
            panel_prior_column_names.append(panel_indicator)

        feature_expressions = (
            event_prior_expressions
            + mdt_prior_expressions
            + lab_prior_expressions
            + panel_prior_expressions
            + enrollment_prior_expressions
        )
        feature_sql = ",\n".join(feature_expressions)
        feature_column_names = (
            event_prior_column_names
            + mdt_prior_column_names
            + lab_prior_column_names
            + panel_prior_column_names
            + enrollment_prior_column_names
        )
        if len(feature_column_names) != len(set(feature_column_names)):
            raise ValueError("Duplicate generated interval feature column.")
        if not feature_sql:
            feature_sql = "1 AS NO_PRIOR_FEATURES"
            feature_column_names = ["NO_PRIOR_FEATURES"]
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
        logger.info("Built interval feature table %s", tmp_interval_features)


        # =============================================================
        # 3. CREATE A-H CLONES
        #
        # Cloning happens after MDT component dates are calculated.
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

        logger.info("Stage 3 complete: clone table %s", tmp_clones)


        # =============================================================
        # 4. CREATE ONE START-STOP RECORD PER CLONE × INTERVAL
        # =============================================================

        create_mode = (
            "CREATE OR REPLACE TABLE" if overwrite else "CREATE TABLE"
        )
        feature_select_sql = ",\n".join(
            f"f.{name}" for name in feature_column_names
        )

        session.sql(
            f"""
            {create_mode} {output_table} AS

            WITH clone_end AS (
                SELECT
                    c.*,
                    LEAST(
                        c.ANALYTIC_END_DATE,
                        COALESCE(c.ARTIFICIAL_CENSOR_DATE,
                                 c.ANALYTIC_END_DATE)
                    ) AS CLONE_END_DATE
                FROM {tmp_clones} c
            ),
            clone_intervals AS (
                SELECT
                    c.*,
                    i.INTERVAL,
                    i.INTERVAL_START,
                    LEAST(
                        DATEADD(
                            day,
                            {interval_days - 1},
                            i.INTERVAL_START
                        ),
                        c.CLONE_END_DATE
                    ) AS INTERVAL_END
                FROM clone_end c
                INNER JOIN {tmp_patient_intervals} i
                    ON c.{patient_id_col} = i.{patient_id_col}
                WHERE i.INTERVAL >= 0
                    AND i.INTERVAL_START <= c.CLONE_END_DATE
            ),
            survival AS (
                SELECT
                    c.*,
                    DATEDIFF(
                        day, c.{index_date_col}, c.INTERVAL_START
                    ) AS TSTART,
                    DATEDIFF(
                        day, c.{index_date_col}, c.INTERVAL_END
                    ) + 1 AS TSTOP,
                    DATEDIFF(
                        day, c.INTERVAL_START, c.INTERVAL_END
                    ) + 1 AS PERSON_DAYS,
                    IFF(
                        c.ARTIFICIAL_CENSOR_DATE BETWEEN
                            c.INTERVAL_START AND c.INTERVAL_END
                        AND c.CLONE_END_DATE = c.ARTIFICIAL_CENSOR_DATE,
                        1, 0
                    ) AS ARTIFICIAL_CENSOR,
                    IFF(
                        c.{death_col} = 1
                        AND c.{censor_date_col} <= c.ANALYTIC_END_DATE
                        AND c.{censor_date_col} BETWEEN
                            c.INTERVAL_START AND c.INTERVAL_END
                        AND (
                            c.ARTIFICIAL_CENSOR_DATE IS NULL
                            OR c.{censor_date_col}
                                <= c.ARTIFICIAL_CENSOR_DATE
                        ),
                        1, 0
                    ) AS EVENT,
                    IFF(
                        c.{death_col} = 0
                        AND c.{censor_date_col} = c.ANALYTIC_END_DATE
                        AND c.CLONE_END_DATE = c.ANALYTIC_END_DATE
                        AND c.{censor_date_col} BETWEEN
                            c.INTERVAL_START AND c.INTERVAL_END
                        AND (
                            c.ARTIFICIAL_CENSOR_DATE IS NULL
                            OR c.ANALYTIC_END_DATE
                                < c.ARTIFICIAL_CENSOR_DATE
                        ),
                        1, 0
                    ) AS OBSERVED_CENSOR,
                    IFF(
                        c.ANALYTIC_END_DATE < c.{censor_date_col}
                        AND c.CLONE_END_DATE = c.ANALYTIC_END_DATE
                        AND c.ANALYTIC_END_DATE BETWEEN
                            c.INTERVAL_START AND c.INTERVAL_END
                        AND (
                            c.ARTIFICIAL_CENSOR_DATE IS NULL
                            OR c.ANALYTIC_END_DATE
                                < c.ARTIFICIAL_CENSOR_DATE
                        ),
                        1, 0
                    ) AS ADMIN_CENSOR,
                    DATEDIFF(
                        day, c.{index_date_col}, c.ANALYTIC_END_DATE
                    ) AS OBSERVED_FOLLOWUP_DAYS,
                    DATEDIFF(
                        day, c.{index_date_col}, c.ANALYTIC_END_DATE
                    ) / 365.25 AS OBSERVED_FOLLOWUP_YEARS,
                    IFF(
                        c.STRATEGY_COMPLETION_DATE IS NOT NULL,
                        DATEDIFF(
                            day,
                            c.{index_date_col},
                            c.STRATEGY_COMPLETION_DATE
                        ),
                        NULL
                    ) AS DAYS_TO_STRATEGY_COMPLETION,
                    IFF(
                        c.STRATEGY_COMPLETION_DATE IS NOT NULL
                        AND c.STRATEGY_COMPLETION_DATE
                            <= c.CLONE_END_DATE,
                        1, 0
                    ) AS STRATEGY_COMPLETED_BY_END
                FROM clone_intervals c
            )
            SELECT
                s.*,
                {feature_select_sql}
            FROM survival s
            LEFT JOIN {tmp_interval_features} f
                ON s.{patient_id_col} = f.{patient_id_col}
                AND s.INTERVAL = f.INTERVAL
            WHERE s.TSTOP > s.TSTART
            ORDER BY s.{patient_id_col}, s.STRATEGY_LEVEL, s.INTERVAL
            """
        ).collect()

        logger.info(
            "Stage 4 complete: clone-by-interval survival table at %s",
            output_table,
        )


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