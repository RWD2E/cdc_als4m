"""
Annual ALS and diabetes (DM) prevalence and incidence rates in Snowpark.

Per calendar year, crude and age-sex-adjusted, each with confidence intervals:
    DM_PREVALENCE_PCT         DM prevalence      out of the denominator cohort
    ALS_PREVALENCE            ALS prevalence     out of the denominator cohort
    ALS_PREVALENCE_IN_DM      ALS prevalence     out of the DM cohort
    DM_PREVALENCE_IN_ALS_PCT  DM prevalence      out of the ALS cohort
    ALS_INCIDENCE             ALS incidence      out of the denominator cohort
    ALS_INCIDENCE_IN_DM       ALS incidence      out of the DM cohort
    ALS_INCIDENCE_IN_NON_DM   ALS incidence      in at-risk time before/without DM

Medication prevalence is computed separately (snapshot method) by
compute_medication_prevalence_snapshot(...). Both pipelines return their `final`
table in the same canonical long format:
    YEAR, OUTCOME, POPULATION, METRIC, ESTIMATE_TYPE,
    NUM, DEN, ESTIMATE, LCL, UCL, UNIT

Zero or missing denominators produce NULL estimates and confidence limits, not
zero rates. Adjusted rates are also NULL when no eligible standard weight remains.
Zero case counts with positive denominators remain valid zero estimates.

Both canonical final tables include YEAR=9999 summaries over all input years.
Counts and person-time are summed before recomputing rates and confidence limits;
prevalence summaries count patient-years, not distinct patients over the period.
Adjusted summaries pool age-sex strata and sum year-specific standard weights
(fixed standards are unchanged). Cell suppression is applied after pooling.

Inputs
------
denom : denominator / at-risk cohort, one row per person
    person, observation start, observation censor, birth date, sex
    GLP1RA_CASE_TABLE1 must be rebuilt with glp1_tbl1.py to include BIRTH_DATE.
    Set denom_start_col="INDEX_DATE" for entry at the first GLP-1 prescription;
    ENTRY_DATE instead represents the original enrollment entry.
num   : ALS risk set, one row per person (deduplicated internally)
    person, initial diagnosis date, ALS censor date
dm    : DM cohort, one row per person (deduplicated internally)
    person, initial diagnosis date, DM censor date
    DM is DUAL-ROLE: numerator for its own prevalence, denominator for the two
    ALS-in-DM measures. Its censor equals the enrollment censor, as ALS's does, so
    the same carry-forward reasoning applies to both.
med   : medication registry, one row per person x year x phenotype
    person, calendar year, denominator label (DENOM), phenotype label (PHE_TYPE)
    DENOM encodes denominator membership (e.g. "DM", "ALL", ...).
    PHE_TYPE may be NULL for denominator-only rows and those rows still count in the
    selected denominator(s).

Prevalence methods (explicitly separated)
-----------------------------------------
1) Carry-forward prevalence (window-based)
    Uses diagnosis-window style inputs (start/diagnosis and censor/end), as used for
    ALS and DM. A person is prevalent in year Y if the active window overlaps Y.

2) Snapshot prevalence (no carry-over)
    Uses longitudinal time-indexed inputs (PATID + YEAR/time-unit), denominator
    labels (DENOM), and event labels (e.g. PHE_TYPE). Prevalence is counted only in
    the recorded time unit and never carried across years. DENOM selection is
    derived from dm_type: ALL -> DM, T1DM -> T1DM, T2DM -> T2DM.

Two design points behind the DM measures
----------------------------------------
1. LEFT TRUNCATION. DM person-time starts at DM onset (+ lag + washout), not at
   cohort entry. Nobody is in the DM cohort before they have diabetes; accruing their
   pre-diabetes time to the DM denominator pads it with unexposed time and drags
   ALS_INCIDENCE_IN_DM toward zero.
2. EVENT ORDER. An incident ALS case counts toward DM only if dm_entry <= als_dx.
   Otherwise a person whose ALS preceded their diabetes is credited to an exposure
   that had not begun.
   These two errors bias in OPPOSITE directions and can cancel, yielding a plausible
   rate ratio from two mistakes. Neither is detectable by inspecting the output;
   `qa_person_time` checks the partition invariants instead.

ALS_INCIDENCE vs ALS_INCIDENCE_IN_DM is NOT a clean contrast -- the denominator
cohort contains the DM patients, so the comparison is subset-vs-whole and attenuated.
ALS_INCIDENCE_IN_DM vs ALS_INCIDENCE_IN_NON_DM is the comparison that answers whether
DM associates with ALS. Set include_non_dm=False to drop it.

ALS_PREVALENCE_IN_DM and DM_PREVALENCE_IN_ALS_PCT share one numerator (both
conditions active in the year) and differ only in which cohort forms the
denominator. Both are CO-PREVALENCE proportions within a year, not lifetime
comorbidity: a person whose DM is recorded but whose ALS window has closed is not in
the numerator. Under the default prevalence_carry_forward="ENROLLMENT" both windows
run to disenrollment, so within-year co-prevalence is the intended reading.

Incident-case ascertainment (washouts)
--------------------------------------
"First diagnosis observed in the data" is not the same as "new diagnosis". A person
whose ALS surfaces on their first claim after enrollment may have been diagnosed
years earlier; counted as incident, they inflate the numerator in exactly the years
with the most new enrollees. Two washouts guard against this, at two anchors:

    entry_washout_days -- clean lookback after DENOMINATOR ENTRY. An ALS dx within
        this window of START_DATE is not counted, and the window itself accrues NO
        at-risk person-time.
    dm_washout_days    -- clean lookback after DM ONSET (+ lag), for the DM-cohort
        measures. Same rule at the DM anchor.

Both are applied to the NUMERATOR AND THE DENOMINATOR TOGETHER. This is the whole
point: excluding a case from a window whose person-time still accrues biases
incidence downward just as surely as counting a prevalent case biases it upward. See
`build_person_year`.

`dm_washout_days` is NOT a duplicate of `dm_lag_days`. They differ in where the
window goes:
    lag     -- induction/latency. The exposure has begun but is presumed not yet
               acting, so the window is UNEXPOSED -> non-DM person-time.
    washout -- ascertainment. The person HAS diabetes, so the window is not
               unexposed; it just cannot support an incident-ALS determination.
               It belongs to NEITHER arm and is accounted for separately.
Both may be set at once. dm_entry = dm_dx + dm_lag_days + dm_washout_days.

Age restriction
---------------
cfg.min_age / cfg.max_age restrict every measure to a closed age range (e.g.
min_age=18 for adults). Age is evaluated at the year mid-point -- the SAME point used
to assign AGE_GROUP -- and the filter is applied to the person-year row, so
numerators, denominators and person-time are restricted by one predicate and cannot
drift apart. See `build_person_year` for the two behavioral consequences.

Epidemiologic conventions
-------------------------
Period prevalence (year Y)
    numerator   = persons carried forward from diagnosis to end of observation,
                  whose active window overlaps Y
    denominator = distinct persons whose observation window [start, censor] overlaps Y

    Washouts do NOT apply. Prevalence is a stock: a case that was prevalent at cohort
    entry is prevalent, and requiring clean lookback would delete exactly the cases
    prevalence is meant to count. Only incidence and at-risk person-time are gated.

    Carry-forward is controlled by `prevalence_carry_forward`:
      "ENROLLMENT" (default) -- active window is [dx, enrollment censor]. Because the
          denominator filter already guarantees enrollment_censor >= year_start, this
          reduces to `dx <= year_end`: once diagnosed, prevalent until leaving the
          cohort. Immune to a spuriously early/missing ALS censor date, which would
          otherwise silently drop a person from the numerator while leaving them in
          the denominator (prevalence biased downward).
      "ALS_CENSOR" -- active window is [dx, als_censor]. Use only if the ALS censor
          is genuinely independent of disenrollment (e.g. ALS-specific loss to
          follow-up). Run `censor_discordance()` first.

Incidence rate (year Y)
    numerator   = persons whose dx_date falls within Y, AFTER the applicable washout
    denominator = person-years AT RISK: observation time intersected with Y, starting
                  at entry + entry_washout_days, and truncated at diagnosis, so
                  prevalent cases contribute 0 at-risk time. Set
                  `at_risk_person_time=False` to drop the diagnosis truncation.

Adjustment
    Direct standardization over (age group x sex) strata. Age is evaluated at the
    mid-point (Jul 1) of each year so every person maps to one stratum per year.

    Weights are built from a PER-YEAR population (start_year..end_year), collapsed to
    a standard by `std_mode`. The default is the cohort's own structure POOLED over
    the study period: a fixed weight set, so the annual series stays comparable, and
    empty strata get zero weight (the reason not to use the 2000 US Standard on a
    Medicare cohort, where ~70% of that standard's weight sits below age 45).

    A standard that VARIES by year is available (std_mode="YEAR_SPECIFIC") but
    re-admits age structure into the year-over-year comparison, and reduces exactly to
    the crude rate when the standard is the cohort itself. Prefer POOLED or
    REFERENCE_YEAR unless you specifically want that.

Confidence intervals  (cfg.ci_method="EXACT", the default)
    Crude prevalence : Wilson score interval (binomial proportion)
    Crude incidence  : Byar's approximation to the exact Poisson interval
    Both adjusted    : Dobson et al. (1991), mapping the Poisson count interval onto
                       the standardized rate scale
    All closed-form, all evaluated in SQL -- no UDF or scipy round-trip. Chosen
    because ALS runs ~5-10 per 100,000, where symmetric Wald intervals undercover and
    emit negative lower bounds. cfg.ci_method="NORMAL" restores Wald if needed.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
import os
from typing import Sequence

from config import conn, env
from snowflake.snowpark import Column, DataFrame, Session
from snowflake.snowpark import functions as F
from snowflake.snowpark.window import Window


# --------------------------------------------------------------------------- #
# Configuration
# --------------------------------------------------------------------------- #
@dataclass(frozen=True)
class RateConfig:
    start_year: int = 2011
    end_year: int = 2025

    # denominator column names
    denom_table: str = "PAT_TABLE1"
    denom_person_col: str = "PATID"
    denom_start_col: str = "START_DATE"
    denom_censor_col: str = "CENSOR_DATE"
    denom_birth_col: str = "BIRTH_DATE"
    denom_sex_col: str = "SEX"
    denom_race_col: str = "RACE"

    # ALS numerator column names
    num_person_col: str = "PATID"
    num_dx_col: str = "DX_DATE"
    num_censor_col: str = "CENSOR_DATE"

    # DM (diabetes) cohort column names. DM is dual-role: a numerator for its own
    # prevalence out of the denominator, and a denominator for the ALS-in-DM measures.
    dm_person_col: str = "PATID"
    dm_dx_col: str = "DX_DATE"
    dm_censor_col: str = "CENSOR_DATE"
    dm_type_col: str = "T1DM_IND"
    # "ALL" (default), "T1DM", or "T2DM"
    dm_type: str = "ALL"

    # Medication registry column names (snapshot person-year rows).
    med_person_col: str = "PATID"
    med_year_col: str = "YEAR"
    med_denom_col: str = "DENOM"
    # Medication DENOM is selected from dm_type:
    #   dm_type=ALL  -> DENOM=DM
    #   dm_type=T1DM -> DENOM=T1DM
    #   dm_type=T2DM -> DENOM=T2DM
    med_type_col: str = "PHE_TYPE"
    med_phenotypes: Sequence[str] = (
        "DPP4", "GLP1RA", "SGLT2", "TZD", "MEG", "AGI",
        "SU", "BIGU", "AML", "INS", "COMBO",
    )


    # ----------------------------------------------------------------------- #
    # Incident-case ascertainment
    # ----------------------------------------------------------------------- #
    # Clean lookback required between DENOMINATOR ENTRY and the ALS diagnosis before
    # that diagnosis can be called INCIDENT. Without it, "first dx observed" is read
    # as "new dx", and a prevalent case surfacing on its first post-enrollment claim
    # is counted as an event -- concentrated in the years with the most new enrollees,
    # which is precisely where a spurious secular trend would appear.
    #
    # Applied to the NUMERATOR AND THE DENOMINATOR TOGETHER: the window accrues no
    # at-risk person-time. Excluding the case but keeping the time would bias
    # incidence DOWN, trading one error for another.
    #
    # Typical claims value is 365. NOTE THE COST: this deletes the first
    # entry_washout_days of every person's follow-up. If the cohort enters en masse at
    # start_year, the first year's incidence denominator collapses toward zero and its
    # rate is uninterpretable (prevalence is unaffected, so the row will still look
    # populated). Either set start_year earlier than the first REPORTING year, or drop
    # the lead-in years from the series. `qa_person_time` reports what was removed.
    entry_washout_days: int = 0

    # Clean lookback required between DM ONSET (+ dm_lag_days) and the ALS diagnosis
    # before that diagnosis counts as incident WITHIN the DM cohort.
    #
    # This is NOT a duplicate of dm_lag_days -- the two differ in where the window is
    # charged:
    #   dm_lag_days     -- induction/latency. Exposure has begun but is presumed not
    #                      yet acting -> window is UNEXPOSED -> non-DM person-time.
    #   dm_washout_days -- ascertainment. The person HAS diabetes, so the window is
    #                      not unexposed; it simply cannot support an incident
    #                      determination. It belongs to NEITHER arm, and is reported
    #                      as PERSON_YEARS_DM_WASHOUT / ALS_INCIDENT_IN_DM_WASHOUT.
    # Both may be set together. dm_entry = dm_dx + dm_lag_days + dm_washout_days.
    dm_washout_days: int = 0

    # Require DM to precede ALS for a case to count as incident ALS *within* DM.
    # Leave True. A person diagnosed with ALS in March and DM in July was not
    # diabetic when the event occurred; counting them attributes an outcome to an
    # exposure that had not yet begun (immortal time / exposure misclassification).
    # Setting this False is incompatible with dm_washout_days > 0 (a washout has no
    # meaning once event order is not enforced) and raises.
    require_dm_before_als: bool = True

    # Lag/induction: treat DM exposure as starting dm_dx + dm_lag_days. Guards against
    # reverse causation -- ALS prodromal weight loss and metabolic change can trigger
    # the encounters that surface a DM diagnosis, manufacturing a spurious DM->ALS
    # association. The lag window is treated as UNEXPOSED (standard lagged-exposure
    # convention), so it accrues to non-DM person-time. For a window that should
    # accrue to neither arm, use dm_washout_days instead.
    dm_lag_days: int = 0

    # Add ALS incidence among the NON-DM at-risk population. Measures 4 and 5 as
    # specified ("out of denominator" vs "out of DM") overlap -- DM patients sit in
    # both -- so their contrast is attenuated. DM vs non-DM is the clean comparison.
    include_non_dm: bool = True

    # left edges of age bands; last edge is the open-ended top band (e.g. 85 -> "85+")
    age_cutpoints: Sequence[int] = (
        0, 5, 10, 15, 20, 25, 30, 35, 40,
        45, 50, 55, 60, 65, 70, 75, 80, 85,
    )

    # Age restriction (both bounds INCLUSIVE; None = unrestricted). Evaluated at the
    # year mid-point, i.e. the SAME age used to assign AGE_GROUP, and applied to the
    # person-year row -- so one predicate restricts numerator, denominator and
    # person-time together and they cannot drift apart.
    #
    # min_age must be an age_cutpoint. A boundary that falls mid-band (min_age=18
    # against the default cutpoints) leaves a partial "15-19" stratum holding only
    # 18-19 year olds, which then collects the full 15-19 weight from any external
    # standard. Realign age_cutpoints to the restriction instead:
    #     RateConfig(min_age=18, age_cutpoints=(18, 20, 25, ..., 85))
    min_age: int | None = None
    max_age: int | None = None

    rate_multiplier: int = 100_000

    # DM prevalence in a Medicare population runs ~25-30%, not ~10 per 100,000. At
    # rate_multiplier it would report as ~275,000 per 100,000, which is technically
    # correct and useless to read. Per 100 (i.e. percent) is the sane unit; the
    # column is named with its unit so the mixed scale can't be misread. Also used
    # for DM prevalence within the ALS cohort, which runs in the same ~20-35% range.
    dm_prevalence_multiplier: int = 100
    med_prevalence_multiplier: int = 100

    days_per_year: float = 365.25
    at_risk_person_time: bool = True   # truncate incidence PY at diagnosis

    # "ENROLLMENT" -> carry cases forward to the enrollment censor (robust default)
    # "ALS_CENSOR" -> carry cases forward to the numerator's ALS censor date
    prevalence_carry_forward: str = "ENROLLMENT"

    # How to collapse a per-year population into standardization weights:
    #   "POOLED"         -> sum population over start_year..end_year (default).
    #                       Fixed weights => rates ARE comparable across years.
    #   "REFERENCE_YEAR" -> use std_reference_year's population as the fixed standard.
    #                       Fixed weights => comparable. Set std_reference_year.
    #   "YEAR_SPECIFIC"  -> each year standardized to its own population.
    #                       Weights vary by year => rates are NOT comparable across
    #                       years, and if the standard is the cohort itself this
    #                       returns the crude rate exactly. Use only deliberately.
    std_mode: str = "POOLED"
    std_reference_year: int | None = None

    add_ci: bool = True
    z: float = 1.959963985            # 95% normal quantile

    # "EXACT" (default) -> Wilson (crude prevalence), Byar/Poisson (crude incidence),
    #                      Dobson (both adjusted rates). Appropriate for rare events.
    # "NORMAL"          -> Wald intervals. Undercovers badly at ALS event rates.
    ci_method: str = "EXACT"

    round_rates: int = 2

    # Null out a measure family in any year whose case count is below this. CMS DUAs
    # generally require cell suppression at 11; leave None for internal analysis.
    suppress_threshold: int | None = None


# 2000 US Standard Population, per 1,000,000, aligned to the default 5-year bands
# (<1 and 1-4 collapsed into 0-4). Source: NCHS / Census P25-1130.
#
# NOT the default -- retained only for producing a secondary set of rates comparable
# with published literature, which is almost universally standardized to this. Note
# it places ~70% of its weight below age 45, so applied to a Medicare cohort it
# weights near-empty strata heavily and yields an unstable adjusted rate.
#
# Under an age restriction, adjusted_rates joins the standard INNER on the stratum
# keys, so bands outside the restriction find no cohort rows and drop out: the result
# is standardized to the restricted PORTION of this standard, renormalized. That is
# the conventional adult-onset presentation, but say so in any methods text.
STD_POP_2000 = {
    "00-04": 69135, "05-09": 72533, "10-14": 73032, "15-19": 72169,
    "20-24": 66478, "25-29": 64529, "30-34": 71044, "35-39": 80762,
    "40-44": 81851, "45-49": 72118, "50-54": 62716, "55-59": 48454,
    "60-64": 38793, "65-69": 34264, "70-74": 31773, "75-79": 26999,
    "80-84": 17842, "85+": 15508,
}


# --------------------------------------------------------------------------- #
# Helpers
# --------------------------------------------------------------------------- #
def _safe_divide(numerator: Column, denominator: Column) -> Column:
    """Guard the divisor itself, including inside SQL conditional branches."""
    return numerator / F.nullif(denominator, F.lit(0))


def _year_dim(session: Session, cfg: RateConfig) -> DataFrame:
    """One row per calendar year with year_start / year_end / mid_year dates."""
    rows = [[y] for y in range(cfg.start_year, cfg.end_year + 1)]
    ys = F.col("YEAR").cast("string")
    return (
        session.create_dataframe(rows, schema=["YEAR"])
        .with_column("YEAR_START", F.to_date(F.concat(ys, F.lit("-01-01"))))
        .with_column("YEAR_END", F.to_date(F.concat(ys, F.lit("-12-31"))))
        .with_column("MID_YEAR", F.to_date(F.concat(ys, F.lit("-07-01"))))
    )


def _age_group_expr(age: Column, cutpoints: Sequence[int]) -> Column:
    """Bucket an integer age column into zero-padded band labels ('00-04', '85+')."""
    edges = list(cutpoints)
    expr = F.when(age < edges[0], F.lit("UNKNOWN"))
    for lo, hi in zip(edges[:-1], edges[1:]):
        expr = expr.when((age >= lo) & (age < hi), F.lit(f"{lo:02d}-{hi - 1:02d}"))
    return expr.otherwise(F.lit(f"{edges[-1]:02d}+"))


def _norm_denom(denom: DataFrame, cfg: RateConfig) -> DataFrame:
    denom_cols_u = [c.upper().strip('"') for c in denom.columns]
    required = (
        cfg.denom_person_col,
        cfg.denom_start_col,
        cfg.denom_censor_col,
        cfg.denom_birth_col,
        cfg.denom_sex_col,
        cfg.denom_race_col,
    )
    missing = [req for req in required if req.upper() not in denom_cols_u]
    if missing:
        raise ValueError(
            f"Denominator {cfg.denom_table!r} is missing required columns: "
            f"{', '.join(missing)}. Check the denom_*_col settings and rebuild "
            "the source table if its schema is outdated."
        )
    return (
        denom
        .filter(F.col(cfg.denom_start_col).is_not_null())
        .select(
            F.col(cfg.denom_person_col).alias("PATID"),
            F.col(cfg.denom_start_col).alias("START_DATE"),
            F.col(cfg.denom_censor_col).alias("CENSOR_DATE"),
            F.col(cfg.denom_birth_col).alias("BIRTH_DATE"),
            F.col(cfg.denom_sex_col).alias("SEX"),
            F.col(cfg.denom_race_col).alias("RACE"),
        )
    )


def _norm_num(num: DataFrame, cfg: RateConfig) -> DataFrame:
    """Deduplicate to one ALS window per person (earliest dx, latest censor)."""
    return (
        num.group_by(cfg.num_person_col)
        .agg(
            F.min(cfg.num_dx_col).alias("ALS_DX_DATE"),
            F.max(cfg.num_censor_col).alias("ALS_CENSOR_DATE"),
        )
        .select(
            F.col(cfg.num_person_col).alias("ALS_PATID"),
            "ALS_DX_DATE",
            "ALS_CENSOR_DATE",
        )
    )


def _norm_dm(dm: DataFrame, cfg: RateConfig) -> DataFrame:
    """Deduplicate to one DM window per person (earliest dx, latest censor)."""
    dm_type = cfg.dm_type.upper()
    if dm_type not in ("ALL", "T1DM", "T2DM"):
        raise ValueError(f"dm_type must be 'ALL', 'T1DM' or 'T2DM', got {cfg.dm_type!r}")

    if dm_type == "T1DM":
        dm = dm.filter(F.col(cfg.dm_type_col).cast("int") == F.lit(1))
    elif dm_type == "T2DM":
        dm = dm.filter(F.col(cfg.dm_type_col).cast("int") == F.lit(0))

    return (
        dm.group_by(cfg.dm_person_col)
        .agg(
            F.min(cfg.dm_dx_col).alias("DM_DX_DATE"),
            F.max(cfg.dm_censor_col).alias("DM_CENSOR_DATE"),
        )
        .select(
            F.col(cfg.dm_person_col).alias("DM_PATID"),
            "DM_DX_DATE",
            "DM_CENSOR_DATE",
        )
    )


def _norm_dm_with_type(dm: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Deduplicate DM to one window per person and retain a patient-level DM type.

    DM_TYPE is assigned from the earliest available DM row:
      - T1DM when dm_type_col == 1
      - T2DM when dm_type_col == 0
      - UNKNOWN otherwise
    """
    dm_base = dm.select(
        F.col(cfg.dm_person_col).alias("DM_PATID"),
        F.col(cfg.dm_dx_col).alias("DM_DX_DATE"),
        F.col(cfg.dm_censor_col).alias("DM_CENSOR_DATE"),
        F.col(cfg.dm_type_col).cast("int").alias("DM_TYPE_FLAG"),
    )

    dm_rollup = (
        dm_base.group_by("DM_PATID")
        .agg(
            F.min("DM_DX_DATE").alias("DM_DX_DATE"),
            F.max("DM_CENSOR_DATE").alias("DM_CENSOR_DATE"),
        )
    )

    w = Window.partition_by("DM_PATID").order_by(
        F.col("DM_DX_DATE"),
        F.col("DM_CENSOR_DATE"),
        F.col("DM_TYPE_FLAG"),
    )

    dm_type = (
        dm_base
        .with_column("rn", F.row_number().over(w))
        .filter(F.col("rn") == F.lit(1))
        .select(
            "DM_PATID",
            F.when(F.col("DM_TYPE_FLAG") == F.lit(1), F.lit("T1DM"))
            .when(F.col("DM_TYPE_FLAG") == F.lit(0), F.lit("T2DM"))
            .otherwise(F.lit("UNKNOWN"))
            .alias("DM_TYPE"),
        )
    )

    return (
        dm_rollup.join(dm_type, on="DM_PATID", how="left")
        .with_column("DM_TYPE", F.coalesce(F.col("DM_TYPE"), F.lit("UNKNOWN")))
    )


def annual_prevalence_cohort_profile(
    denom: DataFrame,
    num: DataFrame,
    dm: DataFrame,
    cfg: RateConfig,
    white_labels: Sequence[str] = ("WHITE",),
) -> DataFrame:
    """
    Annual cohort composition for prevalence cohorts.

    Output columns:
      YEAR, COHORT, N,
      AVG_AGE, SD_AGE,
      MALE_PROP, SD_MALE_PROP,
      NON_WHITE_PROP, SD_NON_WHITE_PROP

        Cohorts (ALL and ALS are always included; the others are driven by cfg.dm_type):
            ALL  -> ALL, ALS, DM, NON_DM, ALS_WITH_DM, ALS_WITH_NON_DM
            T1DM -> ALL, ALS, T1DM, NON_DM, ALS_WITH_T1DM, ALS_WITH_NON_DM
            T2DM -> ALL, ALS, T2DM, NON_DM, ALS_WITH_T2DM, ALS_WITH_NON_DM
    """
    if dm is None:
        raise ValueError("dm is required for prevalence cohort composition summaries")

    if cfg.prevalence_carry_forward.upper() not in ("ENROLLMENT", "ALS_CENSOR"):
        raise ValueError(
            "prevalence_carry_forward must be 'ENROLLMENT' or 'ALS_CENSOR', "
            f"got {cfg.prevalence_carry_forward!r}"
        )

    session = denom._session
    d = _norm_denom(denom, cfg)
    a = _norm_num(num, cfg)
    dm_typed = _norm_dm_with_type(dm, cfg)
    years = _year_dim(session, cfg).select("YEAR", "YEAR_START", "YEAR_END", "MID_YEAR")

    base = (
        d.cross_join(years)
        .filter(
            (F.col("START_DATE") <= F.col("YEAR_END"))
            & (F.col("CENSOR_DATE") >= F.col("YEAR_START"))
        )
        .join(a, F.col("PATID") == F.col("ALS_PATID"), how="left")
        .join(dm_typed, F.col("PATID") == F.col("DM_PATID"), how="left")
    )

    age = F.floor(
        _safe_divide(
            F.datediff("day", F.col("BIRTH_DATE"), F.col("MID_YEAR")),
            F.lit(cfg.days_per_year),
        )
    ).cast("int")
    base = base.with_column("AGE", age)

    if cfg.min_age is not None and cfg.max_age is not None and cfg.min_age > cfg.max_age:
        raise ValueError(f"min_age ({cfg.min_age}) > max_age ({cfg.max_age})")
    if cfg.min_age is not None:
        base = base.filter(F.col("AGE") >= F.lit(cfg.min_age))
    if cfg.max_age is not None:
        base = base.filter(F.col("AGE") <= F.lit(cfg.max_age))

    def _prevalent_carry_forward(dx: Column, censor: Column) -> Column:
        p = dx.is_not_null() & (dx <= F.col("YEAR_END")) & (dx <= F.col("CENSOR_DATE"))
        if cfg.prevalence_carry_forward.upper() == "ALS_CENSOR":
            p = p & (censor >= F.col("YEAR_START"))
        return p

    als_prev = _prevalent_carry_forward(F.col("ALS_DX_DATE"), F.col("ALS_CENSOR_DATE"))
    dm_prev = _prevalent_carry_forward(F.col("DM_DX_DATE"), F.col("DM_CENSOR_DATE"))
    dm_t1_prev = dm_prev & (F.col("DM_TYPE") == F.lit("T1DM"))
    dm_t2_prev = dm_prev & (F.col("DM_TYPE") == F.lit("T2DM"))

    white_set = tuple(w.upper() for w in white_labels)
    sex_u = F.upper(F.trim(F.col("SEX")))
    race_u = F.upper(F.trim(F.col("RACE")))

    base = (
        base.with_column("IS_MALE", F.iff(sex_u == F.lit("M"), F.lit(1.0), F.lit(0.0)))
        .with_column(
            "IS_NON_WHITE",
            F.iff(
                F.col("RACE").is_not_null() & ~race_u.isin(list(white_set)),
                F.lit(1.0),
                F.lit(0.0),
            ),
        )
    )

    dm_type = cfg.dm_type.upper()
    if dm_type == "ALL":
        dm_label = "DM"
        dm_pred = dm_prev
        als_dm_label = "ALS_WITH_DM"
    elif dm_type == "T1DM":
        dm_label = "T1DM"
        dm_pred = dm_t1_prev
        als_dm_label = "ALS_WITH_T1DM"
    elif dm_type == "T2DM":
        dm_label = "T2DM"
        dm_pred = dm_t2_prev
        als_dm_label = "ALS_WITH_T2DM"
    else:
        raise ValueError(f"dm_type must be 'ALL', 'T1DM' or 'T2DM', got {cfg.dm_type!r}")

    non_dm_pred = ~F.coalesce(dm_pred, F.lit(False))

    cohort_defs = [
        ("ALL", F.lit(True)),
        ("ALS", als_prev),
        (dm_label, dm_pred),
        ("NON_DM", non_dm_pred),
        (als_dm_label, als_prev & dm_pred),
        ("ALS_WITH_NON_DM", als_prev & non_dm_pred),
    ]

    def _summarize(name: str, pred: Column) -> DataFrame:
        return (
            base.filter(pred)
            .group_by("YEAR")
            .agg(
                F.count_distinct("PATID").alias("N"),
                F.avg("AGE").alias("AVG_AGE"),
                F.stddev_samp("AGE").alias("SD_AGE"),
                F.avg("IS_MALE").alias("MALE_PROP"),
                F.stddev_samp("IS_MALE").alias("SD_MALE_PROP"),
                F.avg("IS_NON_WHITE").alias("NON_WHITE_PROP"),
                F.stddev_samp("IS_NON_WHITE").alias("SD_NON_WHITE_PROP"),
            )
            .with_column("COHORT", F.lit(name))
        )

    cohort_summ = None
    for name, pred in cohort_defs:
        one = _summarize(name, pred)
        cohort_summ = one if cohort_summ is None else cohort_summ.union_all(one)

    cohort_dim = session.create_dataframe(
        [[name] for name, _ in cohort_defs],
        schema=["COHORT"],
    )
    year_cohort = years.select("YEAR").cross_join(cohort_dim)

    out = (
        year_cohort.join(cohort_summ, on=["YEAR", "COHORT"], how="left")
        .select(
            F.col("YEAR"),
            F.col("COHORT"),
            F.coalesce(F.col("N"), F.lit(0)).alias("N"),
            F.round(F.col("AVG_AGE"), 3).alias("AVG_AGE"),
            F.round(F.col("SD_AGE"), 3).alias("SD_AGE"),
            F.round(F.col("MALE_PROP"), 6).alias("MALE_PROP"),
            F.round(F.col("SD_MALE_PROP"), 6).alias("SD_MALE_PROP"),
            F.round(F.col("NON_WHITE_PROP"), 6).alias("NON_WHITE_PROP"),
            F.round(F.col("SD_NON_WHITE_PROP"), 6).alias("SD_NON_WHITE_PROP"),
        )
        .sort("YEAR", "COHORT")
    )

    return out


def _med_key(phe_type: str) -> str:
    key = "".join(ch if ch.isalnum() else "_" for ch in phe_type.upper()).strip("_")
    if not key:
        raise ValueError(f"invalid medication phenotype label: {phe_type!r}")
    return key


def _med_denom_values(cfg: RateConfig) -> list[str]:
    dm_type = cfg.dm_type.upper()
    if dm_type == "ALL":
        return ["DM"]
    if dm_type == "T1DM":
        return ["T1DM"]
    if dm_type == "T2DM":
        return ["T2DM"]
    raise ValueError(f"dm_type must be 'ALL', 'T1DM' or 'T2DM', got {cfg.dm_type!r}")


def _norm_med(med: DataFrame, cfg: RateConfig) -> DataFrame:
    """Normalize snapshot rows to PATID, YEAR, DENOM, PHE_TYPE (uppercase labels)."""
    return (
        med.select(
            F.col(cfg.med_person_col).alias("MED_PATID"),
            F.col(cfg.med_year_col).cast("int").alias("YEAR"),
            F.upper(F.col(cfg.med_denom_col)).alias("MED_DENOM"),
            F.upper(F.col(cfg.med_type_col)).alias("MED_PHE_TYPE"),
        )
    )


# --------------------------------------------------------------------------- #
# Measure registry
#
# Every rate in this module is (numerator count) / (denominator count or person-time)
# over the same (YEAR, AGE_GROUP, SEX) strata. Declaring them makes crude_rates,
# adjusted_rates and final_table generic loops rather than hand-written blocks,
# so a new measure is one entry here and nothing else.
# --------------------------------------------------------------------------- #
@dataclass(frozen=True)
class Measure:
    name: str            # output column stem, e.g. ALS_INCIDENCE_IN_DM
    numerator: str       # stratum column
    denominator: str     # stratum column
    kind: str            # "PREVALENCE" (binomial proportion) | "INCIDENCE" (Poisson rate)
    outcome: str
    population: str
    multiplier_attr: str = "rate_multiplier"
    unit: str = "per 100,000"
    description: str = ""

    def multiplier(self, cfg: RateConfig) -> int:
        return getattr(cfg, self.multiplier_attr)


def measures(cfg: RateConfig) -> list[Measure]:
    """The measure set, in report order."""
    ms = [
        Measure(
            "DM_PREVALENCE_PCT", "DM_PREVALENT_CASES", "N_OBSERVED", "PREVALENCE",
            outcome="DM", population="ALL",
            multiplier_attr="dm_prevalence_multiplier", unit="percent",
            description="DM prevalence out of the full denominator cohort",
        ),
        Measure(
            "ALS_PREVALENCE", "ALS_PREVALENT_CASES", "N_OBSERVED", "PREVALENCE",
            outcome="ALS", population="ALL",
            description="ALS prevalence out of the full denominator cohort",
        ),
        Measure(
            "ALS_PREVALENCE_IN_DM", "ALS_DM_PREVALENT_CASES", "DM_PREVALENT_CASES",
            "PREVALENCE", outcome="ALS", population="DM",
            description="ALS prevalence out of the DM cohort (both conditions active)",
        ),
        # Mirror of the above: same numerator, ALS cohort as the denominator. Reported
        # as a percent (~20-35% in an ALS/Medicare-age cohort), not per 100,000, for
        # the same reason as DM_PREVALENCE_PCT.
        Measure(
            "DM_PREVALENCE_IN_ALS_PCT", "ALS_DM_PREVALENT_CASES", "ALS_PREVALENT_CASES",
            "PREVALENCE",
            outcome="DM", population="ALS",
            multiplier_attr="dm_prevalence_multiplier", unit="percent",
            description="DM prevalence out of the ALS cohort (both conditions active); "
                        "mirror of ALS_PREVALENCE_IN_DM, same numerator",
        ),
        Measure(
            "ALS_INCIDENCE", "ALS_INCIDENT_CASES", "PERSON_YEARS", "INCIDENCE",
            outcome="ALS", population="ALL",
            unit="per 100,000 PY",
            description="ALS incidence out of the full denominator cohort, "
                        "after entry_washout_days of clean lookback",
        ),
        Measure(
            "ALS_INCIDENCE_IN_DM", "ALS_INCIDENT_IN_DM_CASES", "PERSON_YEARS_DM",
            "INCIDENCE", outcome="ALS", population="DM", unit="per 100,000 PY",
            description="ALS incidence out of the DM cohort, left-truncated at DM "
                        "onset + lag + washout",
        ),
    ]
    if cfg.include_non_dm:
        ms.append(
            Measure(
                "ALS_INCIDENCE_IN_NON_DM", "ALS_INCIDENT_IN_NON_DM_CASES",
                "PERSON_YEARS_NON_DM", "INCIDENCE", unit="per 100,000 PY",
                outcome="ALS", population="NON_DM",
                description="ALS incidence in at-risk time before/without DM "
                            "(the clean comparator for ALS_INCIDENCE_IN_DM)",
            )
        )
    return ms


def measure_dictionary(cfg: RateConfig) -> str:
    """Human-readable description of every output measure, its unit, and the
    restrictions in force for this config."""
    lines = [f"{'measure':<26} {'numerator':<30} {'denominator':<22} unit"]
    lines.append("-" * 96)
    for m in measures(cfg):
        lines.append(f"{m.name:<26} {m.numerator:<30} {m.denominator:<22} {m.unit}")
        lines.append(f"{'':<26} {m.description}")

    notes = []
    if cfg.min_age is not None or cfg.max_age is not None:
        lo = "-inf" if cfg.min_age is None else cfg.min_age
        hi = "+inf" if cfg.max_age is None else cfg.max_age
        notes.append(
            f"AGE: restricted to {lo}..{hi} inclusive, evaluated at the year mid-point."
        )
    if cfg.entry_washout_days:
        notes.append(
            f"ENTRY WASHOUT: incidence requires {cfg.entry_washout_days} days of clean "
            "lookback after cohort entry; that window accrues no at-risk person-time. "
            "Prevalence is NOT gated. Incidence in the first "
            f"~{cfg.entry_washout_days / 365.25:.1f} year(s) of the series is lead-in "
            "and should not be reported."
        )
    if cfg.dm_lag_days:
        notes.append(
            f"DM LAG: exposure lagged {cfg.dm_lag_days} days; the lag window counts as "
            "UNEXPOSED -> non-DM person-time."
        )
    if cfg.dm_washout_days:
        notes.append(
            f"DM WASHOUT: DM measures require {cfg.dm_washout_days} days of clean "
            "lookback after DM onset (+ lag); that window is charged to NEITHER arm "
            "(see PERSON_YEARS_DM_WASHOUT in the stratum/QA frames)."
        )
    if notes:
        lines.append("")
        lines.extend(notes)
    return "\n".join(lines)


# --------------------------------------------------------------------------- #
# Standard population
#
# All builders return either
#     (AGE_GROUP, SEX, STD_POP)          -- fixed standard, comparable across years
# or  (YEAR, AGE_GROUP, SEX, STD_POP)    -- year-specific, NOT comparable
# depending on cfg.std_mode. adjusted_rates() detects which by looking for YEAR.
# --------------------------------------------------------------------------- #
def _collapse_standard(pop: DataFrame, cfg: RateConfig) -> DataFrame:
    """Reduce a per-year (YEAR, AGE_GROUP, SEX, STD_POP) frame per cfg.std_mode."""
    mode = cfg.std_mode.upper()
    pop = pop.filter(F.col("YEAR").between(cfg.start_year, cfg.end_year))

    if mode == "POOLED":
        # Sum across all years -> one fixed weight per stratum. Uses the full study
        # period's age-sex structure, so no single year's composition dominates.
        return pop.group_by("AGE_GROUP", "SEX").agg(F.sum("STD_POP").alias("STD_POP"))

    if mode == "REFERENCE_YEAR":
        if cfg.std_reference_year is None:
            raise ValueError("std_mode='REFERENCE_YEAR' requires std_reference_year")
        if not cfg.start_year <= cfg.std_reference_year <= cfg.end_year:
            raise ValueError("std_reference_year must fall within start_year..end_year")
        return pop.filter(F.col("YEAR") == F.lit(cfg.std_reference_year)).select(
            "AGE_GROUP", "SEX", "STD_POP"
        )

    if mode == "YEAR_SPECIFIC":
        return pop.select("YEAR", "AGE_GROUP", "SEX", "STD_POP")

    raise ValueError(
        "std_mode must be 'POOLED', 'REFERENCE_YEAR' or 'YEAR_SPECIFIC', "
        f"got {cfg.std_mode!r}"
    )


def standard_population_from_cohort(strata: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Internal standard: the study cohort's own per-year age x sex structure.

    Weights are N_OBSERVED (distinct persons observed in the year), NOT PERSON_YEARS
    -- the person-years column is at-risk time truncated at diagnosis and gated by the
    entry washout, which would understate exactly the strata where ALS is common and
    the years with the most new enrollees, biasing the weights.

    Empty strata carry zero weight automatically, which is the main advantage over an
    external general-population standard for a Medicare cohort.

    Inherits cfg.min_age / cfg.max_age for free: `strata` is downstream of the
    person-year age filter, so the standard is the RESTRICTED cohort's structure.
    """
    pop = strata.select(
        "YEAR",
        "AGE_GROUP",
        "SEX",
        F.col("N_OBSERVED").cast("float").alias("STD_POP"),
    )
    return _collapse_standard(pop, cfg)


def standard_population_from_table(
    pop_df: DataFrame,
    cfg: RateConfig,
    year_col: str = "YEAR",
    sex_col: str = "SEX",
    pop_col: str = "POPULATION",
    age_group_col: str | None = "AGE_GROUP",
    age_col: str | None = None,
) -> DataFrame:
    """
    External standard from a per-year population table (e.g. Census/SEER bridged-race
    or Medicare enrollment counts by year x age x sex), covering start_year..end_year.

    Supply either `age_group_col` (labels must match cfg.age_cutpoints bands) or
    `age_col` (single year of age, bucketed here with the same cutpoints).

    Age restriction is NOT applied here -- it does not need to be. adjusted_rates()
    joins the standard INNER on the stratum keys, so bands with no cohort rows
    contribute no weight and the standard is effectively renormalized to the
    restricted range. If you pass age_col, out-of-range ages bucket to "UNKNOWN" or a
    band the cohort does not have and drop out on the same join.
    """
    if (age_group_col is None) == (age_col is None):
        raise ValueError("supply exactly one of age_group_col or age_col")

    ag = (
        F.col(age_group_col)
        if age_col is None
        else _age_group_expr(F.col(age_col).cast("int"), cfg.age_cutpoints)
    )
    pop = (
        pop_df.select(
            F.col(year_col).alias("YEAR"),
            ag.alias("AGE_GROUP"),
            F.col(sex_col).alias("SEX"),
            F.col(pop_col).cast("float").alias("STD_POP"),
        )
        .group_by("YEAR", "AGE_GROUP", "SEX")  # re-aggregate if age was bucketed here
        .agg(F.sum("STD_POP").alias("STD_POP"))
    )
    return _collapse_standard(pop, cfg)


def standard_population_2000(
    session: Session,
    cfg: RateConfig,
    sex_values: Sequence = ("F", "M"),
    sex_split: dict | None = None,
) -> DataFrame:
    """
    Fixed 2000 US Standard Population. Use ONLY for a secondary set of rates intended
    to be comparable with published literature -- see the note on STD_POP_2000.

    Its bands are the DEFAULT 5-year cutpoints. If cfg.age_cutpoints has been
    realigned to an age restriction (e.g. starting at 18), the labels will not match
    on the join and this standard cannot be used as-is; rebuild the dict against the
    new bands, or standardize to the cohort instead.
    """
    split = sex_split or {s: 1.0 / len(sex_values) for s in sex_values}
    rows = [
        [ag, s, float(w) * split[s]]
        for ag, w in STD_POP_2000.items()
        for s in sex_values
    ]
    return session.create_dataframe(rows, schema=["AGE_GROUP", "SEX", "STD_POP"])


# --------------------------------------------------------------------------- #
# Data quality
# --------------------------------------------------------------------------- #
def censor_discordance(denom: DataFrame, num: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Quantify agreement between the ALS censor date and the enrollment censor date.

    If the two are meant to coincide, CONCORDANT should dominate and the remaining
    buckets are data-quality signals rather than real ALS-specific follow-up:

      ALS_CENSOR_AFTER_ENROLL : inconsistent -- ALS followed past disenrollment/death.
                                Under "ALS_CENSOR" this extends prevalence beyond the
                                observation window; under "ENROLLMENT" it is ignored.
      ALS_CENSOR_BEFORE_ENROLL: person stays in the denominator but leaves the
                                numerator. Under "ALS_CENSOR" this biases prevalence
                                downward; inspect the day gap before trusting it.
      DX_AFTER_ENROLL_CENSOR  : diagnosis recorded after end of observation.
      NOT_IN_DENOM            : numerator person absent from the denominator entirely.

    Returns one row per bucket with counts and gap percentiles (in days). Diagnostic
    only -- deliberately NOT age-restricted and NOT washout-gated, since a censor-date
    problem is a property of the source tables, not of the analytic window.
    """
    d = _norm_denom(denom, cfg)
    a = _norm_num(num, cfg)

    j = a.join(d, F.col("ALS_PATID") == F.col("PATID"), how="left")
    gap = F.datediff("day", F.col("CENSOR_DATE"), F.col("ALS_CENSOR_DATE"))

    bucket = (
        F.when(F.col("PATID").is_null(), F.lit("NOT_IN_DENOM"))
        .when(F.col("ALS_DX_DATE") > F.col("CENSOR_DATE"), F.lit("DX_AFTER_ENROLL_CENSOR"))
        .when(F.col("ALS_CENSOR_DATE").is_null(), F.lit("ALS_CENSOR_NULL"))
        .when(gap == 0, F.lit("CONCORDANT"))
        .when(gap > 0, F.lit("ALS_CENSOR_AFTER_ENROLL"))
        .otherwise(F.lit("ALS_CENSOR_BEFORE_ENROLL"))
    )

    grouped = (
        j.with_column("BUCKET", bucket)
        .with_column("GAP_DAYS", gap)
        .group_by("BUCKET")
        .agg(
            F.count("*").alias("N_PERSONS"),
            F.median(F.abs(F.col("GAP_DAYS"))).alias("MEDIAN_ABS_GAP_DAYS"),
            F.max(F.abs(F.col("GAP_DAYS"))).alias("MAX_ABS_GAP_DAYS"),
        )
    )

    total = F.sum(F.col("N_PERSONS")).over()
    return (
        grouped.with_column("PCT", F.round(_safe_divide(F.col("N_PERSONS") * 100.0, total), 3))
        .sort(F.col("N_PERSONS").desc())
    )


def washout_attrition(denom: DataFrame, num: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Person-level accounting of what the ENTRY washout removes from the ALS risk set.

    One row per bucket, over persons with an ALS diagnosis who appear in the
    denominator:

      DX_BEFORE_ENTRY       : dx precedes cohort entry outright. Never incident under
                              any washout -- and note the ungated code would have
                              counted them if the dx year overlapped their observation
                              window, which is a numerator with zero at-risk time.
      DX_IN_ENTRY_WASHOUT   : dx within entry_washout_days of entry. Cannot be shown
                              to be new -> excluded from the numerator, and the window
                              contributes no at-risk time.
      INCIDENT_AFTER_WASHOUT: dx after clean lookback -> eligible.

    Run this before trusting an incidence series. If DX_IN_ENTRY_WASHOUT is a large
    share, the ungated rates were substantially prevalence contamination; if it is
    large AND concentrated in early years, any apparent secular trend was an
    enrollment artifact.

    Not age-restricted: a person-level attrition view, not the analytic table.
    """
    d = _norm_denom(denom, cfg)
    a = _norm_num(num, cfg)

    j = a.join(d, F.col("ALS_PATID") == F.col("PATID"), how="inner")
    wo_end = F.dateadd("day", F.lit(cfg.entry_washout_days), F.col("START_DATE"))
    gap = F.datediff("day", F.col("START_DATE"), F.col("ALS_DX_DATE"))

    bucket = (
        F.when(F.col("ALS_DX_DATE") < F.col("START_DATE"), F.lit("DX_BEFORE_ENTRY"))
        .when(F.col("ALS_DX_DATE") < wo_end, F.lit("DX_IN_ENTRY_WASHOUT"))
        .otherwise(F.lit("INCIDENT_AFTER_WASHOUT"))
    )

    grouped = (
        j.with_column("BUCKET", bucket)
        .with_column("DAYS_FROM_ENTRY_TO_DX", gap)
        .group_by("BUCKET")
        .agg(
            F.count("*").alias("N_PERSONS"),
            F.median("DAYS_FROM_ENTRY_TO_DX").alias("MEDIAN_DAYS_ENTRY_TO_DX"),
            F.min("DAYS_FROM_ENTRY_TO_DX").alias("MIN_DAYS_ENTRY_TO_DX"),
            F.max("DAYS_FROM_ENTRY_TO_DX").alias("MAX_DAYS_ENTRY_TO_DX"),
        )
    )
    total = F.sum(F.col("N_PERSONS")).over()
    return (
        grouped.with_column("PCT", F.round(_safe_divide(F.col("N_PERSONS") * 100.0, total), 3))
        .sort(F.col("N_PERSONS").desc())
    )


# --------------------------------------------------------------------------- #
# Person x year x stratum base table
# --------------------------------------------------------------------------- #
def build_person_year(
    denom: DataFrame,
    num: DataFrame,
    cfg: RateConfig,
    dm: DataFrame | None = None,
) -> DataFrame:
    """
    One row per (person, year) the person is observed in, carrying the stratum keys,
    every measure's person-level indicator, and every measure's person-time.

    Washouts (incident-case ascertainment)
    --------------------------------------
    Two anchors, one rule at each: a diagnosis inside the washout is not an event, AND
    the washout accrues no at-risk person-time. Gating only the numerator would bias
    incidence DOWN -- person-time during which an event cannot be counted must leave
    the denominator too.

      entry_washout_days -> ar_start moves to START_DATE + entry_washout_days, and
          als_inc requires ALS_DX >= that date. A person whose dx falls inside the
          window is excluded by BOTH gates at once: their ar_end (truncated at dx)
          lands before ar_start, so days clamp to 0, and their incident flag is false.
          No separate exclusion pass is needed.

      dm_washout_days -> the window [dm_dx + lag, dm_dx + lag + washout] is charged to
          NEITHER arm. It is not unexposed (the person has diabetes), so it must not
          go to non-DM; and it cannot support an incident determination, so it must
          not go to DM. It is reported as PY_AT_RISK_DM_WASHOUT /
          ALS_INCIDENT_IN_DM_WASHOUT, which is what keeps the partition below exact.

    Prevalence is NOT gated. A prevalent case at entry IS prevalent; requiring clean
    lookback would delete exactly what prevalence exists to count.

    Age restriction
    ---------------
    cfg.min_age / cfg.max_age filter the person-year row on AGE -- the age at the
    year mid-point, the same value that assigns AGE_GROUP. Because every numerator
    indicator, N_OBSERVED and all person-time columns are selected from this frame
    AFTER the filter, one predicate restricts them all. Two consequences:

      * The filter is PER PERSON-YEAR, not per person. Someone turning 18 in
        September is 17 at the Jul 1 evaluation point, so their whole year --
        including Sept-Dec person-time and any event in it -- is excluded; they enter
        in the following year. That is the price of one-stratum-per-person-per-year,
        and it is immaterial at the 18 boundary for ALS. At a max_age boundary it is
        NOT immaterial: mortality is high there and half a year of dropped time is
        real.
      * Prevalent cases diagnosed BELOW the restriction still count. A person
        diagnosed at 16 and observed at 20 is in ALS_PREVALENT_CASES (carry-forward)
        while contributing 0 at-risk PY and no incident case. Correct on both counts
        -- prevalence is a stock, incidence a flow -- but the measures share a set of
        person-years, not a set of cases.

    Person-time columns
    -------------------
    PY_AT_RISK             at-risk time in the year: [entry + entry_washout, censor]
                           intersected with the year, truncated at ALS dx
    PY_AT_RISK_DM          as above, LEFT-truncated at dm_dx + lag + washout
    PY_AT_RISK_DM_WASHOUT  as above, restricted to the DM washout window (neither arm)
    PY_AT_RISK_NON_DM      as above, RIGHT-truncated at dm_dx + lag
    PY_EXCL_ENTRY_WASHOUT  time removed by the entry washout -- outside PY_AT_RISK
                           entirely, reported so the cost is a visible number rather
                           than a silent difference

    The first four partition EXACTLY, for any setting of dm_lag_days and
    dm_washout_days: the three DM pieces are consecutive sub-intervals of the same
    [ar_start, ar_end]. `qa_person_time` asserts it. (An earlier version folded the
    washout into the lag and had to tolerate a gap; giving the washout window its own
    column removed the need for that exception.)
    """
    session = denom._session
    d = _norm_denom(denom, cfg)
    a = _norm_num(num, cfg)
    years = _year_dim(session, cfg)

    # ---- parameter validation ----
    if cfg.entry_washout_days < 0:
        raise ValueError(f"entry_washout_days must be >= 0, got {cfg.entry_washout_days}")
    if cfg.dm_washout_days < 0:
        raise ValueError(f"dm_washout_days must be >= 0, got {cfg.dm_washout_days}")
    if cfg.dm_lag_days < 0:
        raise ValueError(f"dm_lag_days must be >= 0, got {cfg.dm_lag_days}")
    if cfg.dm_washout_days and not cfg.require_dm_before_als:
        # A washout measures clean DM-observed time BEFORE the event. With event order
        # unenforced there is no "before", and the DM / DM-washout / non-DM case
        # partition would silently stop summing to ALS_INCIDENT_CASES.
        raise ValueError(
            "dm_washout_days > 0 requires require_dm_before_als=True; a washout is "
            "meaningless once event order is not enforced"
        )

    ycols = ["YEAR", "YEAR_START", "YEAR_END", "MID_YEAR"]

    base = (
        d.cross_join(years.select(*ycols))
        # observed in year Y: observation window overlaps [year_start, year_end]
        .filter(
            (F.col("START_DATE") <= F.col("YEAR_END"))
            & (F.col("CENSOR_DATE") >= F.col("YEAR_START"))
        )
        .join(a, F.col("PATID") == F.col("ALS_PATID"), how="left")
    )

    if dm is not None:
        base = base.join(
            _norm_dm(dm, cfg), F.col("PATID") == F.col("DM_PATID"), how="left"
        )
    else:
        base = base.with_columns(
            ["DM_DX_DATE", "DM_CENSOR_DATE"],
            [F.lit(None).cast("date"), F.lit(None).cast("date")],
        )

    # age at mid-year -> stratum
    age = F.floor(
        _safe_divide(
            F.datediff("day", F.col("BIRTH_DATE"), F.col("MID_YEAR")),
            F.lit(cfg.days_per_year),
        )
    ).cast("int")
    base = base.with_column("AGE", age).with_column(
        "AGE_GROUP", _age_group_expr(F.col("AGE"), cfg.age_cutpoints)
    )

    # ---- age restriction (single predicate over numerator, denominator, PY) ----
    if cfg.min_age is not None and cfg.max_age is not None and cfg.min_age > cfg.max_age:
        raise ValueError(f"min_age ({cfg.min_age}) > max_age ({cfg.max_age})")
    # A restriction boundary that is not a cutpoint leaves a PARTIAL band: min_age=18
    # against the default cutpoints yields a "15-19" stratum holding only 18-19 year
    # olds, which then receives the full 15-19 weight from any external standard.
    # Realign age_cutpoints to the restriction rather than relaxing this check.
    if cfg.min_age is not None and cfg.min_age not in tuple(cfg.age_cutpoints):
        raise ValueError(
            f"min_age={cfg.min_age} is not in age_cutpoints; the boundary band would "
            "be partial. Set age_cutpoints to start at min_age."
        )
    if cfg.min_age is not None:
        base = base.filter(F.col("AGE") >= F.lit(cfg.min_age))
    if cfg.max_age is not None:
        base = base.filter(F.col("AGE") <= F.lit(cfg.max_age))

    if cfg.prevalence_carry_forward.upper() not in ("ENROLLMENT", "ALS_CENSOR"):
        raise ValueError(
            "prevalence_carry_forward must be 'ENROLLMENT' or 'ALS_CENSOR', "
            f"got {cfg.prevalence_carry_forward!r}"
        )

    # Carry-forward prevalence (window-based): condition active window overlaps year.
    def _prevalent_carry_forward(dx: Column, censor: Column) -> Column:
        p = dx.is_not_null() & (dx <= F.col("YEAR_END")) & (dx <= F.col("CENSOR_DATE"))
        if cfg.prevalence_carry_forward.upper() == "ALS_CENSOR":
            p = p & (censor >= F.col("YEAR_START"))
        return p

    als_prev = _prevalent_carry_forward(F.col("ALS_DX_DATE"), F.col("ALS_CENSOR_DATE"))
    dm_prev = _prevalent_carry_forward(F.col("DM_DX_DATE"), F.col("DM_CENSOR_DATE"))

    # ---- anchors ----
    # Entry anchor: incidence eligibility begins here, and so does at-risk time.
    entry_eligible = F.dateadd("day", F.lit(cfg.entry_washout_days), F.col("START_DATE"))

    # DM anchors. TWO dates, because the lag window and the washout window are charged
    # differently: lag -> non-DM (unexposed), washout -> neither.
    #   dm_lag_end : non-DM person-time ends here
    #   dm_entry   : DM person-time and DM-incidence eligibility begin here
    dm_lag_end = F.dateadd("day", F.lit(cfg.dm_lag_days), F.col("DM_DX_DATE"))
    dm_entry = F.dateadd(
        "day", F.lit(cfg.dm_lag_days + cfg.dm_washout_days), F.col("DM_DX_DATE")
    )
    has_dm = F.col("DM_DX_DATE").is_not_null()

    # ---- incidence numerators ----
    dx_in_year = F.col("ALS_DX_DATE").between(F.col("YEAR_START"), F.col("YEAR_END"))
    # A new ALS dx, with enough clean lookback after entry to call it new.
    als_inc = dx_in_year & (F.col("ALS_DX_DATE") >= entry_eligible)
    # What the entry washout removed -- attrition, not a measure. Reported in QA.
    als_inc_excl = dx_in_year & (F.col("ALS_DX_DATE") < entry_eligible)

    # Incident ALS *within* DM requires the exposure to precede the event by at least
    # the lag + washout. Without `dm_entry <= ALS_DX_DATE`, a person whose ALS preceded
    # their diabetes would be counted as a DM-cohort event -- attributing an outcome to
    # an exposure that had not started, which is exactly the immortal-time error this
    # measure invites.
    if cfg.require_dm_before_als:
        in_dm_at_dx = has_dm & (dm_entry <= F.col("ALS_DX_DATE"))
        in_dm_washout_at_dx = (
            has_dm
            & (dm_lag_end <= F.col("ALS_DX_DATE"))
            & (F.col("ALS_DX_DATE") < dm_entry)
        )
    else:
        in_dm_at_dx = has_dm
        in_dm_washout_at_dx = F.lit(False)

    als_inc_dm = als_inc & in_dm_at_dx
    als_inc_dm_washout = als_inc & in_dm_washout_at_dx
    # Non-DM is the residual of the other two, so the three exhaust als_inc exactly.
    als_inc_non_dm = (
        als_inc
        & ~F.coalesce(in_dm_at_dx, F.lit(False))
        & ~F.coalesce(in_dm_washout_at_dx, F.lit(False))
    )

    # ---- person-time ----
    raw_ar_start = F.greatest(F.col("START_DATE"), F.col("YEAR_START"))
    ar_start = F.greatest(entry_eligible, F.col("YEAR_START"))
    ar_end = F.least(F.col("CENSOR_DATE"), F.col("YEAR_END"))
    if cfg.at_risk_person_time:
        # stop accruing at-risk time at diagnosis (prevalent-before-year -> 0 PY)
        ar_end = F.iff(
            F.col("ALS_DX_DATE").is_not_null(),
            F.least(ar_end, F.col("ALS_DX_DATE")),
            ar_end,
        )
    days = F.greatest(F.datediff("day", ar_start, ar_end), F.lit(0))

    # What the entry washout cost. Same ar_end, so the difference is purely the
    # left-hand gate. Not part of the DM partition -- it sits outside PY_AT_RISK.
    days_raw = F.greatest(F.datediff("day", raw_ar_start, ar_end), F.lit(0))
    days_excl = days_raw - days

    # DM time: left-truncated at dm_entry (= dx + lag + washout). GREATEST() is
    # null-propagating in Snowflake, so never-DM persons must be short-circuited
    # rather than left to the clamp.
    dm_start = F.greatest(ar_start, dm_entry)
    days_dm = F.iff(
        has_dm, F.greatest(F.datediff("day", dm_start, ar_end), F.lit(0)), F.lit(0)
    )

    # DM washout time: the middle slice [dm_lag_end, dm_entry], clamped to the at-risk
    # window. Charged to neither arm. Identically zero when dm_washout_days = 0.
    wo_start = F.greatest(ar_start, dm_lag_end)
    wo_end = F.least(ar_end, dm_entry)
    days_dm_washout = F.iff(
        has_dm, F.greatest(F.datediff("day", wo_start, wo_end), F.lit(0)), F.lit(0)
    )

    # non-DM time: right-truncated at dm_lag_end -- the lag window IS unexposed and
    # belongs here (that is what distinguishes a lag from a washout). All of the
    # at-risk window if never DM.
    non_dm_end = F.iff(has_dm, F.least(ar_end, dm_lag_end), ar_end)
    days_non_dm = F.greatest(F.datediff("day", ar_start, non_dm_end), F.lit(0))

    py = lambda d: _safe_divide(d, F.lit(cfg.days_per_year))
    b = lambda c: F.iff(c, F.lit(1), F.lit(0))

    return base.select(
        "YEAR",
        "PATID",
        "AGE_GROUP",
        "SEX",
        b(als_prev).alias("ALS_PREVALENT"),
        b(dm_prev).alias("DM_PREVALENT"),
        b(als_prev & dm_prev).alias("ALS_DM_PREVALENT"),
        b(als_inc).alias("ALS_INCIDENT"),
        b(als_inc_dm).alias("ALS_INCIDENT_IN_DM"),
        b(als_inc_dm_washout).alias("ALS_INCIDENT_IN_DM_WASHOUT"),
        b(als_inc_non_dm).alias("ALS_INCIDENT_IN_NON_DM"),
        b(als_inc_excl).alias("ALS_INCIDENT_EXCL_ENTRY_WASHOUT"),
        py(days).alias("PY_AT_RISK"),
        py(days_dm).alias("PY_AT_RISK_DM"),
        py(days_dm_washout).alias("PY_AT_RISK_DM_WASHOUT"),
        py(days_non_dm).alias("PY_AT_RISK_NON_DM"),
        py(days_excl).alias("PY_EXCL_ENTRY_WASHOUT"),
    )


def stratum_table(person_year: DataFrame, cfg: RateConfig) -> DataFrame:
    """Collapse to (YEAR, AGE_GROUP, SEX) counts and person-time for every measure."""
    aggs = [
        F.count_distinct("PATID").alias("N_OBSERVED"),
        F.sum("DM_PREVALENT").alias("DM_PREVALENT_CASES"),
        F.sum("ALS_PREVALENT").alias("ALS_PREVALENT_CASES"),
        F.sum("ALS_DM_PREVALENT").alias("ALS_DM_PREVALENT_CASES"),
        F.sum("ALS_INCIDENT").alias("ALS_INCIDENT_CASES"),
        F.sum("ALS_INCIDENT_IN_DM").alias("ALS_INCIDENT_IN_DM_CASES"),
        F.sum("ALS_INCIDENT_IN_DM_WASHOUT").alias("ALS_INCIDENT_IN_DM_WASHOUT_CASES"),
        F.sum("ALS_INCIDENT_IN_NON_DM").alias("ALS_INCIDENT_IN_NON_DM_CASES"),
        F.sum("ALS_INCIDENT_EXCL_ENTRY_WASHOUT").alias("ALS_CASES_EXCL_ENTRY_WASHOUT"),
        F.sum("PY_AT_RISK").alias("PERSON_YEARS"),
        F.sum("PY_AT_RISK_DM").alias("PERSON_YEARS_DM"),
        F.sum("PY_AT_RISK_DM_WASHOUT").alias("PERSON_YEARS_DM_WASHOUT"),
        F.sum("PY_AT_RISK_NON_DM").alias("PERSON_YEARS_NON_DM"),
        F.sum("PY_EXCL_ENTRY_WASHOUT").alias("PERSON_YEARS_EXCL_ENTRY_WASHOUT"),
    ]
    return person_year.group_by("YEAR", "AGE_GROUP", "SEX").agg(*aggs)


def qa_person_time(strata: DataFrame, cfg: RateConfig, tol: float = 1e-6) -> DataFrame:
    """
    Assert the partition invariants, and report what the entry washout removed.

    Every at-risk day falls in exactly one of three consecutive slices of
    [ar_start, ar_end] -- before DM onset + lag, inside the DM washout, or in DM --
    and every incident ALS case falls in exactly one of the matching three. So, per
    year:

        PERSON_YEARS_DM + PERSON_YEARS_DM_WASHOUT + PERSON_YEARS_NON_DM
            == PERSON_YEARS
        ALS_INCIDENT_IN_DM + ALS_INCIDENT_IN_DM_WASHOUT + ALS_INCIDENT_IN_NON_DM
            == ALS_INCIDENT_CASES

    These are EXACT under every parameter setting, including any lag and any washout.
    (Earlier versions folded the washout into the lag and tolerated a gap under a
    non-zero lag; giving the washout window its own column removed the need for that
    exception, so a break here is now unambiguously a bug.) A break most likely means
    a null-handling slip on never-DM persons -- GREATEST/LEAST propagate nulls in
    Snowflake -- which would quietly move unexposed person-time into the DM
    denominator.

    Entry-washout attrition (NOT part of the partition -- this time sits outside
    PERSON_YEARS by construction):
        PY_EXCL_ENTRY_WASHOUT    at-risk time deleted by the clean-lookback rule
        PCT_PY_EXCL              its share of what PERSON_YEARS would otherwise be
        CASES_EXCL_ENTRY_WASHOUT dx-in-year cases that could not be shown to be new

    Read PCT_PY_EXCL down the years. Expect it high in the lead-in year(s) and then
    settling; if it stays high, entry into your denominator is still churning and the
    incidence series is measuring enrollment as much as disease.
    """
    agg = strata.group_by("YEAR").agg(
        F.sum("PERSON_YEARS").alias("PY"),
        F.sum("PERSON_YEARS_DM").alias("PY_DM"),
        F.sum("PERSON_YEARS_DM_WASHOUT").alias("PY_DM_WASHOUT"),
        F.sum("PERSON_YEARS_NON_DM").alias("PY_NON_DM"),
        F.sum("PERSON_YEARS_EXCL_ENTRY_WASHOUT").alias("PY_EXCL"),
        F.sum("ALS_INCIDENT_CASES").alias("INC"),
        F.sum("ALS_INCIDENT_IN_DM_CASES").alias("INC_DM"),
        F.sum("ALS_INCIDENT_IN_DM_WASHOUT_CASES").alias("INC_DM_WASHOUT"),
        F.sum("ALS_INCIDENT_IN_NON_DM_CASES").alias("INC_NON_DM"),
        F.sum("ALS_CASES_EXCL_ENTRY_WASHOUT").alias("CASES_EXCL_ENTRY_WASHOUT"),
    )
    py_gap = F.col("PY") - (F.col("PY_DM") + F.col("PY_DM_WASHOUT") + F.col("PY_NON_DM"))
    inc_gap = F.col("INC") - (
        F.col("INC_DM") + F.col("INC_DM_WASHOUT") + F.col("INC_NON_DM")
    )
    pct_excl = F.col("PY_EXCL") * 100.0 / F.greatest(
        F.col("PY") + F.col("PY_EXCL"), F.lit(1.0)
    )
    return agg.select(
        "YEAR",
        "PY", "PY_DM", "PY_DM_WASHOUT", "PY_NON_DM",
        "INC", "INC_DM", "INC_DM_WASHOUT", "INC_NON_DM",
        py_gap.alias("PY_GAP"),
        inc_gap.alias("INC_GAP"),
        (F.abs(py_gap) <= F.lit(tol) * F.greatest(F.col("PY"), F.lit(1.0))).alias("PY_OK"),
        (F.abs(inc_gap) == 0).alias("INC_OK"),
        F.round(F.col("PY_EXCL"), 1).alias("PY_EXCL_ENTRY_WASHOUT"),
        F.round(pct_excl, 2).alias("PCT_PY_EXCL"),
        F.col("CASES_EXCL_ENTRY_WASHOUT"),
    ).sort("YEAR")


# --------------------------------------------------------------------------- #
# Confidence interval machinery
#
# ALS is rare (~5-10 per 100,000), so the Wald/normal interval is unusable here: it
# is symmetric on a scale where the sampling distribution is strongly skewed, and
# routinely returns negative lower bounds that then have to be clamped at zero --
# which silently destroys coverage rather than fixing it. Everything below is
# closed-form and evaluates in SQL; no UDF or scipy round-trip needed.
# --------------------------------------------------------------------------- #
def _wilson_bounds(x: Column, n: Column, z: Column) -> tuple[Column, Column]:
    """
    Wilson score interval for a binomial proportion x/n.

    Correct behaviour in the rare-event regime: bounds stay inside [0, 1], the
    interval is asymmetric, and x = 0 gives a sensible non-degenerate upper bound.
    """
    p = _safe_divide(x, n)
    z2 = z * z
    denom = F.lit(1) + _safe_divide(z2, n)
    center = _safe_divide(p + _safe_divide(z2, F.lit(2) * n), denom)
    half = _safe_divide(z, denom) * F.sqrt(
        _safe_divide(p * (F.lit(1) - p), n)
        + _safe_divide(z2, F.lit(4) * n * n)
    )
    return (
        F.iff(n > 0, F.greatest(center - half, F.lit(0.0)), F.lit(None)),
        F.iff(n > 0, F.least(center + half, F.lit(1.0)), F.lit(None)),
    )


def _byar_count_bounds(y: Column, z: Column) -> tuple[Column, Column]:
    """
    Byar's approximation to the exact Poisson interval for an observed count y.

    Accurate to ~1% against the exact gamma/chi-square interval for y >= 1, and the
    (y + 1) term makes the upper bound well behaved at y = 0 (-> ~3.67 vs 3.69 exact).
    """
    y1 = y + F.lit(1)
    lo = y * (
        F.lit(1) - _safe_divide(F.lit(1), F.lit(9) * y)
        - _safe_divide(z, F.lit(3) * F.sqrt(y))
    ) ** 3
    lo = F.iff(y <= 0, F.lit(0.0), F.greatest(lo, F.lit(0.0)))
    up = y1 * (
        F.lit(1) - _safe_divide(F.lit(1), F.lit(9) * y1)
        + _safe_divide(z, F.lit(3) * F.sqrt(y1))
    ) ** 3
    return lo, up


def _dobson_bounds(
    rate: Column, var_rate: Column, y: Column, z: Column
) -> tuple[Column, Column]:
    """
    Dobson et al. (1991) interval for a directly standardized rate.

    Maps an exact-ish Poisson interval on the total observed count onto the
    standardized rate scale:

        lo = rate + sqrt(var_rate / y) * (y_lo - y)
        hi = rate + sqrt(var_rate / y) * (y_hi - y)

    This inherits the skewness of the count interval, which is the whole point --
    a normal interval on the standardized rate is symmetric and undercovers badly
    when the case count is small. Preferred over Fay-Feuer here only because it is
    closed-form; Fay-Feuer needs an inverse gamma CDF (scipy UDF) and is marginally
    better when one stratum dominates the weights.

    y = 0 yields (0, NULL): the ratio sqrt(var/y) is undefined and there is no
    closed-form upper bound without the gamma quantile.
    """
    y_lo, y_hi = _byar_count_bounds(y, z)
    scale = F.sqrt(_safe_divide(var_rate, y))
    lo = (
        F.when(rate.is_null(), F.lit(None).cast("double"))
        .when(y <= 0, F.lit(0.0))
        .otherwise(F.greatest(rate + scale * (y_lo - y), F.lit(0.0)))
    )
    hi = F.iff(y <= 0, F.lit(None).cast("double"), rate + scale * (y_hi - y))
    return lo, hi


# --------------------------------------------------------------------------- #
# Crude rates
# --------------------------------------------------------------------------- #
def crude_rates(strata: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Crude annual rates for every measure (marginalize strata within year).

    Intervals: Wilson score for PREVALENCE measures (binomial proportion), Byar-based
    exact Poisson for INCIDENCE measures (count / person-time). cfg.ci_method="NORMAL"
    falls back to Wald -- not recommended for the ALS measures.
    """
    z = F.lit(cfg.z)
    ms = measures(cfg)

    needed = sorted({c for m in ms for c in (m.numerator, m.denominator)})
    agg = strata.group_by("YEAR").agg(*[F.sum(c).alias(c) for c in needed])

    out_cols = [F.col("YEAR")] + [F.col(c) for c in needed]
    for m in ms:
        num, den = F.col(m.numerator), F.col(m.denominator)
        mult = F.lit(m.multiplier(cfg))
        rate = _safe_divide(num, den)
        out_cols.append((rate * mult).alias(f"CRUDE_{m.name}"))

        if not cfg.add_ci:
            continue

        if cfg.ci_method.upper() == "NORMAL":
            if m.kind == "PREVALENCE":
                se = F.sqrt(_safe_divide(rate * (F.lit(1) - rate), den))
            else:
                se = F.sqrt(_safe_divide(num, den * den))
            lo, hi = F.greatest(rate - z * se, F.lit(0.0)), rate + z * se
        elif m.kind == "PREVALENCE":
            lo, hi = _wilson_bounds(num, den, z)
        else:
            c_lo, c_hi = _byar_count_bounds(num, z)
            lo, hi = _safe_divide(c_lo, den), _safe_divide(c_hi, den)

        out_cols.append(F.iff(den > 0, lo * mult, F.lit(None)).alias(f"CRUDE_{m.name}_LCL"))
        out_cols.append(F.iff(den > 0, hi * mult, F.lit(None)).alias(f"CRUDE_{m.name}_UCL"))

    return agg.select(*out_cols).sort("YEAR")


# --------------------------------------------------------------------------- #
# Age-sex adjusted rates (direct standardization)
# --------------------------------------------------------------------------- #
def adjusted_rates(strata: DataFrame, std_pop: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Directly standardized annual rates for every measure.

    adj_rate = sum_s (w_s * r_s) / sum_s w_s, over strata where the measure's
    denominator is non-zero. Note each measure sums weights over its OWN eligible
    strata: a stratum with no DM patients contributes no weight to the DM-denominated
    measures while still contributing to the cohort-denominated ones. The same applies
    to DM_PREVALENCE_IN_ALS_PCT, whose weights fall only on strata containing ALS
    cases -- a thin set at ALS rates, so read its adjusted CI accordingly.

    All measures share one standard, which is what makes them mutually comparable --
    ALS_INCIDENCE and ALS_INCIDENCE_IN_DM then answer "what would each rate be if both
    populations had the standard's age-sex structure", rather than differing partly
    because diabetics are older.

    `std_pop` may be keyed (AGE_GROUP, SEX) -- fixed weights, series comparable across
    years -- or (YEAR, AGE_GROUP, SEX) -- weights vary by year, series NOT comparable
    across years. The key set is detected automatically.

    The join is INNER, so under an age restriction a standard covering ages outside
    the restriction contributes no weight there: the standard is effectively
    renormalized to the restricted range.

    Intervals: Dobson et al. (1991); cfg.ci_method="NORMAL" for the Wald alternative.
    """
    z = F.lit(cfg.z)
    ms = measures(cfg)

    year_specific = "YEAR" in [c.upper().strip('"') for c in std_pop.columns]
    keys = ["YEAR", "AGE_GROUP", "SEX"] if year_specific else ["AGE_GROUP", "SEX"]
    s = strata.join(std_pop, on=keys, how="inner")
    w = F.col("STD_POP")

    # Per-stratum weighted terms for each measure.
    terms = [F.col("YEAR")]
    for m in ms:
        num, den = F.col(m.numerator), F.col(m.denominator)
        r = _safe_divide(num, den)
        eligible = den > 0
        if m.kind == "PREVALENCE":
            var_s = _safe_divide(r * (F.lit(1) - r), den)  # binomial
        else:
            var_s = _safe_divide(num, den * den)         # Poisson rate
        terms += [
            F.iff(eligible, w, F.lit(0.0)).alias(f"W_{m.name}"),
            F.iff(eligible, w * r, F.lit(0.0)).alias(f"WR_{m.name}"),
            F.iff(eligible, w * w * var_s, F.lit(0.0)).alias(f"WVAR_{m.name}"),
            # Dobson scales by the observed count over WEIGHTED strata only.
            F.iff(eligible & (w > 0), num, F.lit(0)).alias(f"Y_{m.name}"),
            F.iff(eligible & (w > 0), F.lit(1), F.lit(0)).alias(f"U_{m.name}"),
        ]
    s = s.select(*terms)

    aggs = []
    for m in ms:
        aggs += [
            F.sum(f"W_{m.name}").alias(f"W_{m.name}"),
            F.sum(f"WR_{m.name}").alias(f"WR_{m.name}"),
            F.sum(f"WVAR_{m.name}").alias(f"WVAR_{m.name}"),
            F.sum(f"Y_{m.name}").alias(f"Y_{m.name}"),
            F.sum(f"U_{m.name}").alias(f"N_STRATA_{m.name}"),
        ]
    agg = s.group_by("YEAR").agg(*aggs)

    out_cols = [F.col("YEAR")]
    for m in ms:
        W, WR = F.col(f"W_{m.name}"), F.col(f"WR_{m.name}")
        adj = _safe_divide(WR, W)
        var = _safe_divide(F.col(f"WVAR_{m.name}"), W * W)
        mult = F.lit(m.multiplier(cfg))

        out_cols.append((adj * mult).alias(f"ADJ_{m.name}"))
        out_cols.append(F.col(f"N_STRATA_{m.name}"))

        if not cfg.add_ci:
            continue
        if cfg.ci_method.upper() == "NORMAL":
            se = F.sqrt(var)
            lo, hi = F.greatest(adj - z * se, F.lit(0.0)), adj + z * se
        else:
            lo, hi = _dobson_bounds(adj, var, F.col(f"Y_{m.name}"), z)
        out_cols.append(F.iff(W > 0, lo * mult, F.lit(None)).alias(f"ADJ_{m.name}_LCL"))
        out_cols.append(F.iff(W > 0, hi * mult, F.lit(None)).alias(f"ADJ_{m.name}_UCL"))

    return agg.select(*out_cols).sort("YEAR")


# --------------------------------------------------------------------------- #
# Final reporting table
# --------------------------------------------------------------------------- #
def _pooled_rate_inputs(
    strata: DataFrame, std_pop: DataFrame, ms: Sequence[Measure],
) -> tuple[DataFrame, DataFrame]:
    """Pool annual measure inputs and standards without averaging estimates."""
    needed = sorted({c for m in ms for c in (m.numerator, m.denominator)})
    annual = strata.filter(F.col("YEAR") != 9999)
    pooled = annual.group_by("AGE_GROUP", "SEX").agg(
        *[F.sum(c).alias(c) for c in needed]
    ).with_column("YEAR", F.lit(9999))
    if "YEAR" in [c.upper().strip('"') for c in std_pop.columns]:
        years = annual.select("YEAR").distinct()
        std_pop = (
            std_pop.join(years, on="YEAR", how="inner")
            .group_by("AGE_GROUP", "SEX")
            .agg(F.sum("STD_POP").alias("STD_POP"))
        )
    return pooled, std_pop


def _canonical_final_table(
    crude: DataFrame,
    adjusted: DataFrame,
    ms: Sequence[Measure],
    cfg: RateConfig,
) -> DataFrame:
    """Convert wide crude/adjusted estimates to the canonical reporting schema."""
    wide = crude.join(adjusted, on="YEAR", how="left")
    have = {name.upper().strip('"') for name in wide.columns}
    rows = None

    for measure in ms:
        for estimate_type, prefix in (("CRUDE", "CRUDE_"), ("ADJUSTED", "ADJ_")):
            estimate_col = f"{prefix}{measure.name}"
            lcl_col = f"{estimate_col}_LCL"
            ucl_col = f"{estimate_col}_UCL"
            one = wide.select(
                F.col("YEAR").alias("YEAR"),
                F.lit(measure.outcome).alias("OUTCOME"),
                F.lit(measure.population).alias("POPULATION"),
                F.lit(measure.kind).alias("METRIC"),
                F.lit(estimate_type).alias("ESTIMATE_TYPE"),
                F.col(measure.numerator).alias("NUM"),
                F.col(measure.denominator).alias("DEN"),
                F.round(F.col(estimate_col), cfg.round_rates).alias("ESTIMATE"),
                (
                    F.round(F.col(lcl_col), cfg.round_rates)
                    if lcl_col in have
                    else F.lit(None).cast("double")
                ).alias("LCL"),
                (
                    F.round(F.col(ucl_col), cfg.round_rates)
                    if ucl_col in have
                    else F.lit(None).cast("double")
                ).alias("UCL"),
                F.lit(measure.unit).alias("UNIT"),
            )
            rows = one if rows is None else rows.union_all(one)

    if rows is None:
        raise ValueError("at least one measure is required")

    if cfg.suppress_threshold is not None:
        disclose = F.col("NUM") >= F.lit(cfg.suppress_threshold)
        for name in ("NUM", "ESTIMATE", "LCL", "UCL"):
            rows = rows.with_column(name, F.iff(disclose, F.col(name), F.lit(None)))

    return rows.select(
        "YEAR", "OUTCOME", "POPULATION", "METRIC", "ESTIMATE_TYPE",
        "NUM", "DEN", "ESTIMATE", "LCL", "UCL", "UNIT",
    ).sort("YEAR", "OUTCOME", "POPULATION", "METRIC", "ESTIMATE_TYPE")


def final_table(strata: DataFrame, std_pop: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Canonical long table with crude and age-sex-adjusted rows for every measure.

    Columns: YEAR, OUTCOME, POPULATION, METRIC, ESTIMATE_TYPE, NUM, DEN,
    ESTIMATE, LCL, UCL, UNIT. Adjusted rows retain observed NUM and DEN as context;
    their estimates are directly standardized and are not computed as NUM / DEN.
    YEAR=9999 contains rates and CIs recomputed from pooled annual inputs.
    """
    ms = measures(cfg)
    c = crude_rates(strata, cfg)
    a = adjusted_rates(strata, std_pop, cfg)
    pooled, pooled_std = _pooled_rate_inputs(strata, std_pop, ms)
    c = c.union_all(crude_rates(pooled, cfg))
    a = a.union_all(adjusted_rates(pooled, pooled_std, cfg))
    return _canonical_final_table(c, a, ms, cfg)


# --------------------------------------------------------------------------- #
# Medication prevalence (separate snapshot pipeline; no carry-over)
# --------------------------------------------------------------------------- #
def medication_measures(cfg: RateConfig) -> list[Measure]:
    """Medication prevalence measures (snapshot method) by dm_type-selected DENOM."""
    denoms = _med_denom_values(cfg)
    seen = set()
    ms: list[Measure] = []
    for phe in cfg.med_phenotypes:
        pkey = _med_key(phe)
        if pkey in seen:
            raise ValueError(f"duplicate medication phenotype key after normalization: {pkey}")
        seen.add(pkey)
        for d in denoms:
            dkey = _med_key(d)
            ms.append(
                Measure(
                    name=f"MED_{pkey}_PREVALENCE_IN_{dkey}_PCT",
                    numerator=f"MED_{pkey}_IN_{dkey}_CASES",
                    denominator=f"DENOM_{dkey}_CASES",
                    kind="PREVALENCE",
                    outcome=phe.upper(),
                    population=d,
                    multiplier_attr="med_prevalence_multiplier",
                    unit="percent",
                    description=(
                        f"Medication prescription prevalence for phenotype {phe.upper()} "
                        f"out of DENOM={d} (snapshot; no carry-over)"
                    ),
                )
            )
    return ms


def medication_snapshot_strata(denom: DataFrame, med: DataFrame, cfg: RateConfig) -> DataFrame:
    """
    Build (YEAR, AGE_GROUP, SEX) strata for snapshot medication prevalence.

        Denominator is explicit same-year set membership from the medication input,
        where selected DENOM is derived from dm_type.
    Numerator for each phenotype adds phenotype-present-in-year == 1.
    """
    denoms = _med_denom_values(cfg)
    med_cols_u = [c.upper().strip('"') for c in med.columns]
    for req in (cfg.med_person_col, cfg.med_year_col, cfg.med_denom_col, cfg.med_type_col):
        if req.upper() not in med_cols_u:
            raise ValueError(f"medication snapshot prevalence requires med column {req!r}")

    session = denom._session
    years = _year_dim(session, cfg).select("YEAR", "YEAR_START", "YEAR_END", "MID_YEAR")
    d = _norm_denom(denom, cfg)

    denom_year = (
        d.cross_join(years)
        .filter(
            (F.col("START_DATE") <= F.col("YEAR_END"))
            & (F.col("CENSOR_DATE") >= F.col("YEAR_START"))
        )
    )

    age = F.floor(
        _safe_divide(
            F.datediff("day", F.col("BIRTH_DATE"), F.col("MID_YEAR")),
            F.lit(cfg.days_per_year),
        )
    ).cast("int")
    denom_year = denom_year.with_column("AGE", age).with_column(
        "AGE_GROUP", _age_group_expr(F.col("AGE"), cfg.age_cutpoints)
    )

    if cfg.min_age is not None and cfg.max_age is not None and cfg.min_age > cfg.max_age:
        raise ValueError(f"min_age ({cfg.min_age}) > max_age ({cfg.max_age})")
    if cfg.min_age is not None and cfg.min_age not in tuple(cfg.age_cutpoints):
        raise ValueError(
            f"min_age={cfg.min_age} is not in age_cutpoints; the boundary band would "
            "be partial. Set age_cutpoints to start at min_age."
        )
    if cfg.min_age is not None:
        denom_year = denom_year.filter(F.col("AGE") >= F.lit(cfg.min_age))
    if cfg.max_age is not None:
        denom_year = denom_year.filter(F.col("AGE") <= F.lit(cfg.max_age))

    m = (
        med.with_column("MED_PHE_TYPE", F.upper(F.col(cfg.med_type_col)))
        .with_column("MED_DENOM", F.upper(F.col(cfg.med_denom_col)))
        .with_column("MED_YEAR", F.col(cfg.med_year_col).cast("int"))
    )

    grp = [F.col(cfg.med_person_col).alias("PATID"), F.col("MED_YEAR").alias("YEAR")]
    aggs = []
    for dval in denoms:
        dkey = _med_key(dval)
        aggs.append(
            F.max(F.iff(F.col("MED_DENOM") == F.lit(dval), F.lit(1), F.lit(0))).alias(
                f"DENOM_{dkey}_IN_YEAR"
            )
        )
    for phe in cfg.med_phenotypes:
        pkey = _med_key(phe)
        for dval in denoms:
            dkey = _med_key(dval)
            aggs.append(
                F.max(
                    F.iff(
                        (F.col("MED_DENOM") == F.lit(dval))
                        & (F.col("MED_PHE_TYPE") == F.lit(phe.upper())),
                        F.lit(1),
                        F.lit(0),
                    )
                ).alias(f"MED_{pkey}_IN_{dkey}_PRESENT")
            )

    med_py = m.group_by(*grp).agg(*aggs)
    joined = denom_year.join(med_py, on=["PATID", "YEAR"], how="inner")

    strata_aggs = [F.count_distinct("PATID").alias("N_OBSERVED")]
    for dval in denoms:
        dkey = _med_key(dval)
        in_d = F.col(f"DENOM_{dkey}_IN_YEAR") == F.lit(1)
        strata_aggs.append(
            F.sum(F.iff(in_d, F.lit(1), F.lit(0))).alias(f"DENOM_{dkey}_CASES")
        )
        for phe in cfg.med_phenotypes:
            pkey = _med_key(phe)
            strata_aggs.append(
                F.sum(
                    F.iff(
                        in_d & (F.col(f"MED_{pkey}_IN_{dkey}_PRESENT") == F.lit(1)),
                        F.lit(1),
                        F.lit(0),
                    )
                ).alias(f"MED_{pkey}_IN_{dkey}_CASES")
            )

    return joined.group_by("YEAR", "AGE_GROUP", "SEX").agg(*strata_aggs)


def medication_standard_population_from_strata(strata: DataFrame, cfg: RateConfig) -> DataFrame:
    """Standard population for medication snapshot prevalence."""
    pop = strata.select(
        "YEAR",
        "AGE_GROUP",
        "SEX",
        F.col("N_OBSERVED").cast("float").alias("STD_POP"),
    )
    return _collapse_standard(pop, cfg)


def medication_crude_rates(strata: DataFrame, cfg: RateConfig) -> DataFrame:
    """Crude annual medication snapshot prevalence rates (+ CIs)."""
    z = F.lit(cfg.z)
    ms = medication_measures(cfg)
    needed = sorted({c for m in ms for c in (m.numerator, m.denominator)})
    agg = strata.group_by("YEAR").agg(*[F.sum(c).alias(c) for c in needed])

    out_cols = [F.col("YEAR")] + [F.col(c) for c in needed]
    for m in ms:
        num, den = F.col(m.numerator), F.col(m.denominator)
        mult = F.lit(m.multiplier(cfg))
        rate = _safe_divide(num, den)
        out_cols.append((rate * mult).alias(f"CRUDE_{m.name}"))

        if not cfg.add_ci:
            continue
        if cfg.ci_method.upper() == "NORMAL":
            se = F.sqrt(_safe_divide(rate * (F.lit(1) - rate), den))
            lo, hi = F.greatest(rate - z * se, F.lit(0.0)), rate + z * se
        else:
            lo, hi = _wilson_bounds(num, den, z)
        out_cols.append(F.iff(den > 0, lo * mult, F.lit(None)).alias(f"CRUDE_{m.name}_LCL"))
        out_cols.append(F.iff(den > 0, hi * mult, F.lit(None)).alias(f"CRUDE_{m.name}_UCL"))

    return agg.select(*out_cols).sort("YEAR")


def medication_adjusted_rates(strata: DataFrame, std_pop: DataFrame, cfg: RateConfig) -> DataFrame:
    """Age-sex adjusted annual medication snapshot prevalence rates (+ CIs)."""
    z = F.lit(cfg.z)
    ms = medication_measures(cfg)

    year_specific = "YEAR" in [c.upper().strip('"') for c in std_pop.columns]
    keys = ["YEAR", "AGE_GROUP", "SEX"] if year_specific else ["AGE_GROUP", "SEX"]
    s = strata.join(std_pop, on=keys, how="inner")
    w = F.col("STD_POP")

    terms = [F.col("YEAR")]
    for m in ms:
        num, den = F.col(m.numerator), F.col(m.denominator)
        r = _safe_divide(num, den)
        eligible = den > 0
        var_s = _safe_divide(r * (F.lit(1) - r), den)
        terms += [
            F.iff(eligible, w, F.lit(0.0)).alias(f"W_{m.name}"),
            F.iff(eligible, w * r, F.lit(0.0)).alias(f"WR_{m.name}"),
            F.iff(eligible, w * w * var_s, F.lit(0.0)).alias(f"WVAR_{m.name}"),
            F.iff(eligible & (w > 0), num, F.lit(0)).alias(f"Y_{m.name}"),
            F.iff(eligible & (w > 0), F.lit(1), F.lit(0)).alias(f"U_{m.name}"),
        ]
    s = s.select(*terms)

    aggs = []
    for m in ms:
        aggs += [
            F.sum(f"W_{m.name}").alias(f"W_{m.name}"),
            F.sum(f"WR_{m.name}").alias(f"WR_{m.name}"),
            F.sum(f"WVAR_{m.name}").alias(f"WVAR_{m.name}"),
            F.sum(f"Y_{m.name}").alias(f"Y_{m.name}"),
            F.sum(f"U_{m.name}").alias(f"N_STRATA_{m.name}"),
        ]
    agg = s.group_by("YEAR").agg(*aggs)

    out_cols = [F.col("YEAR")]
    for m in ms:
        W, WR = F.col(f"W_{m.name}"), F.col(f"WR_{m.name}")
        adj = _safe_divide(WR, W)
        var = _safe_divide(F.col(f"WVAR_{m.name}"), W * W)
        mult = F.lit(m.multiplier(cfg))

        out_cols.append((adj * mult).alias(f"ADJ_{m.name}"))
        out_cols.append(F.col(f"N_STRATA_{m.name}"))

        if not cfg.add_ci:
            continue
        if cfg.ci_method.upper() == "NORMAL":
            se = F.sqrt(var)
            lo, hi = F.greatest(adj - z * se, F.lit(0.0)), adj + z * se
        else:
            lo, hi = _dobson_bounds(adj, var, F.col(f"Y_{m.name}"), z)
        out_cols.append(F.iff(W > 0, lo * mult, F.lit(None)).alias(f"ADJ_{m.name}_LCL"))
        out_cols.append(F.iff(W > 0, hi * mult, F.lit(None)).alias(f"ADJ_{m.name}_UCL"))

    return agg.select(*out_cols).sort("YEAR")


def medication_final_table(strata: DataFrame, std_pop: DataFrame, cfg: RateConfig) -> DataFrame:
    """Annual medication snapshot prevalence plus pooled YEAR=9999 summaries."""
    ms = medication_measures(cfg)
    c = medication_crude_rates(strata, cfg)
    a = medication_adjusted_rates(strata, std_pop, cfg)
    pooled, pooled_std = _pooled_rate_inputs(strata, std_pop, ms)
    c = c.union_all(medication_crude_rates(pooled, cfg))
    a = a.union_all(medication_adjusted_rates(pooled, pooled_std, cfg))
    return _canonical_final_table(c, a, ms, cfg)


def compute_medication_prevalence_snapshot(
    denom: DataFrame,
    med: DataFrame,
    cfg: RateConfig,
    std_pop: DataFrame | None = None,
) -> dict[str, DataFrame]:
    """Compute medication prevalence via the snapshot method into a separate output table."""
    strata = medication_snapshot_strata(denom, med, cfg).cache_result()
    if std_pop is None:
        std_pop = medication_standard_population_from_strata(strata, cfg)
    std_pop = std_pop.cache_result()
    return {
        "final": medication_final_table(strata, std_pop, cfg),
        "stratum": strata,
        "std_pop": std_pop,
        "crude": medication_crude_rates(strata, cfg),
        "adjusted": medication_adjusted_rates(strata, std_pop, cfg),
    }


# --------------------------------------------------------------------------- #
# Orchestration
# --------------------------------------------------------------------------- #
def compute_rates(
    denom: DataFrame,
    num: DataFrame,
    cfg: RateConfig,
    std_pop: DataFrame | None = None,
    dm: DataFrame | None = None,
) -> dict[str, DataFrame]:
    """
    Returns lazy DataFrames:
        'final'    -> canonical long reporting table with one row per
                      year x outcome x population x metric x estimate type
        'stratum'  -> per (year, age group, sex) counts and person-years
        'std_pop'  -> the standardization weights actually used
        'crude'    -> internal wide crude rates (+ CIs)
        'adjusted' -> internal wide age-sex standardized rates (+ CIs)
        'qa'       -> partition invariants (DM / DM-washout / non-DM) plus
                      entry-washout attrition per year

    `dm` is the diabetes cohort (person, dx date, censor date). Omit it and the
    DM-denominated measures return null; the ALS measures are unaffected.

    Medication prevalence is intentionally excluded from this carry-forward pipeline.
    Use compute_medication_prevalence_snapshot(...) for a separate snapshot table.

    `std_pop` defaults to the cohort's own per-year structure collapsed per
    cfg.std_mode. Pass standard_population_from_table(...) for an external population,
    or standard_population_2000(...) for literature-comparable rates.

    cfg.min_age / cfg.max_age and both washouts are applied once, in
    build_person_year, so everything here -- including a cohort-derived standard --
    is restricted consistently.
    """
    pxy = build_person_year(denom, num, cfg, dm=dm)
    strata = stratum_table(pxy, cfg).cache_result()

    if std_pop is None:
        std_pop = standard_population_from_cohort(strata, cfg)
    std_pop = std_pop.cache_result()

    return {
        "final": final_table(strata, std_pop, cfg),
        "stratum": strata,
        "std_pop": std_pop,
        "crude": crude_rates(strata, cfg),
        "adjusted": adjusted_rates(strata, std_pop, cfg),
        "qa": qa_person_time(strata, cfg),
    }


# --------------------------------------------------------------------------- #
# Example usage
# --------------------------------------------------------------------------- #
if __name__ == "__main__":
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")

    cfg = RateConfig(
        # start_year is the first year BUILT, not the first year REPORTABLE. With a
        # 365-day entry washout, 2011 incidence is lead-in: most of its at-risk time
        # falls inside the washout and its cases cannot be shown to be new. 2011
        # prevalence is fine, so the row will look populated -- check PCT_PY_EXCL in
        # the QA frame and drop the lead-in years from the incidence series.
        start_year=2005,
        end_year=2025,
        std_mode="YEAR_SPECIFIC",

        # Incident-case ascertainment: 1 year of clean lookback after cohort entry.
        entry_washout_days=365,
        # DM anchor: no ascertainment washout by default. Distinct from dm_lag_days --
        # different windows, charged differently (see RateConfig).
        dm_washout_days=0,

        # Adults only. age_cutpoints is realigned to start AT the restriction so the
        # boundary band is not partial; the 18-19 band is then a true 2-year band.
        min_age=18,
        age_cutpoints=(18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),

        denom_table="PAT_TABLE1",
        # denom_table="GLP1RA_CASE_TABLE1",
        denom_person_col="PATID",
        denom_start_col="INDEX_DATE",
        denom_censor_col="CENSOR_DATE",
        denom_birth_col="BIRTH_DATE",
        denom_sex_col="SEX",

        num_person_col="PATID",
        num_dx_col="INDEX_DATE",
        num_censor_col="CENSOR_DATE",
        
        dm_person_col="PATID",
        dm_dx_col="DM1DX_DATE",
        dm_censor_col="CENSOR_DATE",

        med_person_col="PATID",
        med_year_col="YEAR",
        med_denom_col="DENOM",
        med_type_col="PHE_TYPE",

        # Diabetes type: "T1DM", "T2DM", default is "DM"
        # dm_type="T1DM",
        dm_type="T2DM",
    )

    denom = session.table(cfg.denom_table)
    num = session.table("ALS_CASE_TABLE1")
    dm = session.table("DM_CASE_TABLE1")
    med = session.table("DM_RX_LONG")

    print(measure_dictionary(cfg))

    # Confirm the ALS/DM censor dates really do track the enrollment censor.
    censor_discordance(denom, num, cfg).show()

    # How much of the ALS risk set the entry washout removes, and why. A large
    # DX_IN_ENTRY_WASHOUT share means the ungated rates were substantially prevalence
    # contamination.
    washout_attrition(denom, num, cfg).show()

    results = compute_rates(denom, num, cfg, dm=dm)
    med_results = compute_medication_prevalence_snapshot(denom, med, cfg)
    cohort_comp = annual_prevalence_cohort_profile(denom, num, dm, cfg)

    # Partition invariants (exact under every setting):
    #   PY  == PY_DM + PY_DM_WASHOUT + PY_NON_DM
    #   INC == INC_DM + INC_DM_WASHOUT + INC_NON_DM
    # plus PCT_PY_EXCL -- the share of at-risk time the entry washout deleted, by
    # year. Expect it high in the lead-in years and settling after; if it stays high,
    # the incidence series is tracking enrollment churn as much as disease.
    results["qa"].show()

    results["final"].show(20)
    results["final"].write.save_as_table(
        f"ALS_DM_ANNUAL_RATES_{cfg.dm_type}_{cfg.denom_table}", mode="overwrite"
    )
    med_results["final"].show(20)
    med_results["final"].write.save_as_table(
        f"ALS_DM_MED_SNAPSHOT_PREVALENCE_{cfg.dm_type}_{cfg.denom_table}", mode="overwrite"
    )

    cohort_comp.show(50)
    cohort_comp.write.save_as_table(
        f"ALS_DM_ANNUAL_PREV_COHORT_PROFILE_{cfg.dm_type}_{cfg.denom_table}", mode="overwrite"
    )

    # ---- sensitivity analyses ----
    # Washout length. The rate should fall as the washout lengthens, then plateau; the
    # plateau is where prevalence contamination is exhausted. If it never plateaus,
    # incident ALS is not identifiable in this data at any lookback -- which is itself
    # the finding, and is better learned here than after the trend is written up.
    # for w in (0, 183, 365, 730):
    #     compute_rates(denom, num, replace(cfg, entry_washout_days=w), dm=dm)["final"]

    # Lag DM exposure by 2 years to blunt reverse causation (ALS prodrome surfacing a
    # DM diagnosis). The lag window counts as UNEXPOSED -> non-DM person-time.
    # lagged = compute_rates(denom, num, replace(cfg, dm_lag_days=730), dm=dm)["final"]

    # Require 1 year of observed DM before an ALS dx counts as incident-within-DM.
    # The washout window is charged to NEITHER arm -- distinct from the lag above.
    # dm_wo = compute_rates(denom, num, replace(cfg, dm_washout_days=365), dm=dm)["final"]

    # Cell-suppressed copy for external release under a CMS DUA:
    # released = compute_rates(denom, num, replace(cfg, suppress_threshold=11),
    #                          dm=dm)["final"]