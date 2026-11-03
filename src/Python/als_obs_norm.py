import os
import json
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
)
from snowflake.snowpark.window import Window
from config import env, conn


def build_bmi_table(session: Session, ref_cohort: str, tgt_long_tbl: str):
    '''
    longitudinal records of bmi, and an index bmi table
    '''
    ref_df = session.table(ref_cohort)
    all_obs = session.table(tgt_long_tbl)

    cte_wt = all_obs.filter(
        (col("OBS_NAME").in_("WT",'Body weight','Weight'))
        & (col("OBS_NUM") > 60)
        & (col("OBS_NUM") < 1400)
    )
    cte_ht = all_obs.filter(
        (col("OBS_NAME").in_("HT",'Body height','Height'))
        & (col("OBS_NUM") > 40)
        & (col("OBS_NUM") < 100)
    )
    cte_bmi = all_obs.filter(
        (col("OBS_NAME").in_("ORIGINAL_BMI","Body mass index"))
        & (col("OBS_NUM").between(10, 200))
    )

    cte_all_dt = (
        cte_wt.select("PATID", "OBS_DATE", "DAYS_SINCE_INDEX", "OBS_SRC")
        .union_all(cte_bmi.select("PATID", "OBS_DATE", "DAYS_SINCE_INDEX", "OBS_SRC"))
        .distinct()
    )

    cte_imputed = (
        cte_all_dt
        .join(
            cte_wt,
            (cte_all_dt["PATID"] == cte_wt["PATID"])
            & (cte_all_dt["OBS_SRC"] == cte_wt["OBS_SRC"])
            & (cte_all_dt["OBS_DATE"] == cte_wt["OBS_DATE"]),
            "left",
        )
        .join(
            cte_ht,
            (cte_all_dt["PATID"] == cte_ht["PATID"])
            & (cte_all_dt["OBS_SRC"] == cte_ht["OBS_SRC"])
            & (cte_all_dt["OBS_DATE"] == cte_ht["OBS_DATE"]),
            "left",
        )
        .join(
            cte_bmi,
            (cte_all_dt["PATID"] == cte_bmi["PATID"])
            & (cte_all_dt["OBS_SRC"] == cte_bmi["OBS_SRC"])
            & (cte_all_dt["OBS_DATE"] == cte_bmi["OBS_DATE"]),
            "left",
        )
        .select(
            cte_all_dt["PATID"].alias("PATID"),
            cte_all_dt["OBS_DATE"].alias("OBS_DATE"),
            cte_all_dt["DAYS_SINCE_INDEX"].alias("DAYS_SINCE_INDEX"),
            cte_wt["OBS_NUM"].alias("WT"),
            cte_ht["OBS_NUM"].alias("HT"),
            cte_bmi["OBS_NUM"].alias("BMI"),
            cte_all_dt["OBS_SRC"].alias("OBS_SRC"),
        )
        .with_column(
            "WT_IMPUTED_LAG",
            sql_expr(
                "lag(WT) ignore nulls over (partition by PATID, OBS_SRC order by OBS_DATE)"
            ),
        )
        .with_column(
            "WT_IMPUTED_LEAD",
            sql_expr(
                "lead(WT) ignore nulls over (partition by PATID, OBS_SRC order by OBS_DATE)"
            ),
        )
        .with_column(
            "HT_IMPUTED_LAG",
            sql_expr(
                "lag(HT) ignore nulls over (partition by PATID, OBS_SRC order by OBS_DATE)"
            ),
        )
        .with_column(
            "HT_IMPUTED_LEAD",
            sql_expr(
                "lead(HT) ignore nulls over (partition by PATID, OBS_SRC order by OBS_DATE)"
            ),
        )
        .with_column(
            "BMI_IMPUTED_LAG",
            sql_expr(
                "lag(BMI) ignore nulls over (partition by PATID, OBS_SRC order by OBS_DATE)"
            ),
        )
        .with_column(
            "BMI_IMPUTED_LEAD",
            sql_expr(
                "lead(BMI) ignore nulls over (partition by PATID, OBS_SRC order by OBS_DATE)"
            ),
        )
        .with_column(
            "DEDUP_IDX",
            row_number().over(Window.partition_by("PATID", "OBS_DATE").order_by("OBS_DATE")),
        )
    )

    sel_obs_bmi = (
        cte_imputed.filter(col("DEDUP_IDX") == 1)
        .select(
            col("PATID"),
            col("OBS_DATE"),
            col("DAYS_SINCE_INDEX"),
            coalesce(col("HT"), col("HT_IMPUTED_LAG"), col("HT_IMPUTED_LEAD")).alias("HT"),
            coalesce(col("WT"), col("WT_IMPUTED_LAG"), col("WT_IMPUTED_LEAD")).alias("WT"),
            s_round(
                coalesce(
                    col("BMI"),
                    col("BMI_IMPUTED_LAG"),
                    col("BMI_IMPUTED_LEAD"),
                    coalesce(
                        col("WT"), col("WT_IMPUTED_LAG"), col("WT_IMPUTED_LEAD")
                    )
                    / (
                        coalesce(
                            col("HT"), col("HT_IMPUTED_LAG"), col("HT_IMPUTED_LEAD")
                        )
                        * coalesce(
                            col("HT"), col("HT_IMPUTED_LAG"), col("HT_IMPUTED_LEAD")
                        )
                    )
                    * lit(703)
                )
            ).alias("BMI"),
            row_number().over(
                Window.partition_by("PATID").order_by(s_abs(col("DAYS_SINCE_INDEX")))
            ).alias("RK_IDX_ASC"),
        )
    )

    sel_obs_bmi.write.mode("overwrite").save_as_table("BMI_LONG")

    bmi_idx = (
        ref_df
        .join(
            sel_obs_bmi,
            (ref_df["PATID"] == sel_obs_bmi["PATID"]) & (sel_obs_bmi["RK_IDX_ASC"] == 1),
            "left",
        )
        .select(
            ref_df["PATID"].alias("PATID"),
            s_round(sel_obs_bmi["HT"], 2).alias("HT"),
            s_round(sel_obs_bmi["WT"], 2).alias("WT"),
            s_round(sel_obs_bmi["BMI"], 2).alias("BMI"),
            when(sel_obs_bmi["BMI"] < 18.5, lit("underwt"))
            .when((sel_obs_bmi["BMI"] >= 18.5) & (sel_obs_bmi["BMI"] < 25), lit("normal"))
            .when((sel_obs_bmi["BMI"] >= 25) & (sel_obs_bmi["BMI"] < 30), lit("overwt"))
            .when((sel_obs_bmi["BMI"] >= 30) & (sel_obs_bmi["BMI"] < 35), lit("obeseI"))
            .when((sel_obs_bmi["BMI"] >= 35) & (sel_obs_bmi["BMI"] < 40), lit("obeseII"))
            .when(sel_obs_bmi["BMI"] >= 40, lit("obeseIII"))
            .otherwise(lit("NI"))
            .alias("BMI_CLS"),
            sel_obs_bmi["DAYS_SINCE_INDEX"].alias("BMI_DAYS_SINCE_INDEX"),
        )
    )

    bmi_idx.write.mode("overwrite").save_as_table("BMI_IDX")


def build_cbc_table(session: Session, ref_cohort: str, tgt_long_tbl: str):
    """
    Build CBC longitudinal and index-nearest tables.
    """
    ref_df = session.table(ref_cohort)
    all_obs = session.table(tgt_long_tbl)

    cbc_codes = {
        "WBC": ["6690-2"],
        "RBC": ["789-8"],
        "HGB": ["718-7"],
        "HCT": ["4544-3"],
        "MCV": ["787-2"],
        "MCH": ["785-6"],
        "MCHC": ["786-4"],
        "RDW_CV": ["788-0"],
        "RDW_SD": ["21000-5"],
        "PLT": ["777-3"],
        "MPV": ["32623-1"],
        "NEUT": ["751-8"],
        "LYMPH": ["731-0"],
        "MONO": ["742-7"],
        "EOS": ["711-2"],
        "BASO": ["704-7"],
    }
    all_codes = [code for codes in cbc_codes.values() for code in codes]

    cbc_group_col = (
        when(col("OBS_CODE").in_(cbc_codes["WBC"]), lit("WBC"))
        .when(col("OBS_CODE").in_(cbc_codes["RBC"]), lit("RBC"))
        .when(col("OBS_CODE").in_(cbc_codes["HGB"]), lit("HGB"))
        .when(col("OBS_CODE").in_(cbc_codes["HCT"]), lit("HCT"))
        .when(col("OBS_CODE").in_(cbc_codes["MCV"]), lit("MCV"))
        .when(col("OBS_CODE").in_(cbc_codes["MCH"]), lit("MCH"))
        .when(col("OBS_CODE").in_(cbc_codes["MCHC"]), lit("MCHC"))
        .when(col("OBS_CODE").in_(cbc_codes["RDW_CV"]), lit("RDW_CV"))
        .when(col("OBS_CODE").in_(cbc_codes["RDW_SD"]), lit("RDW_SD"))
        .when(col("OBS_CODE").in_(cbc_codes["PLT"]), lit("PLT"))
        .when(col("OBS_CODE").in_(cbc_codes["MPV"]), lit("MPV"))
        .when(col("OBS_CODE").in_(cbc_codes["NEUT"]), lit("NEUT"))
        .when(col("OBS_CODE").in_(cbc_codes["LYMPH"]), lit("LYMPH"))
        .when(col("OBS_CODE").in_(cbc_codes["MONO"]), lit("MONO"))
        .when(col("OBS_CODE").in_(cbc_codes["EOS"]), lit("EOS"))
        .when(col("OBS_CODE").in_(cbc_codes["BASO"]), lit("BASO"))
        .otherwise(lit(None))
    )

    cbc_long = (
        all_obs.filter(
            col("OBS_NUM").is_not_null()
            & col("OBS_CODE").in_(all_codes)
        )
        .with_column("CBC_GROUP", cbc_group_col)
        .select(
            col("PATID"),
            col("OBS_DATE"),
            col("DAYS_SINCE_INDEX"),
            col("OBS_CODE"),
            col("OBS_NAME"),
            col("OBS_NUM").alias("LAB_VALUE"),
            col("OBS_UNIT"),
            col("OBS_SRC"),
            col("CBC_GROUP"),
        )
    )
    cbc_long.write.mode("overwrite").save_as_table("CBC_LONG")

    def _nearest_by_group(df, group_name, value_alias, day_alias, count_alias):
        w = Window.partition_by(col("PATID"), col("CBC_GROUP")).order_by(
            s_abs(col("DAYS_SINCE_INDEX"))
        )
        return (
            df.filter(col("CBC_GROUP") == lit(group_name))
            .with_column("LAB_ROW_N", row_number().over(w))
            .with_column(
                "LAB_CNT", sql_expr("count(*) over (partition by PATID, CBC_GROUP)")
            )
            .filter(col("LAB_ROW_N") == 1)
            .select(
                col("PATID").alias("PATID"),
                col("LAB_VALUE").alias(value_alias),
                col("DAYS_SINCE_INDEX").alias(day_alias),
                col("LAB_CNT").alias(count_alias),
            )
        )

    wbc = _nearest_by_group(cbc_long, "WBC", "LAB_WBC", "WBC_DAYS", "WBC_CNT")
    rbc = _nearest_by_group(cbc_long, "RBC", "LAB_RBC", "RBC_DAYS", "RBC_CNT")
    hgb = _nearest_by_group(cbc_long, "HGB", "LAB_HGB", "HGB_DAYS", "HGB_CNT")
    hct = _nearest_by_group(cbc_long, "HCT", "LAB_HCT", "HCT_DAYS", "HCT_CNT")
    mcv = _nearest_by_group(cbc_long, "MCV", "LAB_MCV", "MCV_DAYS", "MCV_CNT")
    mch = _nearest_by_group(cbc_long, "MCH", "LAB_MCH", "MCH_DAYS", "MCH_CNT")
    mchc = _nearest_by_group(cbc_long, "MCHC", "LAB_MCHC", "MCHC_DAYS", "MCHC_CNT")
    rdw_cv = _nearest_by_group(cbc_long, "RDW_CV", "LAB_RDW_CV", "RDW_CV_DAYS", "RDW_CV_CNT")
    rdw_sd = _nearest_by_group(cbc_long, "RDW_SD", "LAB_RDW_SD", "RDW_SD_DAYS", "RDW_SD_CNT")
    plt = _nearest_by_group(cbc_long, "PLT", "LAB_PLT", "PLT_DAYS", "PLT_CNT")
    mpv = _nearest_by_group(cbc_long, "MPV", "LAB_MPV", "MPV_DAYS", "MPV_CNT")
    neut = _nearest_by_group(cbc_long, "NEUT", "LAB_NEUT", "NEUT_DAYS", "NEUT_CNT")
    lymph = _nearest_by_group(cbc_long, "LYMPH", "LAB_LYMPH", "LYMPH_DAYS", "LYMPH_CNT")
    mono = _nearest_by_group(cbc_long, "MONO", "LAB_MONO", "MONO_DAYS", "MONO_CNT")
    eos = _nearest_by_group(cbc_long, "EOS", "LAB_EOS", "EOS_DAYS", "EOS_CNT")
    baso = _nearest_by_group(cbc_long, "BASO", "LAB_BASO", "BASO_DAYS", "BASO_CNT")

    cbc_idx = (
        ref_df.join(wbc, ref_df["PATID"] == wbc["PATID"], "left")
        .join(rbc, ref_df["PATID"] == rbc["PATID"], "left")
        .join(hgb, ref_df["PATID"] == hgb["PATID"], "left")
        .join(hct, ref_df["PATID"] == hct["PATID"], "left")
        .join(mcv, ref_df["PATID"] == mcv["PATID"], "left")
        .join(mch, ref_df["PATID"] == mch["PATID"], "left")
        .join(mchc, ref_df["PATID"] == mchc["PATID"], "left")
        .join(rdw_cv, ref_df["PATID"] == rdw_cv["PATID"], "left")
        .join(rdw_sd, ref_df["PATID"] == rdw_sd["PATID"], "left")
        .join(plt, ref_df["PATID"] == plt["PATID"], "left")
        .join(mpv, ref_df["PATID"] == mpv["PATID"], "left")
        .join(neut, ref_df["PATID"] == neut["PATID"], "left")
        .join(lymph, ref_df["PATID"] == lymph["PATID"], "left")
        .join(mono, ref_df["PATID"] == mono["PATID"], "left")
        .join(eos, ref_df["PATID"] == eos["PATID"], "left")
        .join(baso, ref_df["PATID"] == baso["PATID"], "left")
        .select(
            ref_df["PATID"].alias("PATID"),
            wbc["LAB_WBC"], wbc["WBC_DAYS"], wbc["WBC_CNT"],
            rbc["LAB_RBC"], rbc["RBC_DAYS"], rbc["RBC_CNT"],
            hgb["LAB_HGB"], hgb["HGB_DAYS"], hgb["HGB_CNT"],
            hct["LAB_HCT"], hct["HCT_DAYS"], hct["HCT_CNT"],
            mcv["LAB_MCV"], mcv["MCV_DAYS"], mcv["MCV_CNT"],
            mch["LAB_MCH"], mch["MCH_DAYS"], mch["MCH_CNT"],
            mchc["LAB_MCHC"], mchc["MCHC_DAYS"], mchc["MCHC_CNT"],
            rdw_cv["LAB_RDW_CV"], rdw_cv["RDW_CV_DAYS"], rdw_cv["RDW_CV_CNT"],
            rdw_sd["LAB_RDW_SD"], rdw_sd["RDW_SD_DAYS"], rdw_sd["RDW_SD_CNT"],
            plt["LAB_PLT"], plt["PLT_DAYS"], plt["PLT_CNT"],
            mpv["LAB_MPV"], mpv["MPV_DAYS"], mpv["MPV_CNT"],
            neut["LAB_NEUT"], neut["NEUT_DAYS"], neut["NEUT_CNT"],
            lymph["LAB_LYMPH"], lymph["LYMPH_DAYS"], lymph["LYMPH_CNT"],
            mono["LAB_MONO"], mono["MONO_DAYS"], mono["MONO_CNT"],
            eos["LAB_EOS"], eos["EOS_DAYS"], eos["EOS_CNT"],
            baso["LAB_BASO"], baso["BASO_DAYS"], baso["BASO_CNT"],
        )
    )
    cbc_idx.write.mode("overwrite").save_as_table("CBC_IDX")


def build_metabolic_table(session: Session, ref_cohort: str, tgt_long_tbl: str):
    """
    Build metabolic panel longitudinal and index-nearest tables.
    """
    ref_df = session.table(ref_cohort)
    all_obs = session.table(tgt_long_tbl)

    metabolic_codes = {
        "NA": ["2951-2"],
        "K": ["2823-3"],
        "CL": ["2075-0"],
        "CO2": ["2028-9"],
        "GLU": ["2345-7"],
        "BUN": ["3094-0"],
        "CR": ["2160-0"],
        "CA": ["17861-6"],
        "TPRO": ["2885-2"],
        "ALB": ["1751-7"],
        "TBILI": ["1975-2"],
        "ALP": ["6768-6"],
        "AST": ["1920-8"],
        "ALT": ["1742-6"],
    }
    all_codes = [code for codes in metabolic_codes.values() for code in codes]

    metabolic_group_col = (
        when(col("OBS_CODE").in_(metabolic_codes["NA"]), lit("NA"))
        .when(col("OBS_CODE").in_(metabolic_codes["K"]), lit("K"))
        .when(col("OBS_CODE").in_(metabolic_codes["CL"]), lit("CL"))
        .when(col("OBS_CODE").in_(metabolic_codes["CO2"]), lit("CO2"))
        .when(col("OBS_CODE").in_(metabolic_codes["GLU"]), lit("GLU"))
        .when(col("OBS_CODE").in_(metabolic_codes["BUN"]), lit("BUN"))
        .when(col("OBS_CODE").in_(metabolic_codes["CR"]), lit("CR"))
        .when(col("OBS_CODE").in_(metabolic_codes["CA"]), lit("CA"))
        .when(col("OBS_CODE").in_(metabolic_codes["TPRO"]), lit("TPRO"))
        .when(col("OBS_CODE").in_(metabolic_codes["ALB"]), lit("ALB"))
        .when(col("OBS_CODE").in_(metabolic_codes["TBILI"]), lit("TBILI"))
        .when(col("OBS_CODE").in_(metabolic_codes["ALP"]), lit("ALP"))
        .when(col("OBS_CODE").in_(metabolic_codes["AST"]), lit("AST"))
        .when(col("OBS_CODE").in_(metabolic_codes["ALT"]), lit("ALT"))
        .otherwise(lit(None))
    )

    metabolic_long = (
        all_obs.filter(
            col("OBS_NUM").is_not_null()
            & col("OBS_CODE").in_(all_codes)
        )
        .with_column("METAB_GROUP", metabolic_group_col)
        .select(
            col("PATID"),
            col("OBS_DATE"),
            col("DAYS_SINCE_INDEX"),
            col("OBS_CODE"),
            col("OBS_NAME"),
            col("OBS_NUM").alias("LAB_VALUE"),
            col("OBS_UNIT"),
            col("OBS_SRC"),
            col("METAB_GROUP"),
        )
    )
    metabolic_long.write.mode("overwrite").save_as_table("METABOLIC_LONG")

    def _nearest_by_group(df, group_name, value_alias, day_alias, count_alias):
        w = Window.partition_by(col("PATID"), col("METAB_GROUP")).order_by(
            s_abs(col("DAYS_SINCE_INDEX"))
        )
        return (
            df.filter(col("METAB_GROUP") == lit(group_name))
            .with_column("LAB_ROW_N", row_number().over(w))
            .with_column(
                "LAB_CNT", sql_expr("count(*) over (partition by PATID, METAB_GROUP)")
            )
            .filter(col("LAB_ROW_N") == 1)
            .select(
                col("PATID").alias("PATID"),
                col("LAB_VALUE").alias(value_alias),
                col("DAYS_SINCE_INDEX").alias(day_alias),
                col("LAB_CNT").alias(count_alias),
            )
        )

    na = _nearest_by_group(metabolic_long, "NA", "LAB_NA", "NA_DAYS", "NA_CNT")
    k = _nearest_by_group(metabolic_long, "K", "LAB_K", "K_DAYS", "K_CNT")
    cl = _nearest_by_group(metabolic_long, "CL", "LAB_CL", "CL_DAYS", "CL_CNT")
    co2 = _nearest_by_group(metabolic_long, "CO2", "LAB_CO2", "CO2_DAYS", "CO2_CNT")
    glu = _nearest_by_group(metabolic_long, "GLU", "LAB_GLU", "GLU_DAYS", "GLU_CNT")
    bun = _nearest_by_group(metabolic_long, "BUN", "LAB_BUN", "BUN_DAYS", "BUN_CNT")
    cr = _nearest_by_group(metabolic_long, "CR", "LAB_CR", "CR_DAYS", "CR_CNT")
    ca = _nearest_by_group(metabolic_long, "CA", "LAB_CA", "CA_DAYS", "CA_CNT")
    tpro = _nearest_by_group(metabolic_long, "TPRO", "LAB_TPRO", "TPRO_DAYS", "TPRO_CNT")
    alb = _nearest_by_group(metabolic_long, "ALB", "LAB_ALB", "ALB_DAYS", "ALB_CNT")
    tbili = _nearest_by_group(metabolic_long, "TBILI", "LAB_TBILI", "TBILI_DAYS", "TBILI_CNT")
    alp = _nearest_by_group(metabolic_long, "ALP", "LAB_ALP", "ALP_DAYS", "ALP_CNT")
    ast = _nearest_by_group(metabolic_long, "AST", "LAB_AST", "AST_DAYS", "AST_CNT")
    alt = _nearest_by_group(metabolic_long, "ALT", "LAB_ALT", "ALT_DAYS", "ALT_CNT")

    metabolic_idx = (
        ref_df.join(na, ref_df["PATID"] == na["PATID"], "left")
        .join(k, ref_df["PATID"] == k["PATID"], "left")
        .join(cl, ref_df["PATID"] == cl["PATID"], "left")
        .join(co2, ref_df["PATID"] == co2["PATID"], "left")
        .join(glu, ref_df["PATID"] == glu["PATID"], "left")
        .join(bun, ref_df["PATID"] == bun["PATID"], "left")
        .join(cr, ref_df["PATID"] == cr["PATID"], "left")
        .join(ca, ref_df["PATID"] == ca["PATID"], "left")
        .join(tpro, ref_df["PATID"] == tpro["PATID"], "left")
        .join(alb, ref_df["PATID"] == alb["PATID"], "left")
        .join(tbili, ref_df["PATID"] == tbili["PATID"], "left")
        .join(alp, ref_df["PATID"] == alp["PATID"], "left")
        .join(ast, ref_df["PATID"] == ast["PATID"], "left")
        .join(alt, ref_df["PATID"] == alt["PATID"], "left")
        .select(
            ref_df["PATID"].alias("PATID"),
            na["LAB_NA"], na["NA_DAYS"], na["NA_CNT"],
            k["LAB_K"], k["K_DAYS"], k["K_CNT"],
            cl["LAB_CL"], cl["CL_DAYS"], cl["CL_CNT"],
            co2["LAB_CO2"], co2["CO2_DAYS"], co2["CO2_CNT"],
            glu["LAB_GLU"], glu["GLU_DAYS"], glu["GLU_CNT"],
            bun["LAB_BUN"], bun["BUN_DAYS"], bun["BUN_CNT"],
            cr["LAB_CR"], cr["CR_DAYS"], cr["CR_CNT"],
            ca["LAB_CA"], ca["CA_DAYS"], ca["CA_CNT"],
            tpro["LAB_TPRO"], tpro["TPRO_DAYS"], tpro["TPRO_CNT"],
            alb["LAB_ALB"], alb["ALB_DAYS"], alb["ALB_CNT"],
            tbili["LAB_TBILI"], tbili["TBILI_DAYS"], tbili["TBILI_CNT"],
            alp["LAB_ALP"], alp["ALP_DAYS"], alp["ALP_CNT"],
            ast["LAB_AST"], ast["AST_DAYS"], ast["AST_CNT"],
            alt["LAB_ALT"], alt["ALT_DAYS"], alt["ALT_CNT"],
        )
    )
    metabolic_idx.write.mode("overwrite").save_as_table("METABOLIC_IDX")



def main():
    env.load_env()
    connect_params = conn.load_snowcfg("ID")
    session = conn.get_snow_conn(connect_params)
    session.use_database(os.environ["ID_SNOW_DATABASE"])
    session.use_schema("SX_ALS_GPC")
    print(session.sql("SELECT CURRENT_USER(), CURRENT_ROLE(), CURRENT_DATABASE()").collect())

    ref_cohort = "ALS_CASE_TABLE1"
    tgt_long_tbl = "ALS_ALL_OBS"
    build_bmi_table(session, ref_cohort, tgt_long_tbl)
    print("✅ BMI tables built.")
    build_cbc_table(session, ref_cohort, tgt_long_tbl)
    print("✅ CBC tables built.")
    build_metabolic_table(session, ref_cohort, tgt_long_tbl)
    print("✅ Metabolic tables built.")
    
if __name__ == "__main__":
    main()
