/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: pat-glp1.sql
# Description: identify all patients who have ever used GLP1 agonist or DPP4 inhibitor
# Dependency: 
# - Z_GLP1_DPP4_RXN_NDC: staged from rxnorm and ndc ref file 
*/

select * from Z_GLP1_DPP4_RXN_NDC limit 5;

create or replace temporary table RXCUI_REF as 
select distinct 
       split_part(SUBCLS,' ',1) as RX_CLS,
       ING as RX_IN,
       to_char(RXCUI) as RXCUI
from Z_GLP1_DPP4_RXN_NDC
;

create or replace temporary table NDC_REF as 
select distinct * 
from (
    select distinct 
        split_part(SUBCLS,' ',1) as RX_CLS,
        ING as RX_IN,
        NDC11,
        NDC10
    from Z_GLP1_DPP4_RXN_NDC
    where coalesce(NDC11,NDC10) is not null
)
unpivot (
    NDC for NDC_TY in (NDC11, NDC10)
)
where NDC is not null
;


create or replace procedure get_glp1_event_long(
    SITES array,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * Stored procedure to collect a long tables for all GLP1 or DPP4 prescription or dispense:
 * @param {array} SITES: an array of site acronyms (matching schema name suffix) - include CMS
 * @param {boolean} DRY_RUN: dry run indicator. If true, only sql script will be created and stored in dev.sp_out table
 * @param {boolean} DRY_RUN_AT: A temporary location to store the generated sql query for debugging purpose. 
                                When DRY_RUN = True, provide absolute path to the table; when DRY_RUN = False, provide NULL 
**/
if (DRY_RUN) {
    var log_stmt = snowflake.createStatement({
        sqlText: `CREATE OR REPLACE TEMPORARY TABLE `+ DRY_RUN_AT +`(QRY VARCHAR);`});
    log_stmt.execute(); 
}

var i;
for(i=0; i<SITES.length; i++){
    var site = SITES[i].toString();
    var site_cdm = (site === 'CMS') ? 'CMS_PCORNET_CDM' : 'PCORNET_CDM_' + site;
    
    // dynamic query
    var sqlstmt_par_drx = `
        INSERT INTO GLP1_EVENT_LONG
        SELECT  a.patid,
                a.dispensingid as RXID,
                'DRX' AS event_type,
                round(datediff(day,b.birth_date,a.dispense_date)/365.25) AS age_at_event,
                d.RX_CLS,
                d.RX_IN,
                a.dispense_date AS RX_START_DATE,
                NULL as RX_END_DATE,
                a.dispense_dose_disp AS RX_DOSE,
                coalesce(a.dispense_dose_disp_unit,a.raw_dispense_dose_disp_unit) AS RX_DOSE_UNIT,
                try_to_number(a.dispense_amt::integer) AS RX_AMT,
                try_to_number(a.dispense_sup::integer) AS RX_SUP,         
                0 as RX_REFILL,
                NULL as RX_FREQUENCY,
                a.NDC as RAW_RX_CODE,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DISPENSING a
        JOIN NDC_REF d ON a.NDC = d.NDC
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;
        `;
    
    var sqlstmt_par_prx = `
       INSERT INTO GLP1_EVENT_LONG
        SELECT  a.patid,
                a.prescribingid as RXID,
                'PRX' AS event_type,
                round(datediff(day,b.birth_date,coalesce(a.rx_order_date,a.rx_start_date))/365.25) AS age_at_event,
                p.RX_CLS,
                p.RX_IN,
                coalesce(a.rx_order_date,a.rx_start_date) AS RX_START_DATE,
                coalesce(a.rx_end_date,a.rx_start_date) AS RX_END_DATE,
                a.rx_dose_ordered AS RX_DOSE,
                coalesce(a.rx_dose_ordered_unit,a.raw_rx_dose_ordered_unit) AS RX_DOSE_UNIT,
                coalesce(try_to_number(a.rx_quantity::integer),try_to_number(a.raw_rx_quantity)) AS RX_AMT,
                try_to_number(a.rx_days_supply::integer) AS RX_SUP,  
                try_to_number(a.rx_refills::integer) AS RX_REFILL,
                coalesce(a.rx_frequency,a.raw_rx_frequency) AS RX_FREQUENCY,
                a.RXNORM_CUI as RAW_RX_CODE,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_PRESCRIBING a
        JOIN RXCUI_REF p ON a.RXNORM_CUI = p.RXCUI
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;
        `;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1), (:2);`,
                        binds: [sqlstmt_par_drx,sqlstmt_par_prx]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par_drx = snowflake.createStatement({sqlText: sqlstmt_par_drx}); run_sqlstmt_par_drx.execute();
        
        if(site != 'CMS'){
            var run_sqlstmt_par_prx = snowflake.createStatement({sqlText: sqlstmt_par_prx}); run_sqlstmt_par_prx.execute();
        }
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

/*test*/
-- call get_glp1_event_long(
--     array_construct(
--          'CMS'
--         ,'ALLINA'
--     ),
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;
create or replace table GLP1_EVENT_LONG (
    PATID varchar(50) NOT NULL,
    RXID varchar(200),
    EVENT_TYPE varchar(10) NOT NULL,
    AGE_AT_EVENT integer,
    RX_CLS varchar(5),
    RX_IN varchar(20),
    RX_START_DATE date, 
    RX_END_DATE date, 
    RX_DOSE varchar(50),
    RX_DOSE_UNIT varchar(50),
    RX_AMT integer,
    RX_SUP integer,
    RX_REFILL integer,
    RX_FREQUENCY varchar(5),
    RAW_RX_CODE varchar(20),
    RX_SRC varchar(10)
);
call get_glp1_event_long(
    array_construct(
         'CMS'
        ,'ALLINA'
        ,'IHC'
        ,'KUMC'
        ,'MCRI'
        ,'MCW'
        ,'MU'
        ,'UIOWA'
        ,'UNMC'
        ,'UTHOUSTON'
        ,'UTHSCSA'
        ,'UTSW'
        ,'UU'
        ,'WASHU'
    ), 
    FALSE, NULL
);

select * from GLP1_EVENT_LONG limit 5;

select count(distinct patid), count(distinct rxid), count(*)
from GLP1_EVENT_LONG;
-- 1606792

select RX_CLS, count(distinct patid), count(distinct rxid), count(*)
from GLP1_EVENT_LONG
group by RX_CLS;
-- DPP4	957359
-- GLP1	850536

select RX_CLS, count(distinct patid), count(distinct rxid), count(*)
from GLP1_EVENT_LONG
where RX_START_DATE between '2011-01-01' and '2020-12-31'
group by RX_CLS;
-- DPP4	896983
-- GLP1	525080

select RX_SRC, count(distinct patid), count(distinct rxid), count(*)
from GLP1_EVENT_LONG
where RX_CLS = 'GLP1'
group by RX_SRC
order by count(distinct patid) desc;

create or replace table GLP1_DPP4_TABLE1 as 
with cte_either as (
    select patid, 
           min(RX_START_DATE) as RX_START_DATE,
           max(RX_END_DATE) as RX_END_DATE
    from GLP1_EVENT_LONG
    group by patid
), cte_glp1 as (
    select patid, 
           min(RX_START_DATE) as RX_START_DATE,
           max(RX_END_DATE) as RX_END_DATE
    from GLP1_EVENT_LONG
    where RX_CLS = 'GLP1'
    group by patid
), cte_dpp4 as (
    select patid, 
           min(RX_START_DATE) as RX_START_DATE,
           max(RX_END_DATE) as RX_END_DATE
    from GLP1_EVENT_LONG
    where RX_CLS = 'DPP4'
    group by patid
)
select a.patid, 
       a.rx_start_date,
       a.rx_end_date,
       case when b.rx_start_date is not null then 1 else 0 end as GLP1_IND,
       b.rx_start_date as GLP1_START_DATE, 
       b.rx_end_date as GLP1_END_DATE, 
       case when d.rx_start_date is not null then 1 else 0 end as DPP4_IND,
       d.rx_start_date as DPP4_START_DATE, 
       d.rx_end_date as DPP4_END_DATE
from cte_either a 
left join cte_glp1 b on a.patid = b.patid
left join cte_dpp4 d on a.patid = d.patid
;

select * from GLP1_DPP4_TABLE1 limit 5;
select count(distinct patid), count(*), sum(GLP1_IND), sum(DPP4_IND),sum(GLP1_IND*DPP4_IND)
from GLP1_DPP4_TABLE1
;
-- 1606792	1606792	850536	957359	201103

create or replace table ALS_GLP1 as 
with als_glp1 as (
    select a.patid, 
           a.RX_CLS,
           a.RX_IN,
           a.RX_START_DATE,
           datediff('day',b.als1dx_date, a.RX_START_DATE) as DAYS_GLP1_TO_ALS1,
           row_number() over (partition by a.patid order by a.RX_START_DATE) as rn
    from GLP1_EVENT_LONG a
    join ALS_CASE_TABLE1 b
    on a.patid = b.patid
)
select a.PATID, 
       b.RX_CLS,
       b.RX_IN,
       b.RX_START_DATE,
       b.DAYS_GLP1_TO_ALS1,
       case when b.DAYS_GLP1_TO_ALS1 < 0 then 'bef'
            else 'aft'
       end as GLP1_TIME_GRP
from ALS_CASE_TABLE1 a 
join als_glp1 b
on a.patid = b.patid 
where b.rn = 1
;

select * from ALS_GLP1 limit 5;

select count(distinct patid) 
from ALS_GLP1
;
-- 991

select count(distinct patid) 
from ALS_GLP1
where DAYS_GLP1_TO_ALS1 > 0
;
-- 350

select * from ALS_ENDPTS limit 5;
create or replace table ALS_GLP1_TRT_CTRL as 
with dth_censor as (
    select * from 
    (
        select patid, 
               endpt,
               stage_since_index
    from ALS_ENDPTS 
    where endpt in ('censor','death')
    )
    pivot(
        min(stage_since_index) for endpt in ('censor','death')
    ) as 
    p(patid, DAYS_ALS1_TO_CENSOR, DAYS_ALS1_TO_DEATH)
),  bl_bmi as (
    select patid, days_since_index as DAYS_ALS1_TO_BMI, obs_num as BMI_AT_ALS1
    from (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from ALS_SEL_OBS a
        where a.obs_name = 'BMI' and abs(a.days_since_index) <= 90
    )
    where rn = 1
), bl_gluc as (
    select patid, days_since_index as DAYS_ALS1_TO_GLUC, obs_num as GLUC_AT_ALS1, 1 as IND
    from (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from ALS_SEL_OBS a
        where a.obs_name = 'Glucose' and abs(a.days_since_index) <= 90
    )
    where rn = 1
), bl_hba1c as (
    select patid, days_since_index as DAYS_ALS1_TO_HBA1C, obs_num as HBA1C_AT_ALS1, 1 as IND
    from (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from ALS_SEL_OBS a
        where a.obs_name = 'Hemoglobin A1c/Hemoglobin.total' and abs(a.days_since_index) <= 90
    )
    where rn = 1
), bl_chol as (
    select patid, days_since_index as DAYS_ALS1_TO_CHOL, obs_num as CHOL_AT_ALS1, 1 as IND
    from (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from ALS_SEL_OBS a
        where a.obs_name = 'Cholesterol' and abs(a.days_since_index) <= 90
    )
    where rn = 1
), bl_hdl as (
    select patid, days_since_index as DAYS_ALS1_TO_HDL, obs_num as HDL_AT_ALS1, 1 as IND
    from (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from ALS_SEL_OBS a
        where a.obs_name = 'Cholesterol.in HDL' and abs(a.days_since_index) <= 90
    )
    where rn = 1
), bl_ldl as (
    select patid, days_since_index as DAYS_ALS1_TO_LDL, obs_num as LDL_AT_ALS1, 1 as IND
    from (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from ALS_SEL_OBS a
        where a.obs_name = 'Cholesterol.in LDL' and abs(a.days_since_index) <= 90
    )
    where rn = 1
), bl_trig as (
    select patid, days_since_index as DAYS_ALS1_TO_TRIG, obs_num as TRIG_AT_ALS1, 1 as IND
    from (
        select a.*, row_number() over (partition by a.patid order by abs(a.days_since_index)) rn
        from ALS_SEL_OBS a
        where a.obs_name = 'Triglyceride' and abs(a.days_since_index) <= 90
    )
    where rn = 1
), hist_dm as (
    select patid, days_since_index as DAYS_ALS1_TO_DM, 1 as IND
    from (
        select a.*, row_number() over (partition by a.patid order by a.days_since_index) rn
        from ALS_ALL_PHECD a
        where a.phecd_dxgrpcd = '202' and a.days_since_index <= 90
    )
    where rn = 1
), use_metform as (
    select patid, 
           min(days_since_index) as DAYS_ALS1_TO_METFM_F, 
           max(days_since_index) as DAYS_ALS1_TO_METFM_L,
           1 as IND
    from ALS_ALL_RX_IN
    where lower(ingredient) like '%metform%'
    group by patid
)
select distinct 
       a.*,
       g.RX_CLS,
       g.RX_IN,
       g.RX_START_DATE,
       year(g.RX_START_DATE) as RX_START_YEAR,
       g.DAYS_GLP1_TO_ALS1,
       coalesce(g.GLP1_TIME_GRP,'none') as GLP1_USE_GRP,
       b.DAYS_ALS1_TO_BMI,
       b.BMI_AT_ALS1,
       case when b.BMI_AT_ALS1 >= 30 then 'obese'
            when b.BMI_AT_ALS1 >= 25 and b.BMI_AT_ALS1 < 30 then 'overweight'
            when b.BMI_AT_ALS1 < 18.5 then 'underweight'
            else 'normal'
       end as BMIGRP_AT_ALS1,
       mt.DAYS_ALS1_TO_METFM_F,
       mt.DAYS_ALS1_TO_METFM_L,
       coalesce(mt.IND,0) as METFM_IND,
       gl.GLUC_AT_ALS1, 
       coalesce(gl.IND, 0) as GLUC_IND,
       hb.HBA1C_AT_ALS1, 
       coalesce(hb.IND, 0) as HBA1C_IND,
       ch.CHOL_AT_ALS1, 
       coalesce(ch.IND, 0) as CHOL_IND,
       hdl.HDL_AT_ALS1, 
       coalesce(hdl.IND, 0) as HDL_IND,
       ldl.LDL_AT_ALS1, 
       coalesce(ldl.IND, 0) as LDL_IND,
       tr.TRIG_AT_ALS1, 
       coalesce(tr.IND, 0) as TRIG_IND,
       c.DAYS_ALS1_TO_CENSOR,
       c.DAYS_ALS1_TO_DEATH,
       coalesce(c.DAYS_ALS1_TO_CENSOR,c.DAYS_ALS1_TO_DEATH) as DAYS_ALS1_TO_END,
       case when DAYS_ALS1_TO_DEATH is not null then 1 else 0 end as STATUS
from ALS_CASE_TABLE1 a 
join DM_TABLE1 dm 
on a.patid = dm.patid
left join ALS_GLP1 g
on a.patid = g.patid
join bl_bmi b
on a.patid = b.patid
left join bl_gluc gl
on a.patid = gl.patid
left join bl_hba1c hb
on a.patid = hb.patid
left join bl_chol ch
on a.patid = ch.patid
left join bl_hdl hdl
on a.patid = hdl.patid
left join bl_ldl ldl
on a.patid = ldl.patid
left join bl_trig tr
on a.patid = tr.patid
left join use_metform mt
on a.patid = mt.patid
left join dth_censor c 
on a.patid = c.patid
where a.index_date between '2011-01-01' and '2020-12-31'
;

select * from ALS_GLP1_TRT_CTRL 
where RX_START_YEAR = 2011 and RX_CLS = 'GLP1'
order by patid
-- limit 5
;

select count(distinct patid) from ALS_GLP1_TRT_CTRL;
-- 1128

select RX_START_YEAR, count(distinct patid) as denom_N
from ALS_GLP1_TRT_CTRL
group by RX_START_YEAR
;

with denom as (
    select GLP1_USE_GRP, BMIGRP_AT_ALS1, count(distinct patid) as denom_N
    from ALS_GLP1_TRT_CTRL
    group by GLP1_USE_GRP, BMIGRP_AT_ALS1
)
select a.GLP1_USE_GRP,a.BMIGRP_AT_ALS1,a.status, count(distinct a.patid) as pat_n, 
       round(count(distinct a.patid)/denom.denom_N,4) as pat_p
from ALS_GLP1_TRT_CTRL a
join denom 
on a.GLP1_USE_GRP = denom.GLP1_USE_GRP and a.BMIGRP_AT_ALS1 = denom.BMIGRP_AT_ALS1
group by a.GLP1_USE_GRP,a.BMIGRP_AT_ALS1, a.status, denom.denom_N
order by a.GLP1_USE_GRP,a.BMIGRP_AT_ALS1, a.status
;