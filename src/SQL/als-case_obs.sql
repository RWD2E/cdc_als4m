/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_obs.sql
*/
create or replace procedure get_obs_long(
    REF_COHORT string,
    SITES array,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} REF_COHORT: name of reference patient table, should at least include (patid, birth_date, index_date)
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
       var raw_rx_name = (site === 'CMS') ? 'COALESCE(d.RAW_RX_MED_NAME,g.STR)' : 'g.STR';

       // collect all available labs
       sqlstmt1 = `
              INSERT INTO ALS_ALL_OBS
              select  distinct
                      a.PATID
                     ,coalesce(b.specimen_date, b.lab_order_date, b.result_date) as OBS_DATE
                     ,datediff(day,a.index_date,coalesce(b.specimen_date, b.lab_order_date, b.result_date)) as DAYS_SINCE_INDEX
                     ,'LC' as OBS_CODE_TYPE
                     ,b.lab_loinc as OBS_CODE
                     ,coalesce(c.component,b.raw_lab_name) as OBS_NAME
                     ,b.result_num as OBS_NUM
                     ,b.result_unit as OBS_UNIT
                     ,b.norm_range_low as OBS_REF_LOW
                     ,b.norm_range_high as OBS_REF_HIGH
                     ,b.result_qual as OBS_QUAL
                     ,'lab' as OBS_TYPE
                     ,'`+ site +`' as OBS_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_LAB_RESULT_CM b
                     on a.patid = b.patid
              left join ONTOLOGY.LOINC.LOINC_V2_17 c
                     on b.lab_loinc = c.loinc_num
              ;
       `;
       var run_sqlstmt1 = snowflake.createStatement({sqlText: sqlstmt1});

       // collect all available vitals
       sqlstmt2 = `
              INSERT INTO ALS_ALL_OBS(patid,obs_date,days_since_index,obs_code_type,obs_num,obs_unit,obs_qual,obs_name,obs_type,obs_src)
              with cte_unpvt_num as (
                     select patid, measure_date, measure_time, 
                            OBS_NAME, OBS_NUM,'NI' as OBS_QUAL,
                            case when OBS_NAME in ('SYSTOLIC','DIASTOLIC') then 'mm[Hg]'
                                 when OBS_NAME = 'HT' then 'in_us'
                                 when OBS_NAME = 'WT' then 'lb_av'
                                 when OBS_NAME = 'ORIGINAl_BMI' then 'kg/m2'
                                 else null
                            end as OBS_UNIT
                     from (
                            select patid, measure_date, measure_time,
                                   round(systolic) as systolic, 
                                   round(diastolic) as diastolic, 
                                   round(ht) as ht, 
                                   round(wt) as wt, 
                                   round(original_bmi) as original_bmi
                            from GROUSE_DB.`+ site_cdm +`.LDS_VITAL
                     )
                     unpivot (
                            OBS_NUM
                            for OBS_NAME in (
                                   systolic, diastolic, ht, wt, original_bmi
                            )
                     )
                     where OBS_NUM is not null and trim(OBS_NUM) <> ''
              ), cte_unpvt_qual as (
                     select patid, measure_date, measure_time, 
                            OBS_NAME, NULL as OBS_NUM, OBS_QUAL, NULL as OBS_UNIT
                     from (
                            select patid, measure_date, measure_time,
                            smoking, tobacco, tobacco_type
                            from GROUSE_DB.`+ site_cdm +`.LDS_VITAL
                     ) 
                     unpivot (
                            OBS_QUAL
                            for OBS_NAME in (
                                   smoking, tobacco, tobacco_type
                            )
                     )
                     where OBS_QUAL is not null and trim(OBS_QUAL) <> '' 
                       and OBS_QUAL not in ('UN','NI','OT')
              )
              select  distinct
                      a.PATID
                     ,b.measure_date as OBS_DATE
                     ,datediff(day,a.index_date,b.measure_date) as DAYS_SINCE_INDEX
                     ,'UD' as OBS_CODE_TYPE 
                     ,b.OBS_NUM
                     ,b.OBS_UNIT
                     ,b.OBS_QUAL
                     ,b.OBS_NAME
                     ,'vital' as OBS_TYPE
                     ,'`+ site +`' as OBS_SRC
              from `+ REF_COHORT +` a
              join (
                     select * from cte_unpvt_num
                     union 
                     select * from cte_unpvt_qual
              ) b
                     on a.patid = b.patid
              ;
       `;
       var run_sqlstmt2 = snowflake.createStatement({sqlText: sqlstmt2});

       // collect all available obs_clin
       sqlstmt3 = `
              INSERT INTO ALS_ALL_OBS(patid,obs_date,days_since_index,obs_code_type,obs_code,obs_name,obs_num,obs_unit,obs_qual,obs_type,obs_src)
              select  distinct
                      a.PATID
                     ,coalesce(b.obsclin_start_date, b.obsclin_stop_date) as OBS_DATE
                     ,datediff(day,a.index_date,coalesce(b.obsclin_start_date, b.obsclin_stop_date)) as DAYS_SINCE_INDEX
                     ,b.obsclin_type as OBS_CODE_TYPE
                     ,b.obsclin_code as OBS_CODE
                     ,coalesce(c.component,b.raw_obsclin_name) as OBS_NAME
                     ,b.obsclin_result_num as OBS_NUM
                     ,b.obsclin_result_unit as OBS_UNIT
                     ,coalesce(trim(b.obsclin_result_qual),trim(a.obsclin_result_text)) as OBS_QUAL
                     ,'obsclin' as OBS_TYPE
                     ,'`+ site +`' as OBS_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_OBS_CLIN b
                     on a.patid = b.patid
              left join ONTOLOGY.LOINC.LOINC_V2_17 c
                     on b.obsclin_code = c.loinc_num and b.obsclin_type = 'LC'
              where b.obsclin_result_num is not null
                 or (
                     coalesce(trim(b.obsclin_result_qual),trim(b.obsclin_result_text)) is not null 
                     and coalesce(trim(b.obsclin_result_qual),trim(b.obsclin_result_text)) <> '' 
                     and coalesce(trim(b.obsclin_result_qual),trim(b.obsclin_result_text)) not in ('UN','NI','OT')
                    )
              ;

       `;

       // collect all available from obs_gen

       var run_sqlstmt3 = snowflake.createStatement({sqlText: sqlstmt3});

       if (DRY_RUN) {
              // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
              var log_stmt = snowflake.createStatement({
                            sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1),(:2),(:3);`,
                            binds: [sqlstmt1, sqlstmt2,sqlstmt3]});
        log_stmt.execute(); 
       } else {
              // run dynamic dml query
              var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
              try{run_sqlstmt1.execute();} catch(error) {};
              try{run_sqlstmt2.execute();} catch(error) {};
              try{run_sqlstmt3.execute();} catch(error) {};
              commit_txn.execute();
       }
}
$$
;

/* test */
-- call get_obs_long(
--        'ALS_TABLE1',
--        array_construct(
--               'CMS'
--              ,'MU'
--        ),
--        True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;

/* main */
create or replace table ALS_ALL_OBS (
        PATID varchar(50) NOT NULL
       ,OBS_DATE date
       ,DAYS_SINCE_INDEX number
       ,OBS_CODE_TYPE varchar(10)
       ,OBS_CODE varchar(100)
       ,OBS_NAME varchar(500)
       ,OBS_NUM number 
       ,OBS_UNIT varchar(50)
       ,OBS_REF_LOW varchar(100)
       ,OBS_REF_HIGH varchar(100) 
       ,OBS_QUAL varchar(100)
       ,OBS_TYPE varchar(105)
       ,OBS_SRC varchar(10)
);

call get_obs_long(
       'ALS_TABLE1',
       array_construct(
       --   'CMS'
         'ALLINA'
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

select * from ALS_ALL_OBS limit 5;

select count(distinct patid), count(*) from ALS_ALL_OBS;
-- 11623	5979774

select obs_name, obs_code, count(distinct patid)
from als_all_obs
where lower(obs_name) like '%platelet%' 
  and lower(obs_name) not like '%platelet poor plasma%'
group by obs_name, obs_code
order by count(distinct patid) desc;


create or replace table ALS_OBS_SUMM as 
select obs_name
      ,count(distinct patid) as lab_n
from als_all_obs
group by  obs_name
;
select * from ALS_OBS_SUMM;


create or replace table ALS_SEL_OBS as
with dem as (
       select count(distinct PATID) as N 
       from als_all_obs    
), lab_sel as (
       select distinct 
              a.obs_name,a.lab_n
       from ALS_OBS_SUMM a
       cross join dem 
       where a.lab_n/dem.N >= 0.2 -- at least populated for 20% of the population
)
select a.* 
from ALS_ALL_OBS a
where exists (
       select 1 from lab_sel
       where lab_sel.obs_name = a.obs_name
) and a.obs_name not in (
       'SYSTOLIC',
       'DIASTOILC',
       'WT',
       'HT',
       'ORIGINAL_BMI'
)
union
select * from ALS_ALL_OBS
where obs_name = 'SYSTOLIC'
  and obs_num between 30 and 300
union
select * from ALS_ALL_OBS
where obs_name = 'DIASTOILC'
  and obs_num between 1 and 250
;

create or replace table ALS_SEL_OBS_BMI as
with cte_wt as (
       select * from ALS_ALL_OBS
       where obs_name in ('WT') 
         and obs_num > 60 and obs_num < 1400
),   cte_ht as (
       select * from ALS_ALL_OBS
       where obs_name in ('HT')
         and obs_num > 40 and obs_num < 100
),   cte_bmi as (
       select * from ALS_ALL_OBS
       where obs_name in ('ORIGINAL_BMI') 
         and obs_num between 10 and 200
),  cte_all_dt as (
       select distinct patid, obs_date, days_since_index,obs_src from cte_wt
       union 
       select distinct patid, obs_date, days_since_index,obs_src from cte_bmi
)
select a.patid,
       a.obs_date,
       a.days_since_index,
       wt.obs_num as wt,
       coalesce(wt.obs_num,lag(wt.obs_num) ignore nulls over (partition by a.patid,a.obs_src order by a.obs_date)) as wt_imputed_lag,
       coalesce(wt.obs_num,lead(wt.obs_num) ignore nulls over (partition by a.patid,a.obs_src order by a.obs_date)) as wt_imputed_lead,
       ht.obs_num as ht,
       coalesce(ht.obs_num,lag(ht.obs_num) ignore nulls over (partition by a.patid,a.obs_src order by a.obs_date)) as ht_imputed_lag,
       coalesce(ht.obs_num,lead(ht.obs_num) ignore nulls over (partition by a.patid,a.obs_src order by a.obs_date)) as ht_imputed_lead,
       bmi.obs_num as bmi,
       coalesce(bmi.obs_num,lag(bmi.obs_num) ignore nulls over (partition by a.patid,a.obs_src order by a.obs_date)) as bmi_imputed_lag,
       coalesce(bmi.obs_num,lead(bmi.obs_num) ignore nulls over (partition by a.patid,a.obs_src order by a.obs_date)) as bmi_imputed_lead,
       a.obs_src,
       row_number() over (partition by a.patid, a.obs_date order by a.obs_date) as dedup_idx
from cte_all_dt a
left join cte_wt wt on wt.patid = a.patid and wt.obs_src = a.obs_src and wt.obs_date = a.obs_date
left join cte_ht ht on ht.patid = a.patid and ht.obs_src = a.obs_src  and ht.obs_date = a.obs_date
left join cte_bmi bmi on bmi.patid = a.patid and bmi.obs_src = a.obs_src  and bmi.obs_date = a.obs_date
;

insert into ALS_SEL_OBS(patid, obs_date, days_since_index, obs_code_type, obs_name, obs_num, obs_unit, obs_src)
-- select patid,
--        obs_date,
--        days_since_index,
--        'UD' as obs_code_type,
--        'HT' as obs_name,
--        coalesce(ht,ht_imputed_lag,ht_imputed_lead) as obs_num,
--        'in_us' as obs_unit,
--        obs_src
-- from ALS_SEL_OBS_BMI
-- where dedup_idx = 1
-- union
-- select patid,
--        obs_date,
--        days_since_index,
--        'UD' as obs_code_type,
--        'WT' as obs_name,
--        coalesce(wt,wt_imputed_lag,wt_imputed_lead) as obs_num,
--        'lb_av' as obs_unit,
--        obs_src
-- from ALS_SEL_OBS_BMI
-- where dedup_idx = 1
-- union
select patid,
       obs_date,
       days_since_index,
       'UD' as obs_code_type,
       'BMI' as obs_name,
       round(coalesce(bmi,bmi_imputed_lag,bmi_imputed_lead,coalesce(wt,wt_imputed_lag,wt_imputed_lead)/(coalesce(ht,ht_imputed_lag,ht_imputed_lead)*coalesce(ht,ht_imputed_lag,ht_imputed_lead))*703)) as obs_num,
       'kg/m2' as obs_unit,
       obs_src
from ALS_SEL_OBS_BMI
where dedup_idx = 1
;

select count(distinct obs_name), count(distinct patid) 
from ALS_SEL_OBS
;
-- 70	11571

select obs_name, count(distinct patid) 
from ALS_SEL_OBS
group by obs_name;