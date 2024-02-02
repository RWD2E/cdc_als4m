/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case-mu_cdc-platelet.sql
# Description: collecting different variations of platelet profile for ALS patients 
*/

/****************************************************************************************************************/
/*set environment*/
use role GROUSE_ROLE_C_ANALYTICS;
use warehouse GROUSE_WH;
use database GROUSE_DEID_ANALYTICS_DB; -- write-premitted database

create schema if not exists ALS_MU;
use schema ALS_MU;

select * from ALS_CASE;
select count(distinct patid) from ALS_CASE;
-- 417

select extract(year from first_dx_date), count(distinct patid) 
from ALS_CMS_PCORNET_CDM.ALS_CASE_EHR
group by extract(year from first_dx_date)
order by extract(year from first_dx_date)
;

set cms_cdm_schema = 'GROUSE_DEID_DB.PCORNET_CDM_MU';
set lab_result_cm = $cms_cdm_schema || '.V_DEID_LAB_RESULT_CM';
set encounter = $cms_cdm_schema || '.V_DEID_ENCOUNTER';

/****************************************************************************************************************/
/* Get all platelet replated LOINC codes which also exists in our database*/
create or replace temporary table VS_PLATELET as
select distinct
       a.loinc_num,
       a.component,
       a.long_common_name,
       a.time_aspct,
       a.system,
       a.scale_typ,
       a.class,
       a.status
from ONTOLOGY.LOINC.LOINC_V2_17 a
where lower(long_common_name) like '%platelet%' and 
      exists (select 1 from identifier($lab_result_cm) b where a.loinc_num = b.lab_loinc)
;

select * from VS_PLATELET;

/* frequency check*/
create or replace table ALS_CASE_PLATELET_ALL as
with lab_stk as (
    select a.*,
       b.LAB_LOINC, c.long_common_name,
       b.RESULT_NUM, b.RESULT_UNIT,
       b.RESULT_DATE, b.LAB_ORDER_DATE, b.SPECIMEN_DATE,
       b.NORM_RANGE_LOW,b.NORM_RANGE_HIGH,
       datediff(day, a.first_dx_date, coalesce(b.SPECIMEN_DATE, b.RESULT_DATE, b.LAB_ORDER_DATE)) as LAB_DAYS_SINCE_DX1,
       d.ENC_TYPE, d.ADMIT_DATE, d.DISCHARGE_DATE,
       datediff(day, a.first_dx_date, d.ADMIT_DATE) as ENC_DAYS_SINCE_DX1, 
       datediff(day, d.ADMIT_DATE, d.DISCHARGE_DATE) as LOS
    from ALS_CASE a
    join identifier($lab_result_cm) b on a.patid = b.patid
    join VS_PLATELET c on c.loinc_num = b.lab_loinc
    left join identifier($encounter) d on b.encounterid = d.encounterid
)
select lab_stk.*, 
       case when lab_stk.LAB_DAYS_SINCE_DX1 <=0 then 0 else 1 end as LAB_POST_DX_IND, 
       count(distinct LAB_DAYS_SINCE_DX1) over (partition by lab_stk.patid, lab_stk.lab_loinc) as LAB_CNT_PER_PAT,
       count(distinct LAB_DAYS_SINCE_DX1) over (partition by lab_stk.patid, lab_stk.lab_loinc, case when lab_stk.LAB_DAYS_SINCE_DX1 <=0 then 0 else 1 end) as LAB_CNT_PER_PAT_PERIOD
from lab_stk
;

select * from ALS_CASE_PLATELET_ALL;

select long_common_name, lab_loinc,
       count(distinct patid) as pat_cnt,
       median(LAB_CNT_PER_PAT) as lab_cnt_per_pat_med
from ALS_CASE_PLATELET_ALL
group by long_common_name, lab_loinc
order by pat_cnt desc
;

select long_common_name,
       LAB_POST_DX_IND,
       count(distinct patid) as pat_cnt,
       median(LAB_CNT_PER_PAT) as lab_cnt_per_pat_med
from ALS_CASE_PLATELET_ALL
group by long_common_name, LAB_POST_DX_IND
order by long_common_name, LAB_POST_DX_IND
;

select * from ALS_CASE_PLATELET_ALL;

select * from ALS_CASE_PLATELET_ALL
where lab_loinc = '48065-7'
order by patid, LAB_DAYS_SINCE_DX1
;


select * from ALS_CASE_PLATELET_ALL
where lab_loinc = '14979-9' and LAB_CNT_PER_PAT > 2
order by patid, LAB_DAYS_SINCE_DX1
;

