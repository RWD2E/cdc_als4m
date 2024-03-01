/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_sdoh.sql
# Description: extract other covariates for ALS case cohort
*/
create or replace table ALS_ALL_SDOH as
-- census tract level 
select distinct
       a.patid, 
       b.address_period_start,
       datediff(day,a.index_date,b.address_period_start) as days_since_index,
       b.address_period_end,
       d.obscomm_result_text,
       d.obscomm_result_num, 
       d.obscomm_code,
       c.geo_accuracy,
       d.raw_obscomm_name
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY b on a.patid = b.patid
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_GEOCODE c on b.addressid = c.addressid 
join GROUSE_DB.CMS_PCORNET_CDM.LDS_OBS_COMM d on d.obscomm_geocodeid = c.geocode_tract and d.obscomm_geo_accuracy = 'TR'
union
-- census block group level
select distinct
       a.patid, 
       b.address_period_start,
       datediff(day,a.index_date,b.address_period_start) as days_since_index,
       b.address_period_end,
       d.obscomm_result_text,
       d.obscomm_result_num, 
       d.obscomm_code,
       c.geo_accuracy,
       d.raw_obscomm_name
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY b on a.patid = b.patid
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_GEOCODE c on b.addressid = c.addressid 
join GROUSE_DB.CMS_PCORNET_CDM.LDS_OBS_COMM d on d.obscomm_geocodeid = c.geocode_group and d.obscomm_geo_accuracy = 'BG'
;

INSERT INTO ALS_ALL_SDOH(patid,address_period_start,address_period_end,days_since_index,obscomm_result_num,obscomm_code,geo_accuracy) 
select distinct
       a.patid, 
       b.enr_start_date as address_period_start,
       b.enr_end_date as address_period_end,
       datediff(day,a.index_date,b.enr_start_date) as days_since_index,
       1 as obscomm_result_num,
       'LIS_DUAL' as obscomm_code,
       'PS' as geo_accuracy
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b
on a.patid = b.patid
where b.raw_basis in ('LIS','DUAL')
;

INSERT INTO ALS_ALL_SDOH(patid,address_period_start,address_period_end,days_since_index,obscomm_result_num,obscomm_code,geo_accuracy) 
select distinct
       a.patid, 
       b.enr_start_date as address_period_start,
       b.enr_end_date as address_period_end,
       datediff(day,a.index_date,b.enr_start_date) as days_since_index,
       1 as obscomm_result_num,
       'PART_C' as obscomm_code,
       'PS' as geo_accuracy
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b
on a.patid = b.patid
where b.raw_basis = 'C'
;

INSERT INTO ALS_ALL_SDOH(patid,address_period_start,address_period_end,days_since_index,obscomm_result_num,obscomm_code,geo_accuracy) 
select distinct 
       a.patid, 
       b.enr_start_date as address_period_start,
       b.enr_end_date as address_period_end,
       datediff(day,a.index_date,b.enr_start_date) as days_since_index,
       1 as obscomm_result_num,
       'PART_D' as obscomm_code,
       'PS' as geo_accuracy
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b
on a.patid = b.patid
where b.enr_basis = 'D'
;

select count(distinct patid), count(distinct obscomm_result_text) 
from ALS_ALL_SDOH;
-- 10,567

select * from ALS_ALL_SDOH limit 5;

select obscomm_code, count(distinct patid)
from ALS_ALL_SDOH
group by obscomm_code;

create or replace table ALS_SEL_SDOH as
with cte_stk as (
       select distinct
              PATID,
              OBSCOMM_CODE,
              'Y' as OBSCOMM_RESULT,
              address_period_start as OBSCOMM_DATE,
              DAYS_SINCE_INDEX
       from ALS_ALL_SDOH
       where (
              OBSCOMM_CODE in (
                     'LIS_DUAL'
              ) or 
              OBSCOMM_CODE like 'PART%'
       ) and 
       OBSCOMM_RESULT_NUM = 1
       union
       select distinct
              PATID,
              OBSCOMM_CODE,
              OBSCOMM_RESULT_TEXT as OBSCOMM_RESULT,
              address_period_start as OBSCOMM_DATE,
              DAYS_SINCE_INDEX
       from ALS_ALL_SDOH
       where OBSCOMM_CODE like 'ADI%'
       union
       select distinct
              PATID,
              OBSCOMM_CODE,
              case when OBSCOMM_RESULT_TEXT in ('1','2','3') then 'metro'
              when OBSCOMM_RESULT_TEXT in ('4','5','6') then 'micro'
              when OBSCOMM_RESULT_TEXT in ('7','8','9','10') then 'small'
              end as OBSCOMM_RESULT,
              address_period_start as OBSCOMM_DATE,
              DAYS_SINCE_INDEX
       from ALS_ALL_SDOH
       where OBSCOMM_CODE = 'RUCA|PRIMARY'
)
select distinct
       PATID,
       OBSCOMM_CODE,
       OBSCOMM_RESULT,
       OBSCOMM_DATE,
       DAYS_SINCE_INDEX
from cte_stk
;

select * from ALS_ALL_SDOH limit 5;