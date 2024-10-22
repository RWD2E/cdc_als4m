/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_dx-alg.sql
# Description: simple dx-based computable phenotype for identifying NMD patients, 
*/
/****************************************************************************************************************/
/*set environment*/
use role GROUSE_ROLE_C_ANALYTICS;
use warehouse GROUSE_WH;
use database GROUSE_DEID_ANALYTICS_DB; -- write-premitted database

set cms_cdm_schema = 'GROUSE_DEID_DB.CMS_PCORNET_CDM';
set diagnosis = $cms_cdm_schema || '.V_DEID_DIAGNOSIS';
set demographic = $cms_cdm_schema || '.V_DEID_DEMOGRAPHIC';
set death = $cms_cdm_schema || '.V_DEID_DEATH';
set procedures = $cms_cdm_schema || '.V_DEID_PROCEDURES';
set dispensing = $cms_cdm_schema || '.V_DEID_DISPENSING';
set encounter = $cms_cdm_schema || '.V_DEID_ENCOUNTER';
set enrollment = $cms_cdm_schema || '.V_DEID_ENROLLMENT';
set address_history = $cms_cdm_schema || '.V_DEID_ADDRESS_HISTORY';
set address_geocode = $cms_cdm_schema || '.V_DEID_ADDRESS_GEOCODE';
set obs_comm = $cms_cdm_schema || '.V_DEID_OBS_COMM';

/****************************************************************************************************************/

-------------------------------------------------------------------------------------------------
/* ≥ 1 ALS claims on two different dates from Q1 2011 to Q4 2017*/
CREATE OR REPLACE TABLE ALS_INIT as
with als_stk as (
    SELECT patid, dx_date, enc_type,
           count(distinct dx_date) over (partition by patid) as als_dx_cnt,
           row_number() over (partition by patid order by dx_date) dx_rn
    FROM identifier($diagnosis)
    WHERE (DX like 'G12.21%' OR DX like '335.20%')
)
select patid,
       dx_date as als1dx_date,  
       enc_type as als1dx_enc,
       als_dx_cnt,
       dx_rn
from als_stk 
;

select count(distinct patid) 
from ALS_INIT; 
-- 14,404

select count(distinct patid) 
from ALS_INIT where als_dx_cnt>=2; 
-- 10,852

-------------------------------------------------------------------------------------------------
/* ALS_INIT_ENR; ≥ 1 ALS claims on two different dates + 2 year washup period before first ALS claim */
CREATE OR REPLACE TABLE ALS_INIT_ENR as
with lis_dual_ind as (
    select distinct patid, 1 as ind
    from identifier($ENROLLMENT)
    where raw_basis like 'LIS|%' or raw_basis like 'DUAL|%' 
),   partd_ind as (
    select distinct patid, 1 as ind
    from identifier($ENROLLMENT)
    where raw_basis like 'D|%' 
)
select d.patid
      ,d.als1dx_date
      ,d.als1dx_enc
      ,e.enr_start_date
      ,e.enr_end_date
      ,round(datediff(day,e.enr_start_date,e.enr_end_date)/365.25,2) as enr_duration
      ,round(datediff(day,e.enr_start_date,d.als1dx_date)/365.25,2) as enr_duration_prior_ALS
      ,NVL(ld.ind,0) as LIS_DUAL_IND
      ,NVL(pd.ind,0) as PARTD_IND
from ALS_INIT d
join identifier($ENROLLMENT) e on d.patid = e.patid and d.als_dx_cnt >= 2 and d.dx_rn = 1
left join lis_dual_ind ld on ld.patid = d.patid
left join partd_ind pd on pd.patid = d.patid
where enr_duration_prior_ALS >= 2 and e.RAW_BASIS like 'AB%' -- 2-year washup
      and 
      enr_end_date >= d.als1dx_date -- exclude disconnect enrollment period
;

select count(distinct patid), count(*) 
from ALS_INIT_ENR; 
--2,872

-------------------------------------------------------------------------------------------------
/*NPI specialty reference table*/
create or replace table NPI_TAXONOMY as
select n.NPI,
       t.PROVIDER_TAXONOMY_CODE, 
       n.ENTITY_TYPE_CODE,
       listagg(t.MEDICARE_PROVIDER_TYPE_DESCRIPTION,';') within group (order by t.MEDICARE_SPECIALTY_CODE) as MEDICARE_PROVIDER_TYPE_DESCRIPTION
from nppes_npi_registry.nppes_feb.npidata n 
join nppes_npi_registry.nppes_feb.npi_taxonomy t
on n.HEALTHCARE_PROVIDER_TAXONOMY_CODE_1 = t.PROVIDER_TAXONOMY_CODE
group by n.NPI, t.PROVIDER_TAXONOMY_CODE, n.entity_type_code
;

select * from NPI_TAXONOMY;
