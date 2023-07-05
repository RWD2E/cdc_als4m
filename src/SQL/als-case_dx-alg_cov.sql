/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_dx-alg_cov.sql
# Description: extract other covariates for ALS case cohort identified using 
#              simple dx-based computable phenotype with 2-year washup period
*/
/****************************************************************************************************************/
/*set environment*/
use role GROUSE_ROLE_C_ANALYTICS;
use warehouse GROUSE_WH;
use database GROUSE_DEID_ANALYTICS_DB; -- write-premitted database

create schema if not exists ALS_GPC;
use schema ALS_GPC;

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
set als_case_cohort = 'ALS_INIT_ENR';
set npi_taxonomy_ref = 'NPI_TAXONOMY';

/****************************************************************************************************************/
/*demographic table*/
create or replace table ALS_CASE_DEMO as
select e.*,
       d.BIRTH_DATE,
       round(datediff(day,d.BIRTH_DATE,e.als1dx_date)/365.25) as AGE_AT_ALS1DX,
       d.SEX,
       d.RACE,
       d.HISPANIC,
       dth.DEATH_DATE,
       datediff(day,e.als1dx_date,dth.DEATH_DATE) as DAYS_ALS1DX_TO_DEATH,
       datediff(day,e.als1dx_date,e.ENR_END_DATE) as DAYS_ALS1DX_TO_CENSOR
from identifier($als_case_cohort) e
join identifier($DEMOGRAPHIC) d on e.patid = d.patid
left join identifier($DEATH) dth on e.patid = dth.patid
;

-- /* orec not transformed to CDM yet */
-- -- original reason of enrollment
-- select s.orec, count(distinct a.patid)
-- from ALS_CASE_DEMO a
-- join cms_pcornet_cdm_staging.private_enrollment_stage_ab s
-- on a.patid = s.bene_id
-- group by s.orec
-- order by s.orec
-- ;

-- -- 3	11
-- -- 2	12
-- -- 1	664
-- -- 0	2,193


-- with etc as (
--     select patid, to_char(count(distinct address_zip5)) as zip5_change
--     from identifier($lds_address_history)
--     group by patid
--     having count(distinct address_zip5)<10
--     union
--     select patid, '10+' as zip5_change
--     from identifier($lds_address_history)
--     group by patid
--     having count(distinct address_zip5)>=10
-- )
-- select zip5_change, count(distinct patid)
-- from etc
-- group by zip5_change
-- order by zip5_change
-- ;

-- 1	5,532,961
-- 2	3,136,856
-- 3	2,136,797
-- 4	1,333,147
-- 5	761,290
-- 6	413,399
-- 7	216,652
-- 8	112,302
-- 9	58,309
-- 10+	72,538

------------------------------------------------------------------------------------
/*sdoh table*/
create or replace table ALS_CASE_SDOH as
-- census tract level 
select distinct
       a.patid, 
       b.address_period_start,
       datediff(day,b.address_period_start,a.als1dx_date) as address_period_start_als1dx_days,
       b.address_period_end,
       d.obscomm_result_text,
       d.obscomm_result_num, 
       d.obscomm_code,
       c.geo_accuracy,
       d.raw_obscomm_name
from identifier($als_case_cohort) a
join identifier($address_history) b on a.patid = b.patid
join identifier($address_geocode) c on b.addressid = c.addressid 
join identifier($obs_comm) d on d.obscomm_geocodeid = c.geocode_tract and d.obscomm_geo_accuracy = 'TR'
union
-- census block group level
select distinct
       a.patid, 
       b.address_period_start,
       datediff(day,b.address_period_start,a.als1dx_date) as address_period_start_als1dx_days,
       b.address_period_end,
       d.obscomm_result_text,
       d.obscomm_result_num, 
       d.obscomm_code,
       c.geo_accuracy,
       d.raw_obscomm_name
from identifier($als_case_cohort) a
join identifier($address_history) b on a.patid = b.patid
join identifier($address_geocode) c on b.addressid = c.addressid 
join identifier($obs_comm) d on d.obscomm_geocodeid = c.geocode_group and d.obscomm_geo_accuracy = 'BG'
;

select count(distinct obscomm_code) 
from ALS_CASE_SDOH;

select count(distinct patid) 
from ALS_CASE_SDOH;

-------------------------------------------------------------------------------------------
/* ALL_DIAGNOSIS_BEFORE_ALS1DX: collect all diagnosis codes prior to first_ALS_date*/
create or replace table ALL_DIAGNOSIS_BEFORE_ALS1DX as
select a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day,NVL(d.DX_DATE,d.ADMIT_DATE),a.als1dx_date) as DAYS_SINCE_ALS1DX -- >=0
      ,d.DX
      ,d.DX_TYPE
      ,d.PDX
      ,d.DX_DATE
      ,d.ADMIT_DATE
      ,d.ENC_TYPE
      ,d.DIAGNOSISID
      ,d.PROVIDERID
      ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
      ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' then 1 else 0 end as NEUROLOGY_FLAG
from identifier($als_case_cohort) a
join identifier($DIAGNOSIS) d on a.PATID = d.PATID
left join identifier($npi_taxonomy_ref) np on to_char(d.PROVIDERID) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
where NVL(d.DX_DATE,d.ADMIT_DATE) <= a.als1dx_date and
      NVL(d.DX_DATE,d.ADMIT_DATE) between a.ENR_START_DATE and a.ENR_END_DATE        
;

select count(distinct patid) 
from ALL_DIAGNOSIS_BEFORE_ALS1DX
where NEUROLOGY_FLAG = 1; 
-- 2,013

-- map to phecodes
create or replace table ALL_DIAGNOSIS_PHECD_BEFORE_ALS1DX as 
with phecd_map_cte as (
    select distinct dx.*,
           phe."phecode" as phecd_dxgrpcd, 
           ref."phecode_string" as phecd_dxgrp
    from ALL_DIAGNOSIS_BEFORE_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD10CM_PHECODEX phe on dx.DX = phe."icd10" and dx.DX_TYPE = '10'
    join ONTOLOGY.GROUPER_VALUESETS.PHECODEX_REF ref on phe."phecode" = ref."phecode"
    union
    select distinct dx.*,
           phe."phecode" as phecd_dxgrpcd, 
           ref."phecode_string" as phecd_dxgrp
    from ALL_DIAGNOSIS_BEFORE_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD9CM_PHECODEX phe on dx.DX = phe."icd9" and dx.DX_TYPE = '09'
    join ONTOLOGY.GROUPER_VALUESETS.PHECODEX_REF ref on phe."phecode" = ref."phecode"
), phecd_fill_na as (
    select * from phecd_map_cte
    union 
    select dx.*,'00000', 'NI'
    from ALL_DIAGNOSIS_BEFORE_ALS1DX dx
    where not exists (select 1 from phecd_map_cte cte where cte.diagnosisid = dx.diagnosisid) 
)
select a.*,row_number() over (partition by a.patid, a.phecd_dxgrpcd order by a.DAYS_SINCE_ALS1DX) as rec_rn
from phecd_fill_na a
;

select count(distinct phecd_dxgrpcd) 
from ALL_DIAGNOSIS_PHECD_BEFORE_ALS1DX
where rec_rn = 1; 
--2,712

-- map to CCS category
create or replace table ALL_DIAGNOSIS_CCS_BEFORE_ALS1DX as 
with ccs_map_cte as (
    select distinct dx.*,
           replace(ccs.ccs_slvl1,'''','') as ccs_dxgrpcd, 
           ccs.ccs_slvl1label as ccs_dxgrp
    from ALL_DIAGNOSIS_BEFORE_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD10CM_CCS ccs 
    on replace(dx.DX,'.','') = replace(ccs.ICD10CM,'''','') and dx.DX_TYPE = '10'
    union
    select distinct dx.*,
           replace(icd9.ccs_slvl1,'''','') as ccs_dxgrpcd, 
           icd9.ccs_slvl1label as ccs_dxgrp
    from ALL_DIAGNOSIS_BEFORE_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD9DX_CCS icd9 
    on rpad(replace(dx.DX,'.',''),5,'0') = replace(icd9.ICD9,'''','') and dx.DX_TYPE = '09'
), ccs_fill_na as (
    select * from ccs_map_cte
    union 
    select dx.*,'00000', 'NI'
    from ALL_DIAGNOSIS_BEFORE_ALS1DX dx
    where not exists (select 1 from ccs_map_cte cte where cte.diagnosisid = dx.diagnosisid) 
)
select a.*, row_number() over (partition by a.patid, a.ccs_dxgrpcd order by a.DAYS_SINCE_ALS1DX) as rec_rn
from ccs_fill_na a
;

select count(distinct ccs_dxgrpcd) 
from ALL_DIAGNOSIS_CCS_BEFORE_ALS1DX
where rec_rn = 1; --496

---------------------------------------------------------------------------------------------------
/* ALL_PROCEDURES_BEFORE_ALS1DX: collect all procedure codes prior to first_ALS_date*/
create or replace table ALL_PROCEDURES_BEFORE_ALS1DX as
with px_cte as (
    select a.PATID
          ,a.ENR_START_DATE
          ,a.ENR_END_DATE
          ,a.ENR_DURATION
          ,datediff(day,NVL(p.PX_DATE,p.ADMIT_DATE),a.als1dx_date) as DAYS_SINCE_ALS1DX -- >=0
          ,p.PX
          ,p.PX_TYPE
          ,p.PPX
          ,p.PX_DATE
          ,p.ADMIT_DATE
          ,p.ENC_TYPE
          ,p.PROCEDURESID
          ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
          ,case when p.PX in ('99201','99202','99203','99204','99205',
                              '99206','99207','99208','99209','99210',
                              '99211','99212','99213','99214','99215')  then 1 
           else 0 end as OFFICE_FLAG
          ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' and
                     p.PX in ('99201','99202','99203','99204','99205',
                              '99206','99207','99208','99209','99210',
                              '99211','99212','99213','99214','99215') then 1 
                else 0 end as NEUROLOGY_FLAG
    from identifier($als_case_cohort) a
    join identifier($PROCEDURES) p on a.PATID = p.PATID
    left join identifier($npi_taxonomy_ref) np on to_char(p.PROVIDERID) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
    where NVL(p.PX_DATE,p.ADMIT_DATE) <= a.als1dx_date and
          NVL(p.PX_DATE,p.ADMIT_DATE) between a.ENR_START_DATE and a.ENR_END_DATE
), px_grp as (
    select px_cte.*, a.ccslvl::varchar as px_grpcd, a.ccslvl_label as px_grp
    from px_cte join ONTOLOGY.GROUPER_VALUESETS.CPT_CCS a 
    on to_double(px_cte.PX) between to_double(a.cpt_lb) and to_double(cpt_ub) and px_cte.PX_TYPE = 'CH' and regexp_like(px_cte.PX,'^[[:digit:]]+$') and regexp_like(a.cpt_lb,'^[[:digit:]]+$')
    union 
    select px_cte.*, a.ccslvl::varchar as px_grpcd, a.ccslvl_label as px_grp
    from px_cte join ONTOLOGY.GROUPER_VALUESETS.CPT_CCS a 
    on px_cte.PX = a.cpt_lb and px_cte.PX_TYPE = 'CH' and not regexp_like(a.cpt_lb,'^[[:digit:]]+$')
    union
    select px_cte.*, replace(b.ccs_slvl1,'''','') as px_grpcd, b.ccs_slvl1label as px_grp
    from px_cte join ONTOLOGY.GROUPER_VALUESETS.ICD9PX_CCS b 
    on replace(px_cte.PX,'.','') = replace(b.ICD9,'''','') and px_cte.PX_TYPE = '09'
    -- union 
    -- select px_cte.*, replace(c.ccsr,'''','') as px_grpcd, c.ccsr_label as px_grp
    -- from px_cte join GROUPER_VALUESETS.ICD10PCS_CCS c 
    -- on replace(px_cte.PX,'.','') = replace(c.ICD10PCS,'''','')  and px_cte.PX_TYPE = '10'
)
select * from px_grp
union 
select px_cte.*,'00000','NI' from px_cte
where not exists (select 1 from px_grp where px_grp.proceduresid = px_cte.proceduresid) and
      px_cte.PX_TYPE <> 'RE'
;

select count(distinct patid) 
from ALL_PROCEDURES_BEFORE_ALS1DX
where NEUROLOGY_FLAG = 1; --2,067

select px_grpcd, px_grp, count(distinct patid)
from ALL_PROCEDURES_BEFORE_ALS1DX
group by px_grpcd, px_grp
;

----------------------------------------------------------------------------------------
/* ALL_DISPENSING_BEFORE_ALS1DX: collect all part D events prior to first_ALS_date*/
create or replace table ALL_DISPENSING_BEFORE_ALS1DX as
with ptd_coc_yr as (
    select e.patid, e.ENR_START_DATE, e.ENR_END_DATE, a.als1dx_date,
           floor(datediff(day,e.ENR_START_DATE,e.ENR_END_DATE)/365.25) as ENR_DURATION,
           row_number() over (partition by e.patid order by datediff(day,e.ENR_START_DATE,e.ENR_END_DATE) desc) AS rn
    from identifier($enrollment) e
    join identifier($als_case_cohort) a on a.patid = e.patid
    where e.ENR_BASIS = 'D' and floor(datediff(day,e.ENR_START_DATE,e.ENR_END_DATE)/365.25) >= 2
)
select a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day,d.dispense_date,a.als1dx_date) as DAYS_SINCE_ALS1DX -- >=0
      ,d.ndc
      ,d.DISPENSE_SUP
      ,d.DISPENSE_AMT
      ,d.DISPENSE_ROUTE
      ,d.dispense_dose_disp
      ,d.dispense_dose_disp_unit
      ,d.raw_rx_med_name
      ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
      ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' then 1 else 0 end as NEUROLOGY_FLAG
from ptd_coc_yr a
join identifier($DISPENSING) d on a.PATID = d.PATID
left join identifier($npi_taxonomy_ref) np on to_char(d.prescriberid) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
where d.dispense_date <= a.als1dx_date
;

select count(distinct patid) 
from ALL_DISPENSING_BEFORE_ALS1DX
where NEUROLOGY_FLAG = 1;
--599

-------------------------------------------------------------------------------------------
/* ALL_ENCOUNTER_BEFORE_ALS1DX: collect all encounters prior to first_ALS_date*/
create or replace table ALL_ENCOUNTER_BEFORE_ALS1DX as
select distinct
       a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day,e.ADMIT_DATE,a.als1dx_date) as DAYS_SINCE_ALS1DX -- >=0
      ,e.ENC_TYPE
      ,e.ADMITTING_SOURCE
      ,e.ADMIT_DATE
      ,e.DISCHARGE_DATE
      ,e.DISCHARGE_STATUS
      ,e.DISCHARGE_DISPOSITION
      ,e.FACILITY_TYPE
      ,e.FACILITYID
      ,no.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_ORG_SPECIALTY
      ,e.PROVIDERID
      ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
      ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' then 1 else 0 end as NEUROLOGY_FLAG
from identifier($als_case_cohort) a
join identifier($ENCOUNTER) e on a.PATID = e.PATID
left join identifier($npi_taxonomy_ref) no on to_char(e.FACILITYID) = to_char(no.NPI) and no.ENTITY_TYPE_CODE = 2
left join identifier($npi_taxonomy_ref) np on to_char(e.PROVIDERID) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
where e.ADMIT_DATE <= a.als1dx_date and
      e.ADMIT_DATE between a.ENR_START_DATE and a.ENR_END_DATE
;

select count(distinct patid) 
from ALL_ENCOUNTER_BEFORE_ALS1DX
where NEUROLOGY_FLAG = 1;
-- 2,039

--------------------------------------------------------------------------------------
/* ALL_DIAGNOSIS_AFTER_ALS1DX: collect all diagnosis codes after first_ALS_date*/
create or replace table ALL_DIAGNOSIS_AFTER_ALS1DX as
select a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day,a.als1dx_date,NVL(d.DX_DATE,d.ADMIT_DATE)) as DAYS_SINCE_ALS1DX -- > 0
      ,d.DX
      ,d.DX_TYPE
      ,d.PDX
      ,d.DX_DATE
      ,d.ADMIT_DATE
      ,d.ENC_TYPE
      ,d.DIAGNOSISID
      ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
      ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' then 1 else 0 end as NEUROLOGY_FLAG
from identifier($als_case_cohort) a
join identifier($DIAGNOSIS) d on a.PATID = d.PATID
left join identifier($npi_taxonomy_ref) np on to_char(d.PROVIDERID) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
where NVL(d.DX_DATE,d.ADMIT_DATE) > a.als1dx_date and
      NVL(d.DX_DATE,d.ADMIT_DATE) between a.ENR_START_DATE and a.ENR_END_DATE    
;

select count(distinct patid) 
from ALL_DIAGNOSIS_AFTER_ALS1DX
where NEUROLOGY_FLAG = 1;
-- 1,869

-- map to phecodes
create or replace table ALL_DIAGNOSIS_PHECD_AFTER_ALS1DX as 
with phecd_map_cte as (
    select distinct dx.*,
           phe."phecode" as phecd_dxgrpcd, 
           ref."phecode_string" as phecd_dxgrp
    from ALL_DIAGNOSIS_AFTER_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD10CM_PHECODEX phe on dx.DX = phe."icd10" and dx.DX_TYPE = '10'
    join ONTOLOGY.GROUPER_VALUESETS.PHECODEX_REF ref on phe."phecode" = ref."phecode"
    union
    select distinct dx.*,
           phe."phecode" as phecd_dxgrpcd, 
           ref."phecode_string" as phecd_dxgrp
    from ALL_DIAGNOSIS_AFTER_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD9CM_PHECODEX phe on dx.DX = phe."icd9" and dx.DX_TYPE = '09'
    join ONTOLOGY.GROUPER_VALUESETS.PHECODEX_REF ref on phe."phecode" = ref."phecode"
), phecd_fill_na as(
    select * from phecd_map_cte
    union 
    select dx.*,'00000', 'NI'
    from ALL_DIAGNOSIS_AFTER_ALS1DX dx
    where not exists (select 1 from phecd_map_cte cte where cte.diagnosisid = dx.diagnosisid)
)
select a.*, row_number() over (partition by a.patid, a.phecd_dxgrpcd order by a.DAYS_SINCE_ALS1DX) as rec_rn 
from phecd_fill_na a
;

select count(distinct phecd_dxgrpcd) 
from ALL_DIAGNOSIS_PHECD_AFTER_ALS1DX
where rec_rn = 1;
-- 2,475

-- map to ccs categories
create or replace table ALL_DIAGNOSIS_CCS_AFTER_ALS1DX as 
with ccs_map_cte as (
    select distinct dx.*,
           replace(ccs.ccs_slvl1,'''','') as ccs_dxgrpcd, 
           ccs.ccs_slvl1label as ccs_dxgrp
    from ALL_DIAGNOSIS_AFTER_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD10CM_CCS ccs 
    on replace(dx.DX,'.','') = replace(ccs.ICD10CM,'''','') and dx.DX_TYPE = '10'
    union
    select distinct dx.*,
           replace(icd9.ccs_slvl1,'''','') as ccs_dxgrpcd, 
           icd9.ccs_slvl1label as ccs_dxgrp
    from ALL_DIAGNOSIS_AFTER_ALS1DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD9DX_CCS icd9 
    on rpad(replace(dx.DX,'.',''),5,'0') = replace(icd9.ICD9,'''','') and dx.DX_TYPE = '09'
), ccs_fill_na as (
    select * from ccs_map_cte
    union 
    select dx.*,'00000', 'NI'
    from ALL_DIAGNOSIS_AFTER_ALS1DX dx
    where not exists (select 1 from ccs_map_cte cte where cte.diagnosisid = dx.diagnosisid) 
)
select a.*, row_number() over (partition by a.patid, a.ccs_dxgrpcd order by a.DAYS_SINCE_ALS1DX) as rec_rn
from ccs_fill_na a
;

select count(distinct ccs_dxgrpcd) 
from ALL_DIAGNOSIS_CCS_AFTER_ALS1DX
where rec_rn = 1;
-- 477

 -------------------------------------------------------------------------------------       
/* ALL_PROCEDURES_AFTER_ALS1DX: collect all procedure codes after first_ALS_date*/
create or replace table ALL_PROCEDURES_AFTER_ALS1DX as
with px_cte as (
    select a.PATID
          ,a.ENR_START_DATE
          ,a.ENR_END_DATE
          ,a.ENR_DURATION
          ,datediff(day,a.als1dx_date, NVL(p.PX_DATE,p.ADMIT_DATE)) as DAYS_SINCE_ALS1DX -- >0
          ,p.PX
          ,p.PX_TYPE
          ,p.PPX
          ,p.PX_DATE
          ,p.ADMIT_DATE
          ,p.ENC_TYPE
          ,p.PROCEDURESID
          ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
          ,case when p.PX in ('99201','99202','99203','99204','99205',
                              '99206','99207','99208','99209','99210',
                              '99211','99212','99213','99214','99215')  then 1 
           else 0 end as OFFICE_FLAG
          ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' and
                 p.PX in ('99201','99202','99203','99204','99205',
                          '99206','99207','99208','99209','99210',
                          '99211','99212','99213','99214','99215') then 1 
            else 0 end as NEUROLOGY_FLAG
    from identifier($als_case_cohort) a
    join identifier($PROCEDURES) p on a.PATID = p.PATID
    left join identifier($npi_taxonomy_ref) np on to_char(p.PROVIDERID) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
    where NVL(p.PX_DATE,p.ADMIT_DATE) > a.als1dx_date and
          NVL(p.PX_DATE,p.ADMIT_DATE) between a.ENR_START_DATE and a.ENR_END_DATE
), px_grp as (
    select px_cte.*, a.ccslvl::varchar as px_grpcd, a.ccslvl_label as px_grp
    from px_cte join ONTOLOGY.GROUPER_VALUESETS.CPT_CCS a 
    on to_double(px_cte.PX) between to_double(a.cpt_lb) and to_double(cpt_ub) and px_cte.PX_TYPE = 'CH' and regexp_like(px_cte.PX,'^[[:digit:]]+$') and regexp_like(a.cpt_lb,'^[[:digit:]]+$')
    union 
    select px_cte.*, a.ccslvl::varchar as px_grpcd, a.ccslvl_label as px_grp
    from px_cte join ONTOLOGY.GROUPER_VALUESETS.CPT_CCS a 
    on px_cte.PX = a.cpt_lb and px_cte.PX_TYPE = 'CH' and not regexp_like(a.cpt_lb,'^[[:digit:]]+$')
    union
    select px_cte.*, replace(b.ccs_slvl1,'''','') as px_grpcd, b.ccs_slvl1label as px_grp
    from px_cte join ONTOLOGY.GROUPER_VALUESETS.ICD9PX_CCS b 
    on replace(px_cte.PX,'.','') = replace(b.ICD9,'''','') and px_cte.PX_TYPE = '09'
    -- union 
    -- select px_cte.*, replace(c.ccsr,'''','') as px_grpcd, c.ccsr_label as px_grp
    -- from px_cte join GROUPER_VALUESETS.ICD10PCS_CCS c 
    -- on replace(px_cte.PX,'.','') = replace(c.ICD10PCS,'''','')  and px_cte.PX_TYPE = '10'
)
select * from px_grp
union 
select px_cte.*,'00000','NI' from px_cte
where not exists (select 1 from px_grp where px_grp.proceduresid = px_cte.proceduresid) and
      px_cte.PX_TYPE <> 'RE'
;

select count(distinct patid) 
from ALL_PROCEDURES_AFTER_ALS1DX
where NEUROLOGY_FLAG = 1; 
-- 1,712

select px_grpcd,listagg(distinct px_grp,', ') within group (order by px_grp),
       count(distinct patid) as pat_cnt, count(*) as obs_cnt 
from ALL_PROCEDURES_AFTER_ALS1DX
group by px_grpcd
order by pat_cnt desc;


select PROVIDER_SPECIALTY, count(distinct patid) as pat_cnt
from ALL_PROCEDURES_AFTER_ALS1DX
where office_flag = 1
group by PROVIDER_SPECIALTY
order by pat_cnt desc;

select PROVIDER_SPECIALTY, count(distinct patid) as pat_cnt
from ALL_PROCEDURES_AFTER_ALS1DX
group by PROVIDER_SPECIALTY
order by pat_cnt desc;


select PROVIDER_SPECIALTY, count(distinct patid) as pat_cnt
from ALL_ENCOUNTER_AFTER_ALS1DX
group by PROVIDER_SPECIALTY
order by pat_cnt desc;

select PROVIDER_SPECIALTY, count(distinct patid) as pat_cnt
from ALL_DISPENSING_AFTER_ALS1DX
group by PROVIDER_SPECIALTY
order by pat_cnt desc;


select px_grp, PROVIDER_SPECIALTY, count(distinct patid) as pat_cnt
from ALL_PROCEDURES_AFTER_ALS1DX
group by px_grp, PROVIDER_SPECIALTY
order by pat_cnt desc;


-- select * from ALL_PROCEDURES_AFTER_ALS1DX
-- where PROVIDER_SPECIALTY is null;

---------------------------------------------------------------------------------------
/* ALL_DISPENSING_AFTER_ALS1DX: collect all part D events after first_ALS_date*/

create or replace table ALL_DISPENSING_AFTER_ALS1DX as
with ptd_coc_yr as (
    select e.patid, e.ENR_START_DATE, e.ENR_END_DATE, a.als1dx_date,
           floor(datediff(day,e.ENR_START_DATE,e.ENR_END_DATE)/365.25) as ENR_DURATION,
           row_number() over (partition by e.patid order by datediff(day,e.ENR_START_DATE,e.ENR_END_DATE) desc) AS rn
    from identifier($enrollment) e
    join identifier($als_case_cohort) a on a.patid = e.patid
    where e.ENR_BASIS = 'D'
)
select a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day,a.als1dx_date,d.dispense_date) as DAYS_SINCE_ALS1DX -- >0
      ,d.NDC
      ,d.DISPENSE_SUP
      ,d.DISPENSE_AMT
      ,d.DISPENSE_ROUTE
      ,d.dispense_dose_disp
      ,d.dispense_dose_disp_unit
      ,d.raw_rx_med_name
      ,rxnsat.RXCUI
      ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
      ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' then 1 else 0 end as NEUROLOGY_FLAG
from ptd_coc_yr a
join identifier($DISPENSING) d on a.PATID = d.PATID
left join ontology.rxnorm.rxnsat rxnsat on rxnsat.ATV = d.NDC and rxnsat.ATN = 'NDC'
left join identifier($npi_taxonomy_ref) np on to_char(d.prescriberid) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
where d.dispense_date > a.als1dx_date
;

select count(distinct patid) 
from ALL_DISPENSING_AFTER_ALS1DX
where NEUROLOGY_FLAG = 1;

--861

-----------------------------------------------------------------------------------
/* ALL_ENCOUNTER_AFTER_ALS1DX: collect all encounters after first_ALS_date*/
create or replace table ALL_ENCOUNTER_AFTER_ALS1DX as
select distinct
       a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day,a.als1dx_date,e.ADMIT_DATE) as DAYS_SINCE_ALS1DX -- >0
      ,e.ENC_TYPE
      ,e.ADMITTING_SOURCE
      ,e.ADMIT_DATE
      ,e.DISCHARGE_DATE
      ,e.DISCHARGE_STATUS
      ,e.DISCHARGE_DISPOSITION
      ,e.FACILITY_TYPE
      ,e.FACILITYID
      ,no.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_ORG_SPECIALTY
      ,e.PROVIDERID
      ,np.MEDICARE_PROVIDER_TYPE_DESCRIPTION as PROVIDER_SPECIALTY
      ,case when lower(np.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%' then 1 else 0 end as NEUROLOGY_FLAG
from identifier($als_case_cohort) a
join identifier($ENCOUNTER) e on a.PATID = e.PATID
left join identifier($npi_taxonomy_ref) no on to_char(e.FACILITYID) = to_char(no.NPI) and no.ENTITY_TYPE_CODE = 2
left join identifier($npi_taxonomy_ref) np on to_char(e.PROVIDERID) = to_char(np.NPI) and np.ENTITY_TYPE_CODE = 1
where e.ADMIT_DATE > a.als1dx_date and
      e.ADMIT_DATE between a.ENR_START_DATE and a.ENR_END_DATE
;

select count(distinct patid) 
from ALL_ENCOUNTER_AFTER_ALS1DX
where NEUROLOGY_FLAG = 1;


show integrations;
desc integration GROUSE_S3_EXT_INT;