/*
Descriptions: 
Author: 
Date: 
*/

/*set environment*/
use role GROUSE_ROlE_B_ADMIN;
use warehouse GROUSE_MEDIUM_WH;
use database GROUSE_DB;
use schema ALS;

set cms_cdm_schema = 'CMS_PCORNET_CDM';
set diagnosis = $cms_cdm_schema || '.DIAGNOSIS';
set demographic = $cms_cdm_schema || '.DEMOGRAPHIC';
set death = $cms_cdm_schema || '.DEATH';
set procedures = $cms_cdm_schema || '.PROCEDURES';
set dispensing = $cms_cdm_schema || '.DISPENSING';
set encounter = $cms_cdm_schema || '.ENCOUNTER';
set enrollment = $cms_cdm_schema || '.ENROLLMENT';


/*identify potential control group - as broad as possible?
- at least 2-full year AB enrollment
*/

create or replace table ALS_CTRL_DEMO as
with coc_yr as (
    select patid, ENR_START_DATE, ENR_END_DATE, 
           floor(datediff(day,ENR_START_DATE,ENR_END_DATE)/365.25) as ENR_DURATION,
           row_number() over (partition by patid order by datediff(day,ENR_START_DATE,ENR_END_DATE) desc) AS rn
    from identifier($enrollment)
    where RAW_BASIS like 'AB%' and floor(datediff(day,ENR_START_DATE,ENR_END_DATE)/365.25) >= 2
), orec_agg as (
    select bene_id, orec, row_number() over (partition by bene_id order by rfrnc_yr desc) AS rn
    from cms_pcornet_cdm_staging.private_enrollment_stage_ab
),   lis_dual_ind as (
    select distinct patid, 1 as ind
    from CMS_PCORNET_CDM_BACKUP.ENROLLMENT
    where raw_basis like 'LIS|%' or raw_basis like 'DUAL|%' 
),   partd_ind as (
    select distinct patid, 1 as ind
    from CMS_PCORNET_CDM_BACKUP.ENROLLMENT
    where raw_basis like 'D|%' 
)
select d.*
      ,coc_yr.enr_start_date
      ,coc_yr.enr_end_date
      ,coc_yr.enr_duration
      ,s.orec
      ,NVL(ld.ind,0) as LIS_DUAL_IND
      ,NVL(pd.ind,0) as PARTD_IND 
from identifier($demographic) d
join coc_yr on coc_yr.patid = d.patid and coc_yr.rn = 1
join orec_agg s on s.bene_id = d.patid and s.rn = 1
left join lis_dual_ind ld on ld.patid = d.patid
left join partd_ind pd on pd.patid = d.patid
where not exists (select 1 from ALS_ONE_CLAIM oc where oc.patid = d.patid) 
;

select count(distinct patid), count(*) from ALS_CTRL_DEMO; --6,960,698

/* ALL_DIAGNOSIS_BEFORE_ALS1DX: collect all diagnosis codes prior to censor*/
create or replace table ALL_DIAGNOSIS_BEFORE_CENSOR as
with dx_cte as (
    select a.PATID
          ,a.ENR_START_DATE
          ,a.ENR_END_DATE
          ,a.ENR_DURATION
          ,datediff(day,a.ENR_START_DATE,NVL(d.DX_DATE,d.ADMIT_DATE)) as DAYS_SINCE_ALS1DX -- >=0
          ,d.DX
          ,d.DX_TYPE
          ,d.PDX
          ,d.DX_DATE
          ,d.ADMIT_DATE
          ,d.ENC_TYPE
          ,row_number() over (partition by a.PATID,d.DX order by DAYS_SINCE_ALS1DX) dx_rn
    from ALS_CTRL_DEMO a
    join identifier($DIAGNOSIS) d on a.PATID = d.PATID
    where NVL(d.DX_DATE,d.ADMIT_DATE) between a.ENR_START_DATE and a.ENR_END_DATE
)
select cte.*,
       NVL(pcx."phecode",'NA') as phecode
from dx_cte cte 
left join grouper_valuesets.icd10cm_phecodex pcx on pcx."icd10" = cte.dx 
where cte.dx_type = '10' and cte.dx_rn = 1
union
select cte.*,
       NVL(pcx."phecode",'NA') as phecode
from dx_cte cte 
left join grouper_valuesets.icd9cm_phecodex pcx on pcx."icd9" = cte.dx 
where cte.dx_type = '09' and cte.dx_rn = 1           
;


create or replace table ALL_REC_PHECODES_BEFORE_CENSOR as
with cte as (
    select a.*, 
           row_number() over (partition by patid,phecode order by DAYS_SINCE_ALS1DX) as phecode_rn
    from ALL_DIAGNOSIS_BEFORE_CENSOR a
)
select patid,
       dx,
       dx_type,
       phecode,
       days_since_als1dx,
       dx_date,
       admit_date,
       enc_type
from cte
where phecode_rn = 1;

select count(distinct phecode) from ALL_REC_PHECODES_BEFORE_ALS1DX;



/* ALL_PROCEDURES_BEFORE_ALS1DX: collect all procedure codes prior to censor*/
create or replace table ALL_PROCEDURES_BEFORE_CENSOR as
select a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day,a.ENR_START_DATE,NVL(p.PX_DATE,p.ADMIT_DATE)) as DAYS_SINCE_ALS1DX -- >=0
      ,p.PX
      ,p.PX_TYPE
      ,p.PPX
      ,p.PX_DATE
      ,p.ADMIT_DATE
      ,p.ENC_TYPE
from ALS_CTRL_DEMO a
join identifier($PROCEDURES) p on a.PATID = p.PATID
where NVL(p.PX_DATE,p.ADMIT_DATE) between a.ENR_START_DATE and a.ENR_END_DATE
;


/* ALL_DISPENSING_BEFORE_ALS1DX: collect all part D events prior to censor*/
create or replace table ALL_DISPENSING_BEFORE_CENSOR as
with ptd_coc_yr as (
    select e.patid, e.ENR_START_DATE, e.ENR_END_DATE, 
           floor(datediff(day,e.ENR_START_DATE,e.ENR_END_DATE)/365.25) as ENR_DURATION,
           row_number() over (partition by e.patid order by datediff(day,e.ENR_START_DATE,e.ENR_END_DATE) desc) AS rn
    from identifier($enrollment) e
    join ALS_CTRL_DEMO a on a.patid = e.patid
    where e.ENR_BASIS = 'D' and floor(datediff(day,e.ENR_START_DATE,e.ENR_END_DATE)/365.25) >= 2
)
select a.PATID
      ,a.ENR_START_DATE
      ,a.ENR_END_DATE
      ,a.ENR_DURATION
      ,datediff(day, a.ENR_START_DATE, d.srvc_dt) as DAYS_SINCE_ALS1DX -- >=0
      ,d.prdsrvid as NDC
      ,d.dayssply as DISPENSE_SUP
      ,d.qtydspns as DISPENSE_AMT
      ,d.gcdf AS DISPENSE_ROUTE
      ,d.str AS raw_dispense_dose_disp
      ,d.GNN
from ptd_coc_yr a
-- join identifier($DISPENSING) d on a.PATID = d.PATID
join CMS_PCORNET_CDM_STAGING.PRIVATE_DISPENSING_STAGE d on a.PATID = d.BENE_ID
where d.srvc_dt between a.ENR_START_DATE and a.ENR_END_DATE
;
