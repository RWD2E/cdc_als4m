/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case-omop.sql
# Description: multifactorial computable phenotype for identifying ALS patients (CDC alg)
# Dependency: OMOP CDM
*/

/* stage the ./ref/als_cde.csv reference table, name it REF_ALS_CDE*/
select * from REF_ALS_CDE limit 5;

/* check other dependency table status */
select distinct vocabulary_id,vocabulary_concept_id 
from OMOP_CDM.CDM.VOCABULARY;

select * from OMOP_CDM.CDM.VOCABULARY 
where vocabulary_name like '%ICD%';

select * from OMOP_CDM.CDM.CONCEPT 
where vocabulary_id like '%ICD%';

select * from OMOP_CDM.CDM.DRUG_EXPOSURE limit 5;
select * from OMOP_CDM.CDM.CONDITION_OCCURRENCE limit 5;
select * from OMOP_CDM.CDM.PROCEDURE_OCCURRENCE limit 5;
select * from OMOP_CDM.CDM.PERSON limit 5;

select * from OMOP_CDM.CDM.VISIT_OCCURRENCE limit 5;
select * from OMOP_CDM.CDM.VISIT_OCCURRENCE where care_site_id is not null limit 5;
select * from OMOP_CDM.CDM.CARE_SITE limit 5;
select * from OMOP_CDM.CDM.CARE_SITE where care_site_name is not null limit 5;
select distinct care_site_name from OMOP_CDM.CDM.CARE_SITE
where lower(care_site_name) like '%neuro%';
select * from OMOP_CDM.CDM.PROVIDER limit 5;
select * from OMOP_CDM.CDM.PROVIDER where specialty_concept_id is not null limit 5;

/* stage rx mapping tables */
create or replace table OMOP_CONCEPT as 
select voc.*, ref.
from 
where
union 
select voc.*, ref.
from 
where
;



/*
apply CDC alg. and identify ALS cases (1 pat/row)
- ICD9: 335.20; ICD10: G12.21
- visit to neurologist
- use of riluzole or endaravone or tofersen
- covered by CMS when age under 65 yo
*/
create or replace table ALS_EVENT_LONG as
;

create or replace table ALS_CASE_TABLE1 as
with cte_ord as (
    select a.patid, 
           a.event_type,
           a.event_date,
           a.age_at_event,
           a.event_src,
           count(distinct a.event_type) over (partition by a.patid) as distinct_event_cnt,
           count(distinct a.event_date) over (partition by a.patid) as distinct_date_cnt,
           listagg(distinct a.event_type, '|') within group (order by a.event_type) over (partition by a.patid) as event_str,
           max(case when b.chart = 'Y' then 1 else 0 end) over (partition by a.patid) as complt_ind,
           max(case when b.enr_start_date is null then 1 else 0 end) over (partition by a.patid) as ehr_ind,
           row_number() over (partition by a.patid order by a.event_date) as rn
    from ALS_EVENT_LONG a
    left join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b 
    on a.patid = b.patid
), cte_dx1 as(
    select patid,
           min(event_date) as als1dx_date
    from ALS_EVENT_LONG
    where event_type = 'DX'
    group by patid
)
select a.patid 
      ,b.birth_date
      ,b.sex
      ,b.race 
      ,b.hispanic
      ,c.als1dx_date
      ,a.event_type as index_event
      ,case when a.event_type = 'NEUROLOGIST' then c.als1dx_date else a.event_date end as index_date
      ,case when a.event_type = 'NEUROLOGIST' then round(datediff(day,b.birth_date,c.als1dx_date)/365.25) else a.age_at_event end as age_at_index
      ,a.event_src as index_src
      ,a.distinct_event_cnt
      ,a.distinct_date_cnt
      ,a.event_str
      ,case when a.complt_ind = 1 then 'complete'
            when a.complt_ind = 0 and a.ehr_ind = 1 then 'enr_only'
            else 'cms_only'
       end as complt_flag
      ,case when a.distinct_event_cnt > 2 then 'confirmed'
            else 'likely'
       end as case_assert 
from cte_ord a 
join PAT_TABLE1 b on a.patid = b.patid
join cte_dx1 c on a.patid = c.patid
where a.rn = 1 and b.birth_date is not null
      and a.distinct_date_cnt > 1 
      and (a.event_str like '%DX%' and a.event_str <> 'DX') -- at least likely, only 1 DX is considered "undetermined"
;


/* 
identify control patients (1 pat/row): 
1. either with similar symptoms, or
2. took EMG or Nerve conduction test, or 
3. diagnosed with other neuromuscular and muscularskelenton conditions 
*/
create or replace table NONALS_EVENT_LONG as
;

create or replace table ALS_CTRL_TABLE1 as
from NONALS_EVENT_LONG
;



/*
ALS_CASE_TABLE1, and ALS_CTRL_TABLE1 will be loaded into R for downstream analyses: 
- ./src/R/extract.R: extract final tables into R and save as rdata files
- ./src/R/explore.R: generate descriptive reports
- ./src/R/pec.R: a positive exposure control test on Riluzole effectiveness
- ./src/R/path2dx.R: describing the average pathways to first ALS dx
*/



