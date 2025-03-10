/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case-omop.sql
# Description: multifactorial computable phenotype for identifying ALS patients (CDC alg)
# Dependency: 
# - REF_ALS_CDE: generated by staging the ./ref/als_cde.csv reference table  
# - Adequately complete data populated in the following OMOP CDM Tables
# -   CONCEPT
# -   PERSON
# -   CONDITION_OCCURRENCE
# -   DRUG_EXPOSURE and/or DRUG_ERA
# -   VISIT_OCCURRENCE
# -   CARE_SITE
# -   PROVIDER
# -   PROCEDURE_OCCURRENCE
# -   PAYER_PLAN_PERIOD
*/

select * from REF_ALS_CDE where name in ('NeurologyVisit');

/* check other dependency table status */
set path_to_omop = 'OMOP_CDM.CDM';
set omop_vocabulary = $path_to_omop || '.VOCABULARY';
set omop_concept = $path_to_omop || '.CONCEPT';
set omop_person = $path_to_omop || '.PERSON';
set omop_condition_occurrence= $path_to_omop || '.CONDITION_OCCURRENCE';
set omop_drug_exposure = $path_to_omop || '.DRUG_EXPOSURE';
set omop_drug_era = $path_to_omop || '.DRUG_ERA';
set omop_visit_occurrence= $path_to_omop || '.VISIT_OCCURRENCE';
set omop_care_site= $path_to_omop || '.CARE_SITE';
set omop_provider= $path_to_omop || '.PROVIDER';
set omop_procedure_occurrence= $path_to_omop || '.PROCEDURE_OCCURRENCE';
set omop_payer_plan_period= $path_to_omop || '.PAYER_PLAN_PERIOD';

select * from identifier($omop_vocabulary);
select * from identifier($omop_concept) limit 5;
select * from identifier($omop_person) limit 5;
select * from identifier($omop_condition_occurrence) where condition_source_value in ('335.20','G12.21') limit 5;
select * from identifier($omop_drug_exposure) limit 5;
select * from identifier($omop_drug_era) limit 5;
select * from identifier($omop_visit_occurrence) limit 5;
select * from identifier($omop_care_site) limit 5;
select * from identifier($omop_provider) limit 5;
select * from identifier($omop_procedure_occurrence) limit 5;
select * from identifier($omop_payer_plan_period) limit 5;


/*
apply CDC alg. and identify ALS cases
- ICD9: 335.20; ICD10: G12.21
- visit to neurologist
- use of riluzole or endaravone or tofersen
- covered by Medicare when age under 65 yo
*/
create or replace table ALS_EVENT_LONG_OMOP (
    PERSON_ID varchar(50) NOT NULL,
    EVENT_TYPE varchar(20),
    EVENT_DATE date,     
    AGE_AT_EVENT integer,
    EVENT_DETAIL varchar(100),
    EVENT_SRC varchar(50)
);
-- diagnoses
insert into ALS_EVENT_LONG_OMOP
with dx_cset as (
      select distinct ref.name, cd.concept_id
            -- , cd.concept_name, cd.concept_code
      from identifier($omop_concept) cd
      join REF_ALS_CDE ref 
      on cd.concept_code like ref.code || '%' and 
         upper(ref.codesystem) = upper(cd.vocabulary_id)
      where ref.codesystem in (
                  'icd9cm',
                  'icd10cm'
            ) and 
            ref.op = 'descendent-of' and 
            ref.name = 'DiagnosALS'
      union
      select distinct ref.name, cd.concept_id
            -- , cd.concept_name, cd.concept_code
      from identifier($omop_concept) cd
      join REF_ALS_CDE ref 
      on cd.concept_code like ref.code || '%' and 
         upper(ref.codesystem) = upper(cd.vocabulary_id)
      where ref.codesystem in (
                  'icd9cm',
                  'icd10cm'
            ) and 
            ref.op = 'exists' and 
            ref.name = 'DiagnosALS'
)
select distinct
       a.person_id, 
       'DX' as EVENT_TYPE,
       a.condition_start_date as EVENT_DATE,
       year(a.condition_start_date) - p.year_of_birth as AGE_AT_EVENT,
       b.name as EVENT_DETAIL,
       'CONDITION_OCCURRENCE' as EVENT_SRC     
from identifier($omop_condition_occurrence) a 
join dx_cset b on a.condition_source_concept_id = b.concept_id
join identifier($omop_person) p on a.person_id = p.person_id
;

-- drug
insert into ALS_EVENT_LONG_OMOP
with rx_cset as (
      select distinct ref.name, cd.concept_id
            -- , cd.concept_name, cd.concept_code
      from identifier($omop_concept) cd
      join REF_ALS_CDE ref 
      on cd.concept_code like ref.code || '%' and 
         upper(ref.codesystem) = upper(cd.vocabulary_id)
      where ref.codesystem in (
                  'rxnorm',
                  'ndc'
            ) and 
            ref.op = 'exists' and 
            ref.name in ('riluzole','edaravone','toferson')
)
select distinct
       a.person_id, 
       'PRX' as EVENT_TYPE,
       a.drug_exposure_start_date as EVENT_DATE,
       year(a.drug_exposure_start_date) - p.year_of_birth as AGE_AT_EVENT,
       b.name as EVENT_DETAIL,
       'DRUG_EXPOSURE' as EVENT_SRC     
from identifier($omop_drug_exposure) a 
join rx_cset b on a.drug_concept_id = b.concept_id
join identifier($omop_person) p on a.person_id = p.person_id
union 
select distinct
       a.person_id, 
       'DRX' as EVENT_TYPE,
       a.drug_era_start_date as EVENT_DATE,
       year(a.drug_era_start_date) - p.year_of_birth as AGE_AT_EVENT,
       b.name as EVENT_DETAIL,
       'DRUG_ERA' as EVENT_SRC     
from identifier($omop_drug_era) a 
join rx_cset b on a.drug_concept_id = b.concept_id
join identifier($omop_person) p on a.person_id = p.person_id
;

-- neurology visits
insert into ALS_EVENT_LONG_OMOP
with provider_cset as (
      select distinct ref.name, cd.concept_id
            -- , cd.concept_name, cd.concept_code
      from identifier($omop_concept) cd
      join REF_ALS_CDE ref 
      on cd.concept_code like ref.code || '%' and 
         upper(ref.codesystem) = upper(cd.vocabulary_id)
      where ref.codesystem = 'hcpt' and
            ref.op = 'exists' and 
            ref.name in ('NeurologyVisit')
)
select distinct
       a.person_id, 
       'NEURO' as EVENT_TYPE,
       a.visit_start_date as EVENT_DATE,
       year(a.visit_start_date) - p.year_of_birth as AGE_AT_EVENT,
       ref.name as EVENT_DETAIL,
       'CARE_SITE' as EVENT_SRC     
from identifier($omop_visit_occurrence) a 
join identifier($omop_care_site) b on a.care_site_id = b.care_site_id
join REF_ALS_CDE ref on regexp_like(b.care_site_name, '.*'||ref.code||'.*', 'i') 
join identifier($omop_person) p on a.person_id = p.person_id
where ref.op = 'regex' and ref.name in ('NeurologyVisit')
union 
select distinct
       a.person_id, 
       'NEURO' as EVENT_TYPE,
       a.visit_start_date as EVENT_DATE,
       year(a.visit_start_date) - p.year_of_birth as AGE_AT_EVENT,
       ref.name as EVENT_DETAIL,
       'PROVIDER' as EVENT_SRC     
from identifier($omop_visit_occurrence) a 
join identifier($omop_provider) b on a.provider_id = b.provider_id
join provider_cset ref on b.specialty_concept_id = ref.concept_id
join identifier($omop_person) p on a.person_id = p.person_id
;

-- medicare eligibility under 65
insert into ALS_EVENT_LONG_OMOP
with payor_cset as (
      select distinct concept_id, concept_name
      from identifier($omop_concept)
      where vocabulary_id = 'SOPT' and 
            concept_code like '1%' -- medicare
)
select distinct
       a.person_id, 
       'MEDICARE' as EVENT_TYPE,
       a.payer_plan_period_start_date as EVENT_DATE,
       year(a.payer_plan_period_start_date) - p.year_of_birth as AGE_AT_EVENT,
       ref.concept_name as EVENT_DETAIL,
       'PAYER_PLAN_PERIOD' as EVENT_SRC     
from identifier($omop_payer_plan_period) a 
join payor_cset ref on a.payer_concept_id = ref.concept_id
join identifier($omop_person) p on a.person_id = p.person_id
;


create or replace table ALS_CASE_TABLE1 as
with grp_by_type as (
      select person_id, 
             event_type,
             event_date,
             age_at_event,
             event_src,
             count(distinct event_type) over (partition by person_id) as distinct_event_cnt,
             count(distinct event_date) over (partition by person_id) as distinct_date_cnt,
             count(distinct event_date) over (partition by person_id,event_type) as event_distinct_date_cnt,
             row_number() over (partition by person_id order by event_date) as rn
      from ALS_EVENT_LONG_OMOP
), summ_event_str as (
    select g.*,
           listagg(distinct g.event_type||g.event_distinct_date_cnt,'|') within group (order by g.event_type||g.event_distinct_date_cnt) over (partition by g.person_id) as event_str
    from grp_by_type g
), get_als1dx as(
    select person_id,
           event_date as als1dx_date,
           age_at_event as age_at_als1dx
    from(
      select a.*, row_number() over (partition by a.person_id order by a.event_date) rn
      from ALS_EVENT_LONG_OMOP a
      where a.event_type = 'DX'
    )
    where rn = 1
)
select a.person_id 
      ,b.birth_datetime
      ,zg.concept_name as gender
      ,zr.concept_name as race 
      ,ze.concept_name as ethnicity
      ,c.als1dx_date
      ,a.event_type as index_event
      ,case when a.event_type = 'NEURO' then c.als1dx_date else a.event_date end as index_date
      ,case when a.event_type = 'NEURO' then c.age_at_als1dx else a.age_at_event end as age_at_index
      ,a.event_src as index_src
      ,a.distinct_event_cnt
      ,a.distinct_date_cnt
      ,a.event_str
from summ_event_str a 
join identifier($omop_person) b on a.person_id = b.person_id
join identifier($omop_concept) zg on b.gender_concept_id = zg.concept_id
join identifier($omop_concept) zr on b.race_concept_id = zr.concept_id
join identifier($omop_concept) ze on b.ethnicity_concept_id = ze.concept_id
join get_als1dx c on a.person_id = c.person_id
where a.rn = 1 and b.birth_datetime is not null
      and a.distinct_date_cnt > 1 
      and (a.event_str like '%DX%' and a.event_str <> 'DX1') -- at least likely, only 1 DX is considered "undetermined"
;

-- N(total) of eligible ALS patients
select count(distinct person_id) from ALS_CASE_TABLE1;

-- N of different computable phenotypes
select event_str, count(distinct person_id) from ALS_CASE_TABLE1
group by event_str
;

