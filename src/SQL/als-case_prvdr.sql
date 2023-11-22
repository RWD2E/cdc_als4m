/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_provider.sql
# Description: extract specialty provider
*/
create or replace table ALS_PX_PRVDR as
select PATID,
       PX_DATE,
       DAYS_SINCE_INDEX,
       PROVIDER_SPECIALTY,
       case when lower(PROVIDER_SPECIALTY) like '%neurolog%' then 'neurology'
            when lower(PROVIDER_SPECIALTY) like '%psychiatr%' then 'psychiatry'
            when lower(PROVIDER_SPECIALTY) like '%psycholog%' then 'psychiatry'
            when lower(PROVIDER_SPECIALTY) like '%internal%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%family%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%general%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%geriatric%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%nurse%' and lower(PROVIDER_SPECIALTY) not like '%anesthe' then 'nurse'
            when lower(PROVIDER_SPECIALTY) like '%social%' then 'social'
            when lower(PROVIDER_SPECIALTY) like '%rehap%' then 'rehap'
            when lower(PROVIDER_SPECIALTY) like '%physical%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%occupation%' then 'ot'
            when lower(PROVIDER_SPECIALTY) like '%sport%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%chiropract%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%osteopathic%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%genetic%' then 'genetic'
            when lower(PROVIDER_SPECIALTY) like '%pain%' then 'pain'
            when lower(PROVIDER_SPECIALTY) like '%respiratory%' then 'respiratory'
            when lower(PROVIDER_SPECIALTY) like '%pulmonary%' then 'respiratory'
            when lower(PROVIDER_SPECIALTY) like '%surgery%' then 'surgery'
            when lower(PROVIDER_SPECIALTY) like '%surgical%' then 'surgery'
            when lower(PROVIDER_SPECIALTY) like '%anesthe%' then 'surgery'
            when lower(PROVIDER_SPECIALTY) like '%home health%' then 'home-health'
            when lower(PROVIDER_SPECIALTY) like '%hospice%' then 'palliative'
            when lower(PROVIDER_SPECIALTY) like '%speech language%' then 'slp'
            when lower(PROVIDER_SPECIALTY) like '%diet%' then 'dietition'
            when lower(PROVIDER_SPECIALTY) like '%nutrition%' then 'dietition'    
            when lower(PROVIDER_SPECIALTY) like '%critical%' then 'icu'
            when lower(PROVIDER_SPECIALTY) like '%optometry%' then 'eye'
            when lower(PROVIDER_SPECIALTY) like '%ophthalmology%' then 'eye'
            when lower(PROVIDER_SPECIALTY) like '%otolaryngology%' then 'ent'
            when lower(PROVIDER_SPECIALTY) like '%audiolog%' then 'ent'
            when lower(PROVIDER_SPECIALTY) like '%interventional radiolog%' then 'intv-radiology'
            when lower(PROVIDER_SPECIALTY) like '%cardi%' then 'cardiology'
            when lower(PROVIDER_SPECIALTY) like '%urology%' then 'urology'
       else 'other' end as SPECIALTY_GROUP
from ALS_ALL_PX
where PROVIDER_SPECIALTY is not null 
;
select PROVIDER_SPECIALTY, count(distinct patid)
from ALS_PX_PRVDR
group by PROVIDER_SPECIALTY
order by count(distinct patid) desc;

select SPECIALTY_GROUP, count(distinct patid)
from ALS_PX_PRVDR
group by SPECIALTY_GROUP
order by count(distinct patid) desc;


create or replace table ALS_OFFICE_PRVDR as
select PATID,
       PX_DATE,
       DAYS_SINCE_INDEX,
       PROVIDER_SPECIALTY,
       case when lower(PROVIDER_SPECIALTY) like '%neurolog%' then 'neurology'
            when lower(PROVIDER_SPECIALTY) like '%psychiatr%' then 'psychiatry'
            when lower(PROVIDER_SPECIALTY) like '%psycholog%' then 'psychiatry'
            when lower(PROVIDER_SPECIALTY) like '%internal%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%family%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%general%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%geriatric%' then 'pcp'
            when lower(PROVIDER_SPECIALTY) like '%nurse%' and lower(PROVIDER_SPECIALTY) not like '%anesthe' then 'nurse'
            when lower(PROVIDER_SPECIALTY) like '%social%' then 'social'
            when lower(PROVIDER_SPECIALTY) like '%rehap%' then 'rehap'
            when lower(PROVIDER_SPECIALTY) like '%physical%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%occupation%' then 'ot'
            when lower(PROVIDER_SPECIALTY) like '%sport%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%chiropract%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%osteopathic%' then 'pt'
            when lower(PROVIDER_SPECIALTY) like '%genetic%' then 'genetic'
            when lower(PROVIDER_SPECIALTY) like '%pain%' then 'pain'
            when lower(PROVIDER_SPECIALTY) like '%respiratory%' then 'respiratory'
            when lower(PROVIDER_SPECIALTY) like '%pulmonary%' then 'respiratory'
            when lower(PROVIDER_SPECIALTY) like '%surgery%' then 'surgery'
            when lower(PROVIDER_SPECIALTY) like '%surgical%' then 'surgery'
            when lower(PROVIDER_SPECIALTY) like '%anesthe%' then 'surgery'
            when lower(PROVIDER_SPECIALTY) like '%home health%' then 'home-health'
            when lower(PROVIDER_SPECIALTY) like '%hospice%' then 'palliative'
            when lower(PROVIDER_SPECIALTY) like '%speech language%' then 'slp'
            when lower(PROVIDER_SPECIALTY) like '%diet%' then 'dietition'
            when lower(PROVIDER_SPECIALTY) like '%nutrition%' then 'dietition'    
            when lower(PROVIDER_SPECIALTY) like '%critical%' then 'icu'
            when lower(PROVIDER_SPECIALTY) like '%optometry%' then 'eye'
            when lower(PROVIDER_SPECIALTY) like '%ophthalmology%' then 'eye'
            when lower(PROVIDER_SPECIALTY) like '%otolaryngology%' then 'ent'
            when lower(PROVIDER_SPECIALTY) like '%audiolog%' then 'ent'
            when lower(PROVIDER_SPECIALTY) like '%interventional radiolog%' then 'intv-radiology'
            when lower(PROVIDER_SPECIALTY) like '%cardi%' then 'cardiology'
            when lower(PROVIDER_SPECIALTY) like '%urology%' then 'urology'
       else 'other' end as SPECIALTY_GROUP
from ALS_ALL_PX
where PROVIDER_SPECIALTY is not null 
       and OFFICE_FLAG = 1
;


select PROVIDER_SPECIALTY, count(distinct patid)
from ALS_OFFICE_PRVDR
group by PROVIDER_SPECIALTY
order by count(distinct patid) desc;