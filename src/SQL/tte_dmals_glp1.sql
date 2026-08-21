/*
TTE cohort extraction
*/

select distinct race from ALS_CASE_TABLE1 limit 5;

create or replace table ALS_BASE as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       a.cphety,
       a.case_assert,
       a.complt_flag,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       case when dm.index_date is not null then 1 else 0 end as dm_ind,
       coalesce(dm.t1dm_ind,0) as t1dm_ind,
       case when dm.index_date is not null and dm.t1dm_ind = 0 then 1 else 0 end as t2dm_ind
from ALS_CASE_TABLE1 a 
join PAT_TABLE1 p on a.patid = p.patid
left join DM_TABLE1 dm on a.patid = dm.patid
where a.age_at_index <= 100
;
select case_assert,count(distinct patid), count(*) 
from als_base
group by case_assert
;

create or replace table ALS_BASE_EHR as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       a.cphety,
       a.case_assert,
       a.complt_flag,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       case when dm.index_date is not null then 1 else 0 end as dm_ind,
       coalesce(dm.t1dm_ind,0) as t1dm_ind,
       case when dm.index_date is not null and dm.t1dm_ind = 0 then 1 else 0 end as t2dm_ind
from ALS_CASE_TABLE1_EHR a 
join PAT_TABLE1 p on a.patid = p.patid
left join DM_TABLE1_EHR dm on a.patid = dm.patid
where a.age_at_index <= 100
;
select case_assert,count(distinct patid), count(*) 
from als_base_ehr
group by case_assert
;

create or replace table ALS_BASE_CMS as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       a.cphety,
       a.case_assert,
       a.complt_flag,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       case when dm.index_date is not null then 1 else 0 end as dm_ind,
       coalesce(dm.t1dm_ind,0) as t1dm_ind,
       case when dm.index_date is not null and dm.t1dm_ind = 0 then 1 else 0 end as t2dm_ind
from ALS_CASE_TABLE1_CMS a 
join PAT_TABLE1 p on a.patid = p.patid
left join DM_TABLE1_CMS dm on a.patid = dm.patid
where a.age_at_index <= 100
;
select case_assert,count(distinct patid), count(*) 
from als_base_cms
group by case_assert
;


create or replace table DM_ALS_BASE as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       a.cphety,
       a.case_assert,
       a.complt_flag,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       coalesce(dm.t1dm_ind,0) as t1dm_ind,
       case when coalesce(d.GLP1_IND,d.DPP4_IND) = 1 then 1 else 0 end as GLP1_DPP4_IND,
       coalesce(d.GLP1_IND,0) as GLP1_IND,
       coalesce(d.DPP4_IND,0) as DPP4_IND,
       case when d.GLP1_START_DATE<a.index_date and d.DPP4_START_DATE<a.index_date then 'both'
            when d.GLP1_START_DATE<a.index_date then 'glp1'
            when d.DPP4_START_DATE<a.index_date then 'dpp4'
       end as glp1_dpp4_prior
from ALS_CASE_TABLE1 a 
join PAT_TABLE1 p on a.patid = p.patid
join DM_TABLE1 dm on a.patid = dm.patid
left join GLP1_DPP4_TABLE1 d on a.patid = d.patid
where a.age_at_index <= 100
;

select case_assert,count(distinct patid), count(*) 
from dm_als_base
group by case_assert
;


create or replace table DM_ALS_BASE_EHR as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       a.cphety,
       a.case_assert,
       a.complt_flag,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       coalesce(dm.t1dm_ind,0) as t1dm_ind,
       case when coalesce(d.GLP1_IND,d.DPP4_IND) = 1 then 1 else 0 end as GLP1_DPP4_IND,
       coalesce(d.GLP1_IND,0) as GLP1_IND,
       coalesce(d.DPP4_IND,0) as DPP4_IND,
       case when d.GLP1_START_DATE<a.index_date and d.DPP4_START_DATE<a.index_date then 'both'
            when d.GLP1_START_DATE<a.index_date then 'glp1'
            when d.DPP4_START_DATE<a.index_date then 'dpp4'
       end as glp1_dpp4_prior
from ALS_CASE_TABLE1_EHR a 
join PAT_TABLE1 p on a.patid = p.patid
join DM_TABLE1_EHR dm on a.patid = dm.patid
left join GLP1_DPP4_TABLE1_EHR d on a.patid = d.patid
where a.age_at_index <= 100
;

select case_assert,count(distinct patid), count(*) 
from dm_als_base_EHR
group by case_assert
;


create or replace table DM_ALS_BASE_CMS as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       a.cphety,
       a.case_assert,
       a.complt_flag,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       coalesce(dm.t1dm_ind,0) as t1dm_ind,
       case when coalesce(d.GLP1_IND,d.DPP4_IND) = 1 then 1 else 0 end as GLP1_DPP4_IND,
       coalesce(d.GLP1_IND,0) as GLP1_IND,
       coalesce(d.DPP4_IND,0) as DPP4_IND,
       case when d.GLP1_START_DATE<a.index_date and d.DPP4_START_DATE<a.index_date then 'both'
            when d.GLP1_START_DATE<a.index_date then 'glp1'
            when d.DPP4_START_DATE<a.index_date then 'dpp4'
       end as glp1_dpp4_prior
from ALS_CASE_TABLE1_CMS a 
join PAT_TABLE1 p on a.patid = p.patid
join DM_TABLE1_CMS dm on a.patid = dm.patid
left join GLP1_DPP4_TABLE1_CMS d on a.patid = d.patid
where a.age_at_index <= 100
;

select case_assert,count(distinct patid), count(*) 
from dm_als_base_CMS
group by case_assert
;


create or replace table DM_BASE as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       coalesce(a.t1dm_ind,0) as t1dm_ind
from DM_TABLE1 a 
join PAT_TABLE1 p on a.patid = p.patid
where a.age_at_index <= 100
;
select count(distinct patid), count(*) 
from dm_base
;
-- 6249383

create or replace table DM_BASE_EHR as 
select a.patid,
       a.index_date,
       a.age_at_index,
       a.sex,
       case when a.sex = 'F' then 1 else 0 end as SEX_F,
       a.race, 
       a.hispanic,
       p.censor_date,
       datediff(day,a.index_date,p.censor_date) as censor_since_index,
       p.death_ind,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 1 then 1 else 0 end as surv1yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 2 then 1 else 0 end as surv2yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 3 then 1 else 0 end as surv3yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 4 then 1 else 0 end as surv4yr,
       case when datediff(day,a.index_date,p.censor_date)/365.25 >= 5 then 1 else 0 end as surv5yr,
       coalesce(a.t1dm_ind,0) as t1dm_ind
from DM_TABLE1_EHR a 
join PAT_TABLE1 p on a.patid = p.patid
where a.age_at_index <= 100
;
select count(distinct patid), count(*) 
from dm_base_ehr
;