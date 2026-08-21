select count(distinct patid), count(*) from DM_DXRX_LONG;
-- 4,665,261	359,975,335
-- 10,776,675	989,220,271


select count(distinct patid) from DM_DXRX_LONG
where phe_type in ('T2DM','T1DM') and SITE!='CMS';
-- 2,811,768

select count(distinct patid), count(*) from DM_LAB_LONG;
-- 1,965,409	21,177,216
select count(distinct patid), count(*) from ALS_DXRXPX_LONG;
-- 10,853,298	139,287,065
-- 22,442,397	497,007,816

select * from DM_DXRX_LONG limit 5;
select * from DM_LAB_LONG limit 5;
select PHE_TYPE, count(distinct patid)
from DM_DXRX_LONG
group by PHE_TYPE
;

select PHE_TYPE, count(distinct patid)
from DM_LAB_LONG
group by PHE_TYPE
;

select * from ALS_DXRXPX_LONG limit 5;
select PHE_TYPE, count(distinct patid)
from ALS_DXRXPX_LONG
group by PHE_TYPE
;

create or replace table DM_TBL1_CENSOR as 
with cte as (
  select a.*, b.censor_date,
         row_number() over (partition by a.patid order by b.censor_date desc) as rn
from DM_TBL1 a
join PAT_TABLE1 b 
on a.patid = b.patid
)
select cte.* exclude (rn)
from cte
where rn = 1;

select * from DM_TBL1;
select count(distinct patid), count(*) from DM_TBL1;
-- 2,345,051
-- 7,580,548
-- 2,719,403


select * from ALS_DM_ANNUAL_RATES;

select T1DM_IND, T1DM_IND2, count(distinct patid) 
from DM_TBL1 
group by T1DM_IND, T1DM_IND2
;

select count(distinct patid) from PAT_DEMO_LONG;
-- 35,870,549
select count(distinct patid), count(*) from PAT_TABLE1;
-- 35,870,549

select * from ALS_CASE_TABLE1 limit 5;
select count(distinct patid), count(*) from ALS_CASE_TABLE1;
-- 13806

select count(distinct patid) from ALS_INC_CASE_TABLE1;
-- 5,929

select * from ALS_DM_ANNUAL_RATES;


select * from T2DM_TABLE1 limit 5;


select * from GLP1_DPP4_TABLE1 limit 5;

create or replace table T2DM_REF as 
select patid, race, hispanic, sex, 
       case when age_at_index between 10 and 13 then '10-13'
            when age_at_index between 14 and 17 then '14-17'
            when age_at_index between 18 and 44 then '18-44'
            when age_at_index between 45 and 64 then '45-64'
            when age_at_index >= 65 then '65+'
            else 'Unknown' end as age_group,
       case when age_at_index <= 17 then 1 else 0 end as adolescent_ind,
       index_src as site
from T2DM_TABLE1
where age_at_index >= 17 and index_date<= '2025-12-31' and index_src <> 'CMS'
;

select site, count(distinct patid) as num_patients
from T2DM_REF
group by site
order by count(distinct patid) desc
;
-- WASHU
-- IHC
-- UTSW
-- ALLINA
-- UTHOUSTON
-- MCW
       

select count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
;

select age_group, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by age_group
order by age_group
;

select sex, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by sex
order by sex
;

select race, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by race
order by race
;

select hispanic, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by hispanic
order by hispanic
;

-- with GLP1
select count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and 
  exists (select 1 from GLP1_DPP4_TABLE1 b where T2DM_REF.patid = b.patid and b.glp1_ind = 1) and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
;

select age_group, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  exists (select 1 from GLP1_DPP4_TABLE1 b where T2DM_REF.patid = b.patid and b.glp1_ind = 1) and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by age_group
order by age_group
;

select sex, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  exists (select 1 from GLP1_DPP4_TABLE1 b where T2DM_REF.patid = b.patid and b.glp1_ind = 1) and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by sex
order by sex
;

select race, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  exists (select 1 from GLP1_DPP4_TABLE1 b where T2DM_REF.patid = b.patid and b.glp1_ind = 1) and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by race
order by race
;

select hispanic, count(distinct patid) as num_patients
from T2DM_REF
where adolescent_ind = 1 and
  exists (select 1 from GLP1_DPP4_TABLE1 b where T2DM_REF.patid = b.patid and b.glp1_ind = 1) and
  site in ('WASHU','IHC','UTSW','ALLINA','UTHOUSTON','MCW')
group by hispanic
order by hispanic
;