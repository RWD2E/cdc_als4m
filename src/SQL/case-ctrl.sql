
create or replace table NONALS_TBL1 as 
with nonals_ordered as (
    select  b.patid, 
            b.age_at_diagnosis as age_at_index,
            b.sex as sex,
            b.race,
            CASE WHEN b.hispanic = 'Y' THEN 'hispanic' 
                 WHEN b.hispanic = 'N' THEN 'non-hispanic' 
                 ELSE 'NI' END AS hispanic,
            b.enr_duration, 
            b.firstdx_date,
            datediff('day',b.enr_start_date,b.firstdx_date) baseline_coc,
            bmi.bmi, 
            bmi.ht,
            bmi.wt,
            row_number() over (partition by b.patid order by abs(datediff('day',b.firstdx_date,bmi.obs_date))) as rn
        from GROUSE_ANALYTICS_DB.ASA_ALS_EHR.CONTROL_GROUP_DEMOGRAPHICS b 
        left join GROUSE_ANALYTICS_DB.ASA_ALS_EHR.NON_ALS_SEL_OBS_BMI bmi on b.patid = bmi.patid
)
select patid, age_at_index, sex, race, hispanic, enr_duration, baseline_coc, bmi, ht, wt
from nonals_ordered 
where rn = 1
;
select count(*), count(distinct patid) from NONALS_TBL1;


create or replace table NONALS_MATCHED_TBL1 as 
with nonals_ordered as (
    select  a.nonals_patid as patid, 
            a.nonals_age as age_at_index,
            a.nonals_sex as sex,
            a.nonals_race as race, 
            CASE WHEN b.hispanic = 'Y' THEN 'hispanic' 
                 WHEN b.hispanic = 'N' THEN 'non-hispanic' 
                 ELSE 'NI' END AS hispanic,
            b.enr_duration, 
            b.firstdx_date,
            datediff('day',b.enr_start_date,b.firstdx_date) baseline_coc,
            bmi.bmi, 
            bmi.ht,
            bmi.wt,
            row_number() over (partition by a.nonals_patid order by abs(datediff('day',b.firstdx_date,bmi.obs_date))) as rn
        from GROUSE_ANALYTICS_DB.ASA_ALS_EHR.MATCHED_COHORT a 
        join GROUSE_ANALYTICS_DB.ASA_ALS_EHR.CONTROL_GROUP_DEMOGRAPHICS b on a.nonals_patid = b.patid
        left join GROUSE_ANALYTICS_DB.ASA_ALS_EHR.NON_ALS_SEL_OBS_BMI bmi on a.nonals_patid = bmi.patid
)
select patid, age_at_index, sex, race, hispanic, enr_duration, baseline_coc, bmi, ht, wt
from nonals_ordered 
where rn = 1
-- random sampling
order by random()
limit 8580
;
select count(*), count(distinct patid) from NONALS_MATCHED_TBL1;


create or replace table ALS_TBL1 as 
with als_ordered as (
    select  a.als_patid as patid, 
            a.als_age as age_at_index,
            a.als_sex as sex,
            a.als_race as race, 
            b.hispanic,
            b.enr_duration, 
            b.als1dx_date as firstdx_date,
            datediff('day',b.enr_start_date,b.als1dx_date) baseline_coc,
            bmi.bmi, 
            bmi.ht,
            bmi.wt,
            row_number() over (partition by a.als_patid order by abs(datediff('day',b.als1dx_date,bmi.obs_date))) as rn
        from GROUSE_ANALYTICS_DB.ASA_ALS_EHR.MATCHED_COHORT a 
        join GROUSE_ANALYTICS_DB.ASA_ALS_EHR.ALS_COHORT b on a.als_patid = b.patid
        left join GROUSE_ANALYTICS_DB.ASA_ALS_EHR.ALS_SEL_OBS_BMI bmi on a.als_patid = bmi.patid
)
select patid, age_at_index, sex,race, hispanic, enr_duration, baseline_coc, bmi, ht, wt
from als_ordered 
where rn = 1
;

select count(*), count(distinct patid) from ALS_TBL1;
     