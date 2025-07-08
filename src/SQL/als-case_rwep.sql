/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_rwep.sql
# Dependency: 
# - ALS_CASE_TABLE1
# - ALS_ALL_DX, 
# - ALS_ALL_PX, 
# - ALS_ALL_OBS
# - ALS_ENDPT_REF
# 
# Milestones were defined as symptom onset (functional involvement by weakness, wasting,
# spasticity, dysarthria or dysphagia of one central nervous system region defined as bulbar, upper limb, lower limb or diaphragmatic),
# diagnosis, functional involvement of a second region, functional involvement of a third region, needing gastrostomy and
# non-invasive ventilation.
*/

select * from als_endpt_ref;

create or replace procedure get_rwep_long(
    REF_COHORT string,
    SITES array,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} REF_COHORT: name of reference patient table, should at least include (patid, birth_date, index_date)
 * @param {array} SITES: an array of site acronyms (matching schema name suffix) - include CMS
 * @param {boolean} DRY_RUN: dry run indicator. If true, only sql script will be created and stored in dev.sp_out table
 * @param {boolean} DRY_RUN_AT: A temporary location to store the generated sql query for debugging purpose. 
                                When DRY_RUN = True, provide absolute path to the table; when DRY_RUN = False, provide NULL 
**/
if (DRY_RUN) {
    var log_stmt = snowflake.createStatement({
        sqlText: `CREATE OR REPLACE TEMPORARY TABLE `+ DRY_RUN_AT +`(QRY VARCHAR);`});
    log_stmt.execute(); 
}

var i;
for(i=0; i<SITES.length; i++){
       var site = SITES[i].toString();
       var site_cdm = (site === 'CMS') ? 'CMS_PCORNET_CDM' : 'PCORNET_CDM_' + site;

       // collect death dates
       sqlstmt1 = `
              INSERT INTO ALS_ENDPT_LONG(patid,first_obs_date,first_obs_since_index,last_obs_date,last_obs_since_index,ENDPT_TYPE,ENDPT_SRC)
              select  a.PATID
                     ,max(d.death_date::date) as FIRST_OBS_DATE
                     ,datediff(day,a.index_date,max(d.death_date::date)) as FIRST_OBS_SINCE_INDEX 
                     ,max(d.death_date::date) as LAST_OBS_DATE
                     ,datediff(day,a.index_date,max(d.death_date::date)) as LAST_OBS_SINCE_INDEX 
                     ,'death' as ENDPT_TYPE
                     ,'`+ site +`' as ENDPT_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_DEATH d
                     on a.patid = d.patid
              where d.death_date is not null
              group by a.PATID,a.index_date
              ;
       `;
       var run_sqlstmt1 = snowflake.createStatement({sqlText: sqlstmt1}); 
 
       // collect first and last enrollment date
       sqlstmt2 = `
              INSERT INTO ALS_ENDPT_LONG(patid,first_obs_date,first_obs_since_index,last_obs_date,last_obs_since_index,ENDPT_TYPE,ENDPT_SRC)
              select  a.PATID
                     ,min(coalesce(b.enr_start_date,b.enr_end_date)) as FIRST_OBS_DATE
                     ,datediff(day,a.index_date,min(coalesce(b.enr_start_date,b.enr_end_date)) ) as FIRST_OBS_SINCE_INDEX 
                     ,max(coalesce(b.enr_end_date,b.enr_start_date)) as LAST_OBS_DATE
                     ,datediff(day,a.index_date,max(coalesce(b.enr_end_date,b.enr_start_date)) ) as LAST_OBS_SINCE_INDEX 
                     ,'censor' as ENDPT_TYPE
                     ,'`+ site +`' as ENDPT_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_ENROLLMENT b
                     on a.patid = b.patid
              group by a.PATID,a.index_date
              ;
       `;
       var run_sqlstmt2 = snowflake.createStatement({sqlText: sqlstmt2}); 

       // collect first and last encounter date
       sqlstmt3 = `
              INSERT INTO ALS_ENDPT_LONG(patid,first_obs_date,first_obs_since_index,last_obs_date,last_obs_since_index,ENDPT_TYPE,ENDPT_SRC)
              select  a.PATID
                     ,min(coalesce(b.admit_date,b.discharge_date)) as FIRST_OBS_DATE
                     ,datediff(day,a.index_date,min(coalesce(b.admit_date,b.discharge_date)) ) as FIRST_OBS_SINCE_INDEX 
                     ,max(coalesce(b.discharge_date,b.admit_date)) as LAST_OBS_DATE
                     ,datediff(day,a.index_date,max(coalesce(b.discharge_date,b.admit_date)) ) as LAST_OBS_SINCE_INDEX 
                     ,'censor' as ENDPT_TYPE
                     ,'`+ site +`' as ENDPT_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_ENCOUNTER b
                     on a.patid = b.patid
              where b.enc_type not in ('NI','OT','UN')
              group by a.PATID,a.index_date
              ;
       `;
       var run_sqlstmt3 = snowflake.createStatement({sqlText: sqlstmt3}); 

       if (DRY_RUN) {
              // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
              var log_stmt = snowflake.createStatement({
                            sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1),(:2),(:3);`,
                            binds: [sqlstmt1, sqlstmt2, sqlstmt3]});
        log_stmt.execute(); 
       } else {
              // run dynamic dml query
              var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
              run_sqlstmt1.execute();
              run_sqlstmt2.execute();
              run_sqlstmt3.execute();
              commit_txn.execute();
       }
}
$$
;

/* test */
-- call get_rwep_long(
--        'ALS_TABLE1',
--        array_construct(
--               'CMS'
--              ,'MU'
--        ),
--        True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;

/* main */
create or replace table ALS_ENDPT_LONG(
        PATID varchar(50) not null
       ,FIRST_OBS_DATE date
       ,FIRST_OBS_SINCE_INDEX number
       ,LAST_OBS_DATE date
       ,LAST_OBS_SINCE_INDEX number
       ,ENDPT_TYPE varchar(10)
       ,ENDPT_SRC varchar(10)
       ,ENDPT varchar(100)
       ,ENDPT_SUB varchar(100)
);

call get_rwep_long(
       'ALS_CASE_TABLE1',
       array_construct(
         'CMS'
        ,'ALLINA'
        ,'IHC'
        ,'KUMC'
        ,'MCRI'
        ,'MCW'
        ,'MU'
        ,'UIOWA'
        ,'UNMC'
        ,'UTHOUSTON'
        ,'UTHSCSA'
        ,'UTSW'
        ,'UU'
        ,'WASHU'
    ), 
    FALSE, NULL
);

INSERT INTO ALS_ENDPT_LONG(patid,first_obs_date,first_obs_since_index,last_obs_date,last_obs_since_index,ENDPT_TYPE,ENDPT_SRC,ENDPT,ENDPT_SUB)
select a.PATID
      ,min(coalesce(dx_date,admit_date)) as FIRST_OBS_DATE
      ,min(days_since_index) as FIRST_OBS_SINCE_INDEX 
      ,max(coalesce(dx_date,admit_date)) as LAST_OBS_DATE
      ,max(days_since_index) as LAST_OBS_SINCE_INDEX 
      ,b.endpt_grp as ENDPT_TYPE
      ,a.DX_SRC as ENDPT_SRC
      ,b.endpt as ENDPT
      ,b.endpt_sub as ENDPT_SUB
from ALS_ALL_DX a 
join als_endpt_ref b 
on replace(a.dx,'.','') like b.cd || '%'
  and a.dx_type = b.cd_type 
  and b.endpt_type = 'DX'
group by a.PATID,b.endpt_grp,a.DX_SRC,b.endpt,b.endpt_sub 
;

INSERT INTO ALS_ENDPT_LONG(patid,first_obs_date,first_obs_since_index,last_obs_date,last_obs_since_index,ENDPT_TYPE,ENDPT_SRC,ENDPT,ENDPT_SUB)
select PATID
      ,min(coalesce(px_date,admit_date)) as FIRST_OBS_DATE
      ,min(days_since_index) as FIRST_OBS_SINCE_INDEX 
      ,max(coalesce(px_date,admit_date)) as LAST_OBS_DATE
      ,max(days_since_index) as LAST_OBS_SINCE_INDEX 
      ,b.endpt_grp as ENDPT_TYPE
      ,a.PX_SRC as ENDPT_SRC
      ,b.endpt as ENDPT
      ,b.endpt_sub as ENDPT_SUB
from ALS_ALL_PX a 
join als_endpt_ref b 
on a.px = b.cd 
  and b.cd_type = 'CH' 
  and b.endpt_type = 'PX'
group by a.PATID,b.endpt_grp,a.PX_SRC,b.endpt,b.endpt_sub 
;

select * from ALS_ENDPT_LONG limit 5;

select ENDPT_TYPE, count(distinct patid) 
from ALS_ENDPT_LONG
group by ENDPT_TYPE
;

select ENDPT_TYPE,ENDPT,ENDPT_SUB,count(distinct patid) 
from ALS_ENDPT_LONG
group by ENDPT_TYPE,ENDPT,ENDPT_SUB
order by ENDPT_TYPE,ENDPT,ENDPT_SUB
;

create or replace table ALS_ENDPTS as 
with cte_stg1 as (
       select patid,
              first_obs_date as stage_date,
              FIRST_OBS_SINCE_INDEX as stage_since_index,
              'stg1' as stage,
              endpt_type,
              endpt,
              endpt_sub,
              row_number() over (partition by patid order by FIRST_OBS_SINCE_INDEX) rn
       from ALS_ENDPT_LONG
       where endpt_type in ('limb','bulbar')
), cte_stg2 as (
       select patid,
              first_obs_date as stage_date,
              FIRST_OBS_SINCE_INDEX as stage_since_index,
              'stg2' as stage,
              endpt_type,
              endpt,
              endpt_sub,
              dense_rank() over (partition by patid order by endpt_type) region,
              row_number() over (partition by patid,endpt_type order by FIRST_OBS_SINCE_INDEX) rn
       from ALS_ENDPT_LONG
       where endpt_type in ('limb','bulbar')
), cte_stg3 as (
       select patid,
              first_obs_date as stage_date,
              FIRST_OBS_SINCE_INDEX as stage_since_index,
              case when endpt_sub = 'gastrostomy' then 'stg3'
                   when endpt_sub = 'non-invasive-ventilator' then 'stg4'
                   else 'stg5'
              end as stage,
              endpt_type,
              endpt,
              endpt_sub,
              row_number() over (partition by patid,endpt_sub order by FIRST_OBS_SINCE_INDEX) rn
       from ALS_ENDPT_LONG
       where endpt_sub in ('gastrostomy','non-invasive-ventilator','invasive-ventilator')
), cte_stg4 as (
       select patid,
              first_obs_date as stage_date,
              FIRST_OBS_SINCE_INDEX as stage_since_index,
              'death' as stage,
              'death' as endpt_type,
              'death' as endpt,
              'death' as endpt_sub,
              row_number() over (partition by patid order by FIRST_OBS_SINCE_INDEX) rn
       from ALS_ENDPT_LONG
       where endpt_type = 'death'
), cte_init_dx as (
       select patid,
              als1dx_date as stage_date,
              datediff(day,index_date,als1dx_date) as stage_since_index,
              'als1dx' as stage,
              'als1dx' as endpt_type,
              'als1dx' as endpt,
              'als1dx' as endpt_sub
       from ALS_TABLE1
), cte_censor as (
       select a.patid,
              a.last_obs_date as stage_date,
              a.LAST_OBS_SINCE_INDEX as stage_since_index,
              a.endpt_type as stage,
              a.endpt_type,
              'censor' as endpt,
              'censor' as endpt_sub,
              row_number() over (partition by a.patid order by a.LAST_OBS_SINCE_INDEX desc) rn_desc
       from ALS_ENDPT_LONG a
       where a.endpt_type = 'censor' 
)
select patid,stage,stage_date,stage_since_index,endpt_type,endpt,endpt_sub from cte_stg1 where rn = 1
union
select patid,stage,stage_date,stage_since_index,endpt_type,endpt,endpt_sub from cte_stg2 where region = 2 and rn = 1
union
select patid,stage,stage_date,stage_since_index,endpt_type,endpt,endpt_sub from cte_stg3 where rn = 1
union
select patid,stage,stage_date,stage_since_index,endpt_type,endpt,endpt_sub from cte_stg4 where rn = 1
union
select patid,stage,stage_date,stage_since_index,endpt_type,endpt,endpt_sub from cte_init_dx
union 
select patid,stage,stage_date,stage_since_index,endpt_type,endpt,endpt_sub from cte_censor where rn_desc = 1
order by patid, stage_date
;

select * from ALS_ENDPTS 
order by patid, stage_date;

select count(*), count(distinct patid)
from ALS_ENDPTS;

select stage, count(distinct patid), count(*)
from ALS_ENDPTS
group by stage
order by stage
;

select stage,endpt_sub, count(distinct patid)
from ALS_ENDPTS
group by stage,endpt_sub
;

create or replace table ALS_SEL_DEVICE as 
select distinct
       patid
      ,first_obs_date as DEVICE_DATE
      ,first_obs_since_index as DAYS_SINCE_INDEX
      ,case when endpt_sub in ('wheelchairs','power-wheelchairs') then 'wheelchair'
            else endpt_sub
       end as DEVICE
from ALS_ENDPT_LONG
where ENDPT_SUB in (
       'non-invasive-ventilator',
       'gastrostomy',
       'wheelchairs',
       'power-wheelchairs'
)
;

select device, count(distinct patid)
from als_sel_device
group by device;