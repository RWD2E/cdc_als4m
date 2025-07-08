/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: pat-demo-master.sql
# Description: create PAT_TABLE1 and generate summary statistics  
*/

/*stored procedure to collect overall GPC cohort*/
create or replace procedure get_pat_demo(
    SITES ARRAY,
    DRY_RUN BOOLEAN,
    DRY_RUN_AT STRING
)
returns variant
language javascript
as
$$
/**
 * Stored procedure to collect a Table 1 for overall GPC cohort
 * @param {array} SITES: an array of site acronyms (matching schema name suffix) - not include CMS
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
    var cms_ind = (site === 'CMS') ? 1 : 0;
    
    // dynamic query
    var sqlstmt_par = `
        INSERT INTO PAT_DEMO_LONG
            WITH cte_enc_age AS (
                SELECT d.patid,
                    d.birth_date,
                    e.admit_date::date as index_date,
                    e.enc_type as index_enc_type,
                    round(datediff(day,d.birth_date::date,e.admit_date::date)/365.25) AS age_at_index,
                    case when d.sex in ('NI','UN','OT') then NULL else d.sex end as sex, 
                    case when d.race in ('NI','UN','07','OT') then NULL else d.race end as race, 
                    case when d.hispanic in ('NI','UN','R','OT') then NULL else d.hispanic end as hispanic, 
                    '`+ site +`' as index_src,
                    max(coalesce(e.discharge_date::date,e.admit_date::date,current_date)) over (partition by e.patid) as censor_date,
                    max(dth.death_date) over (partition by e.patid) as death_date
                FROM GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC d 
                JOIN GROUSE_DB.`+ site_cdm +`.LDS_ENCOUNTER e ON d.PATID = e.PATID
                LEFT JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEATH dth on d.PATID = dth.PATID
                WHERE d.patid is not null
                )
                SELECT DISTINCT
                     cte.patid
                    ,cte.birth_date
                    ,cte.index_date
                    ,cte.index_enc_type
                    ,cte.age_at_index
                    ,cte.sex
                    ,cte.race
                    ,cte.hispanic
                    ,cte.index_src
                    ,coalesce(death_date,censor_date) as censor_date
                    ,case when death_date is not null then 1 else 0 end as death_ind
                    ,`+ cms_ind +` as cms_ind
                FROM cte_enc_age cte
                ;
        `;
    
    if (DRY_RUN) {
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                        binds: [sqlstmt_par]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par = snowflake.createStatement({sqlText: sqlstmt_par}); run_sqlstmt_par.execute();
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); commit_txn.execute();
    }
}
$$
;

create or replace table PAT_DEMO_LONG (
    PATID varchar(50) NOT NULL,
    BIRTH_DATE date,
    INDEX_DATE date,  
    INDEX_ENC_TYPE varchar(3),
    AGE_AT_INDEX integer, 
    SEX varchar(3),
    RACE varchar(6),
    HISPANIC varchar(20),
    INDEX_SRC varchar(20),
    CENSOR_DATE date,
    DEATH_IND integer,
    CMS_IND integer
);

/* test */
-- call get_pat_demo(
--     array_construct(
--      'CMS'
--     ,'WASHU'
-- ), True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;

call get_pat_demo(
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
), False, NULL
);

select * from PAT_DEMO_LONG limit 5;

select index_src, count(distinct patid)
from PAT_DEMO_LONG
group by index_src;
                                       

create or replace table PAT_TABLE1 as 
with cte_ord as (
    select a.*,
           row_number() over (partition by a.patid order by a.index_date,a.cms_ind desc) as rn
    from PAT_DEMO_LONG a
), cte_cmsdemo as(
    select a.* exclude(birth_date, sex, race, hispanic, censor_date, death_ind),
           coalesce(a.birth_date,d.birth_date) as birth_date, 
           coalesce(a.sex,d.sex) as sex,
           coalesce(a.race,d.race) as race,
           coalesce(a.hispanic,d.hispanic) as hispanic,
           max(a.censor_date) over (partition by a.patid) as censor_date,
           max(a.death_ind) over (partition by a.patid) as death_ind
    from cte_ord a
    left join GROUSE_DB.CMS_PCORNET_CDM.LDS_DEMOGRAPHIC d
    on a.patid = d.patid
    where a.rn = 1
), cte_partab as (
    select patid,
           min(enr_start_date) as partab_start_date,
           max(NVL(enr_end_date,enr_start_date)) as partab_end_date,
           max(case when chart = 'Y' then 1 else 0 end) as xwalk_ind
    from GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT
    where enr_basis = 'I'
    group by patid
), cte_partd as (
    select patid,
           min(enr_start_date) as partd_start_date,
           max(NVL(enr_end_date,enr_start_date)) as partd_end_date
    from GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT
    where enr_basis = 'D'
    group by patid
), cte_ehr as (
    select patid,
           min(index_date) as ehr_start_date,
           max(index_date) as ehr_end_date
    from PAT_DEMO_LONG
    where cms_ind = 0
    group by patid
)
select a.patid
      ,a.birth_date
      ,a.index_date
      ,year(a.index_date) as index_year
      ,a.age_at_index
      ,case when a.age_at_index is null then 'NI'
            when a.age_at_index < 19 then 'agegrp1'
            when a.age_at_index >= 19 and a.age_at_index < 24 then 'agegrp2'
            when a.age_at_index >= 25 and a.age_at_index < 85 then 'agegrp' || (floor((age_at_index - 25)/5) + 3)
            else 'agegrp15' end as agegrp_at_index
      ,a.sex
      ,CASE WHEN a.race IN ('05') THEN 'white' 
            WHEN a.race IN ('03') THEN 'black'
            WHEN a.race IN ('02') THEN 'asian'
            WHEN a.race IN ('01','04','06','OT') THEN 'other'
            ELSE 'NI' END AS race
      ,CASE WHEN a.hispanic = 'Y' THEN 'hispanic' 
            WHEN a.hispanic = 'N' THEN 'non-hispanic' 
            ELSE 'NI' END AS hispanic
      ,a.index_enc_type
      ,a.index_src
      ,a.censor_date
      ,year(a.censor_date) as censor_year
      ,a.death_ind
      ,coalesce(ab.xwalk_ind,0) as xwalk_ind
      ,ab.partab_start_date
      ,ab.partab_end_date
      ,case when d.partd_start_date is not null then 1 else 0 end as partd_ind
      ,d.partd_start_date
      ,d.partd_end_date
      ,case when ehr.ehr_start_date is not null then 1 else 0 end as ehr_ind
      ,ehr.ehr_start_date
      ,ehr.ehr_end_date
from cte_cmsdemo a
left join cte_partab ab on a.patid = ab.patid
left join cte_partd d on a.patid = d.patid
left join cte_ehr ehr on a.patid = ehr.patid
where a.rn = 1
;

select * from PAT_TABLE1 limit 5;

select count(distinct patid), count(*) from PAT_TABLE1;
-- 45,317,135

select xwalk_ind,  count(distinct patid)
from PAT_TABLE1
group by xwalk_ind;
-- 1	4,545,182
-- 0	40,771,953

select race, count(distinct patid) 
from PAT_TABLE1
group by race;
-- NI	8372094
-- black	4043198
-- asian	986802
-- white	31370071
-- other	528811

select partd_ind, count(distinct patid) 
from PAT_TABLE1
group by partd_ind;
-- 1	15,241,557
-- 0	30,059,419
