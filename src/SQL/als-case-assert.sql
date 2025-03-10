/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_CDC-alg.sql
# Description: multifactorial computable phenotype for identifying ALS patients (CDC alg)
# Dependency: 
# - PAT_TABLE1: generated by pat-demo-master.sql 
# - REF_ALS_CDE: generated by staging the ./ref/als_cde.csv reference table 
# - PCORnet CDM tables
*/

select * from REF_ALS_CDE limit 5;

/*stored procedure to identify ALS cohort*/
create or replace procedure get_als_event_long(
    SITES array,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * Stored procedure to collect a Table 1 for ALS case assertainment events:
 * - ICD9: 335.20 or
 * - ICD10: G12.21 or
 * - Riluzole or Endaravone or Relyvrio or Tofersen
 * - Neurologist office visit
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
    
    // dynamic query
    var sqlstmt_par_dx = `
        INSERT INTO ALS_EVENT_LONG
        SELECT  distinct
                a.patid,
                'DX' AS event_type,
                NVL(a.dx_date,a.admit_date) AS event_date,
                round(datediff(day,b.birth_date,NVL(a.dx_date,a.admit_date))/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DIAGNOSIS a
        JOIN REF_ALS_CDE ref on a.dx LIKE ref.code || '%' and ref.codesystem in ('icd9cm','icd10cm') and ref.name = 'DiagnosALS' and ref.op = 'descendent-of'
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        UNION
        SELECT  distinct
                a.patid,
                'DX' AS event_type,
                NVL(a.dx_date,a.admit_date) AS event_date,
                round(datediff(day,b.birth_date,NVL(a.dx_date,a.admit_date))/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DIAGNOSIS a
        JOIN REF_ALS_CDE ref on a.dx = ref.code and ref.codesystem in ('icd9cm','icd10cm') and ref.name = 'DiagnosALS' and ref.op = 'exists'
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        ;`;

    var sqlstmt_par_drx = `
        INSERT INTO ALS_EVENT_LONG
        SELECT  distinct
                a.patid,
                'DRX' AS event_type,
                a.dispense_date AS event_date,
                round(datediff(day,b.birth_date,a.dispense_date)/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DISPENSING a
        JOIN REF_ALS_CDE ref ON a.NDC = ref.code and ref.codesystem = 'ndc' and ref.name in ('riluzole','edaravone','toferson') 
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;`;
    
    var sqlstmt_par_prx = `
       INSERT INTO ALS_EVENT_LONG
        SELECT  distinct
                a.patid,
                'PRX' AS event_type,
                coalesce(a.rx_order_date,a.rx_start_date) AS event_date,
                round(datediff(day,b.birth_date,coalesce(a.rx_order_date,a.rx_start_date))/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_PRESCRIBING a
        JOIN REF_ALS_CDE ref ON a.RXNORM_CUI = ref.code and ref.codesystem = 'rxnorm' and ref.name in ('riluzole','edaravone','toferson')
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;`;
    
    var sqlstmt_par_px = `
       INSERT INTO ALS_EVENT_LONG
        SELECT  distinct
                a.patid,
                'NEUROLOGIST' AS event_type,
                a.px_date AS event_date,
                round(datediff(day,b.birth_date,a.px_date)/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_PROCEDURES a
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_PROVIDER p ON a.PROVIDERID = p.PROVIDERID
        JOIN REF_ALS_CDE ref ON p.provider_specialty_primary = ref.code and ref.codesystem = 'cmsTaxonomy'
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        WHERE a.PX in (
            '99201','99202','99203','99204','99205',
            '99206','99207','99208','99209','99210',
            '99211','99212','99213','99214','99215'
        );`;
    
    var sqlstmt_par_enr = `
       INSERT INTO ALS_EVENT_LONG
       WITH cte_enrlt65 as (
        SELECT  a.patid,
                a.ENR_START_DATE,
                round(datediff(day,b.birth_date,a.ENR_START_DATE)/365.25) AS age_at_event,
                row_number() over (partition by a.patid order by a.ENR_START_DATE) AS rn
        FROM GROUSE_DB.`+ site_cdm +`.LDS_ENROLLMENT a
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        WHERE round(datediff(day,b.birth_date,a.ENR_START_DATE)/365.25) < 65
       )
       SELECT distinct
              patid,
              'MEDICARE' AS event_type,
              ENR_START_DATE AS event_date,
              age_at_event,
              '`+ site +`'
       FROM cte_enrlt65
       WHERE rn = 1;`;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1), (:2), (:3), (:4), (:5);`,
                        binds: [sqlstmt_par_dx,sqlstmt_par_drx,sqlstmt_par_prx,sqlstmt_par_px,sqlstmt_par_enr]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par_dx = snowflake.createStatement({sqlText: sqlstmt_par_dx}); run_sqlstmt_par_dx.execute();
        var run_sqlstmt_par_drx = snowflake.createStatement({sqlText: sqlstmt_par_drx}); run_sqlstmt_par_drx.execute();
        var run_sqlstmt_par_px = snowflake.createStatement({sqlText: sqlstmt_par_px}); run_sqlstmt_par_px.execute();
        
        if(site === 'CMS'){
            var run_sqlstmt_par_enr = snowflake.createStatement({sqlText: sqlstmt_par_enr}); run_sqlstmt_par_enr.execute();
        }else{
            var run_sqlstmt_par_prx = snowflake.createStatement({sqlText: sqlstmt_par_prx}); run_sqlstmt_par_prx.execute();
        }
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

/*test*/
-- call get_als_event_long(
--     array_construct(
--          'CMS'
--         ,'MU'
--     ),
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;

create or replace table ALS_EVENT_LONG (
    PATID varchar(50) NOT NULL,
    EVENT_TYPE varchar(20),
    EVENT_DATE date,     
    AGE_AT_EVENT integer,
    EVENT_SRC varchar(10)
);
call get_als_event_long(
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

create or replace table ALS_CASE_TABLE1 as
with grp_by_type as (
      select patid, 
             event_type,
             event_date,
             age_at_event,
             event_src,
             count(distinct event_type) over (partition by patid) as distinct_event_cnt,
             count(distinct event_date) over (partition by patid) as distinct_date_cnt,
             count(distinct event_date) over (partition by patid,event_type) as event_distinct_date_cnt,

             row_number() over (partition by patid order by event_date) as rn
      from ALS_EVENT_LONG
), summ_event_str as (
    select g.*,
           max(case when b.chart = 'Y' then 1 else 0 end) over (partition by a.patid) as complt_ind,
           max(case when b.enr_start_date is null then 1 else 0 end) over (partition by a.patid) as ehr_ind,
           listagg(distinct g.event_type||g.event_distinct_date_cnt,'|') within group (order by g.event_type||g.event_distinct_date_cnt) over (partition by g.patid) as event_str
    from grp_by_type g
    left join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b 
    on a.patid = b.patid
), get_als1dx as(
    select patid,
           event_date as als1dx_date,
           age_at_event as age_at_als1dx
    from(
      select a.*, row_number() over (partition by a.patid order by a.event_date) rn
      from ALS_EVENT_LONG a
      where a.event_type = 'DX'
    )
    where rn = 1
)
select a.patid 
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
      ,case when a.complt_ind = 1 then 'complete'
            when a.complt_ind = 0 and a.ehr_ind = 1 then 'enr_only'
            else 'cms_only'
       end as complt_flag
      ,case when a.distinct_event_cnt > 2 then 'confirmed'
            else 'likely'
       end as case_assert 
from summ_event_str a 
join identifier($omop_person) b on a.patid = b.patid
join identifier($omop_concept) zg on b.gender_concept_id = zg.concept_id
join identifier($omop_concept) zr on b.race_concept_id = zr.concept_id
join identifier($omop_concept) ze on b.ethnicity_concept_id = ze.concept_id
join get_als1dx c on a.patid = c.patid
where a.rn = 1 and b.birth_datetime is not null
      and a.distinct_date_cnt > 1 
      and (a.event_str like '%DX%' and a.event_str <> 'DX1') -- at least likely, only 1 DX is considered "undetermined"
;


select * from ALS_TABLE1 limit 5;

select count(distinct patid), count(*) from ALS_TABLE1;
-- 24443

select case_assert, count(distinct patid) 
from ALS_TABLE1
group by case_assert
;
-- confirmed	11798
-- likely	12645

select race, count(distinct patid) 
from ALS_TABLE1
group by race
;
-- NI	2070
-- black	1358
-- asian	267
-- white	20551
-- other	197

select case_assert, complt_flag, count(distinct patid) 
from ALS_TABLE1
group by case_assert, complt_flag
order by case_assert, complt_flag
;

-- confirmed	cms_only	4068
-- confirmed	complete	5510
-- confirmed	enr_only	1717
-- likely	cms_only	6261
-- likely	complete	4159
-- likely	enr_only	1793

