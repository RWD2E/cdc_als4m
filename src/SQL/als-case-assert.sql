/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_CDC-alg.sql
# Description: multifactorial computable phenotype for identifying ALS patients (CDC alg)
# Dependency: pat-demo-master.sql, PAT_TABLE1
*/

use role GROUSE_ROLE_C_ANALYTICS;
use warehouse GROUSE_WH;
use database GROUSE_DEID_ANALYTICS_DB; -- write-premitted database
use schema SX_ALS_GPC;

create or replace table FDA_MEDS_RXCUI as 
select distinct     
       STR
      ,RXCUI
      ,SUPPRESS
      ,TTY
      ,'Riluzole' as GN
from  ontology.rxnorm.rxnconso
where lower(STR) like '%riluz%'
--    and
--    (
--       -- https://www.nlm.nih.gov/research/umls/rxnorm/docs/appendix5.html
--       TTY like 'SCD%' OR --Semantic Clinical Drug or Group
--       TTY like 'SBD%' OR --Semantic Branded Drug or Group
--       TTY like '%PCK' OR -- Generic or Branded drug pack
--       TTY like '%IN'-- Ingredient
--    )
union
select distinct     
         STR
        ,RXCUI
        ,SUPPRESS
        ,TTY
        ,'Edarovone' as GN
    from  ontology.rxnorm.rxnconso
    where lower(STR) like '%edarav%'
union
select distinct     
         STR
        ,RXCUI
        ,SUPPRESS
        ,TTY
        ,'Tofersen' as GN
    from  ontology.rxnorm.rxnconso
where lower(STR) like '%tofersen%'
union
   select distinct     
       STR
      ,RXCUI
      ,SUPPRESS
      ,TTY
      ,'Relyvrio' as GN
from  ontology.rxnorm.rxnconso
where (lower(STR) like '%sodium phenylbutyrate%' AND lower(STR) like '%taurursodiol%') OR 
       lower(STR) like '%relyvrio%'
;

select * from FDA_MEDS_RXCUI;

create or replace table FDA_MEDS_NDC as
with rxcui_unique as (
    select distinct GN, RXCUI 
    from FDA_MEDS_RXCUI
)
select distinct
       rxn.RXCUI
      ,rxn.GN
      ,rxmap.ATV as NDC
      ,rxmap.SUPPRESS
from rxcui_unique rxn
join ontology.rxnorm.rxnsat rxmap
on rxn.RXCUI = rxmap.RXCUI and
   rxmap.ATN = 'NDC'and rxmap.SAB = 'RXNORM' -- normalized 11-digit NDC codes
;

create or replace table NEURO_TAXONOMY as
select distinct
       PROVIDER_TAXONOMY_CODE AS PROVIDER_SPECIALTY_PRIMARY,
       PROVIDER_TAXONOMY_DESCRIPTION
from NPPES_NPI_REGISTRY.NPPES_FEB.NPI_TAXONOMY
where upper(PROVIDER_TAXONOMY_DESCRIPTION) like '%NEUROLOGY%'
;

select * from NEURO_TAXONOMY;

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
        SELECT  a.patid,
                'DX' AS event_type,
                NVL(a.dx_date,a.admit_date) AS event_date,
                round(datediff(day,b.birth_date,NVL(a.dx_date,a.admit_date))/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DIAGNOSIS a
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        WHERE a.dx LIKE '335.20%' OR a.dx LIKE 'G12.21%';`;

    var sqlstmt_par_drx = `
        INSERT INTO ALS_EVENT_LONG
        SELECT  a.patid,
                'DRX' AS event_type,
                a.dispense_date AS event_date,
                round(datediff(day,b.birth_date,a.dispense_date)/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DISPENSING a
        JOIN FDA_MEDS_NDC d ON a.NDC = d.NDC
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;`;
    
    var sqlstmt_par_prx = `
       INSERT INTO ALS_EVENT_LONG
        SELECT  a.patid,
                'PRX' AS event_type,
                coalesce(a.rx_order_date,a.rx_start_date) AS event_date,
                round(datediff(day,b.birth_date,coalesce(a.rx_order_date,a.rx_start_date))/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_PRESCRIBING a
        JOIN FDA_MEDS_RXCUI p ON a.RXNORM_CUI = p.RXCUI
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;`;
    
    var sqlstmt_par_px = `
       INSERT INTO ALS_EVENT_LONG
        SELECT  a.patid,
                'NEUROLOGIST' AS event_type,
                a.px_date AS event_date,
                round(datediff(day,b.birth_date,a.px_date)/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_PROCEDURES a
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_PROVIDER p ON a.PROVIDERID = p.PROVIDERID
        JOIN NEURO_TAXONOMY n on n.PROVIDER_SPECIALTY_PRIMARY = p.PROVIDER_SPECIALTY_PRIMARY
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        WHERE a.PX in ('99201','99202','99203','99204','99205',
                       '99206','99207','99208','99209','99210',
                       '99211','99212','99213','99214','99215');`;
    
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
       SELECT patid,
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

create or replace table ALS_TABLE1 as
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
      ,case when a.distinct_event_cnt > 1 then 'confirmed'
            else 'likely'
       end as case_assert 
from cte_ord a 
join PAT_TABLE1 b on a.patid = b.patid
join cte_dx1 c on a.patid = c.patid
where a.rn = 1 and b.birth_date is not null
      and a.distinct_date_cnt > 1 and a.event_str like '%DX%' -- at least likely
;

select * from ALS_TABLE1 limit 5;

select count(distinct patid), count(*) from ALS_TABLE1;
-- 12,490

select case_assert, count(distinct patid) 
from ALS_TABLE1
group by case_assert
;

select index_src, count(distinct patid) 
from ALS_TABLE1
group by index_src
;

select index_event, count(distinct patid) 
from ALS_TABLE1
where index_src = 'CMS'
group by index_event
;

select complt_flag, count(distinct patid) 
from ALS_TABLE1
group by complt_flag
;