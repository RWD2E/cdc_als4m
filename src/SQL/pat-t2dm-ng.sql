/* 
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: pat-glp1.sql
# Description: identify all patients who have ever used GLP1 agonist or DPP4 inhibitor
# Dependency: vs-mmm-cde,vs-t2dm-cde
*/

create or replace procedure get_dm_event_long(
    SITES array,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * Stored procedure to collect a long tables for all GLP1 or DPP4 prescription or dispense:
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
        INSERT INTO DM_EVENT_LONG
        SELECT  distinct
                a.patid,
                'DX' AS event_type,
                a.dx AS event_val,
                NVL(a.dx_date,a.admit_date) AS event_date,
                round(datediff(day,b.birth_date,NVL(a.dx_date,a.admit_date))/365.25) AS age_at_event,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DIAGNOSIS a
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid
        WHERE (
            a.dx like '250%' OR 
            split_part(a.dx,'.',1) in ('E08','E09','E10','E11','E12','E13')
        )
        AND 
        a.patid is not null
        `;
    
    var sqlstmt_par_lab = `
       INSERT INTO DM_EVENT_LONG
       SELECT  distinct
               l.patid,
               'LAB' AS event_type,
               to_char(round(l.RESULT_NUM,1)) || l.RESULT_UNIT AS event_val,
               Coalesce(l.specimen_date,l.lab_order_date,l.result_date) AS event_date,
               round(datediff(day,b.birth_date,Coalesce(l.specimen_date,l.lab_order_date,l.result_date))/365.25) AS age_at_event,
               '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_LAB_RESULT_CM l
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON l.patid = b.patid
        WHERE (
            (
                UPPER(l.RAW_LAB_NAME) like '%HEMOGLOBIN%A1C%' OR 
                UPPER(l.RAW_LAB_NAME) like '%A1C%' OR 
                UPPER(l.RAW_LAB_NAME) like '%HA1C%' OR 
                UPPER(l.RAW_LAB_NAME) like '%HBA1C%'
            ) OR (
                l.LAB_LOINC IN (
                    '17855-8','4548-4','4549-2','17856-6',
                    '41995-2','59261-8','62388-4','71875-9','54039-3'
                )
            )
        )
        AND l.RESULT_NUM > 6.5
        -- AND UPPER(l.RESULT_UNIT) in ('%','PERCENT')
        AND l.patid is not null
        `;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1), (:2);`,
                        binds: [sqlstmt_par_dx,sqlstmt_par_lab]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par_dx = snowflake.createStatement({sqlText: sqlstmt_par_dx}); run_sqlstmt_par_dx.execute();
        
        if(site != 'CMS'){
            var run_sqlstmt_par_lab = snowflake.createStatement({sqlText: sqlstmt_par_lab}); run_sqlstmt_par_lab.execute();
        }
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

/*test*/
-- call get_dm_event_long(
--     array_construct(
--          'CMS'
--         ,'MU'
--     ),
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;
create or replace table DM_EVENT_LONG (
    PATID varchar(50) NOT NULL,
    EVENT_TYPE varchar(20),
    EVENT_VAL varchar(20),
    EVENT_DATE date,     
    AGE_AT_EVENT integer,
    EVENT_SRC varchar(10)
);
call get_dm_event_long(
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

select count(*), count(distinct patid) from DM_EVENT_LONG;
--8272261

create or replace table DM_TABLE1 as
with cte_ord as (
    select patid, 
           event_type,
           event_date,
           age_at_event,
           event_src,
           count(distinct event_val) over (partition by patid) as distinct_event_cnt,
           count(distinct event_date) over (partition by patid) as distinct_date_cnt,
           listagg(distinct event_type || event_val, '|') within group (order by event_type || event_val) over (partition by patid) as event_str,
        --    listagg(distinct event_type, '|') within group (order by event_type) over (partition by patid) as event_str,
           row_number() over (partition by patid order by event_date) as rn
    from DM_EVENT_LONG
), cte_dx1 as(
    select patid,
           min(event_date) as dm1dx_date
    from DM_EVENT_LONG
    where event_type = 'DX'
    group by patid
)
select a.patid 
      ,b.birth_date
      ,b.sex
      ,b.race 
      ,b.hispanic
      ,c.dm1dx_date
      ,a.event_type as index_event
      ,a.event_date as index_date
      ,a.age_at_event as age_at_index
      ,a.event_src as index_src
      ,a.distinct_event_cnt
      ,a.distinct_date_cnt
      ,a.event_str
from cte_ord a 
join PAT_TABLE1 b on a.patid = b.patid
join cte_dx1 c on a.patid = c.patid
where a.rn = 1 and b.birth_date is not null
      and a.distinct_date_cnt > 2  -- required at least another assertainment event at different time
      and a.event_str like '%DX%'  -- at least 1 confirmed diagnosis
;
select * from DM_TABLE1 limit 5;

select count(*), count(distinct patid) from DM_TABLE1;
-- 6,252,886

select * from DM_TABLE1 limit 5;