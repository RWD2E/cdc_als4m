/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_dx-alg_prvd.sql
# Description: identify types and timings of providers engaged in the care process
# Dependencies: 
# - ALL_ENCOUNTER_..._ALS1DX
# - ALL_DISPENSING_..._ALS1DX
# - ALL_DIAGNOSIS_..._ALS1DX
# - ALL_PROCEDURES_..._ALS1DX with OFFICE_FLAG indicator - office visit
*/
/****************************************************************************************************************/
/*set environment*/
use role GROUSE_ROLE_C_ANALYTICS;
use warehouse GROUSE_WH;
use database GROUSE_DEID_ANALYTICS_DB; -- write-premitted database

create schema if not exists ALS_GPC;
use schema ALS_GPC;
/********************************************************************************/

create or replace procedure als_provider_type(PRVD_TYPE STRING, SRVC_SRC STRING, DRY_RUN BOOLEAN)
returns variant
language javascript
as
$$
/**stage encounter table from different CMS table
 * @param {string} PRVD_TYPE: single-word string expression for provider type (e.g., neurology, ); only understore '_' is allowed
 * @param {string} SRVC_SRC: service context (e.g., dx,px,rx,enc,office)
**/
// matching criteria
let src_cov_tbl = `ALL_`;
let office_ind = ``;
switch(SRVC_SRC){
    case 'dx':
        src_cov_tbl += `DIAGNOSIS_xxx_ALS1DX`;
        break;
    case 'px':
        src_cov_tbl += `PROCEDURES_xxx_ALS1DX`;
        break;
    case 'rx':
        src_cov_tbl += `DISPENSING_xxx_ALS1DX`;
        break;
    case 'enc':
        src_cov_tbl += `ENCOUNTER_xxx_ALS1DX`;
        break;
    case 'office':
        src_cov_tbl += `PROCEDURES_xxx_ALS1DX`;
        office_ind += ` AND OFFICE_FLAG = 1`;
        break;
}
src_bef_tbl = src_cov_tbl.replace('xxx','BEFORE')
src_aft_tbl = src_cov_tbl.replace('xxx','AFTER')
regexp_prvd_type = PRVD_TYPE.replace('_',')|(')
// generate dynamtic query
var t_qry = `CREATE OR REPLACE TABLE `+ PRVD_TYPE +`_care AS
             WITH bef_stk AS (
                SELECT patid, max(DAYS_SINCE_ALS1DX) as days_since_als1dx
                FROM `+ src_bef_tbl +`  
                WHERE lower(PROVIDER_SPECIALTY) regexp '.*((`+ regexp_prvd_type +`)+).*' `+ office_ind +`
                GROUP BY patid
             ),   aft_stk AS (
                SELECT patid, min(DAYS_SINCE_ALS1DX) as days_since_als1dx
                FROM `+ src_aft_tbl +`  
                WHERE lower(PROVIDER_SPECIALTY) regexp '.*((`+ regexp_prvd_type +`)+).*' `+ office_ind +`
                GROUP BY patid
             )
            SELECT DISTINCT a.patid,
                   b.days_since_als1dx as bef_als1dx,
                   c.days_since_als1dx as aft_als1dx
            FROM ALS_INIT_ENR a
            LEFT JOIN bef_stk b on a.patid = b.patid
            LEFT JOIN aft_stk c on a.patid = c.patid
            `;

if (DRY_RUN){
    // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
    var log_stmt = snowflake.createStatement({
                    sqlText: `create or replace temporary table temp_qry as select (:1) as qry;`,
                    binds: [t_qry]});
    log_stmt.execute(); 
}else{
    // run dynamic dml query
    var commit_txn = snowflake.createStatement({sqlText: `commit`});
    var run_collect_dml = snowflake.createStatement({sqlText: t_qry});
    run_collect_dml.execute();
    commit_txn.execute();  
}
$$
;

/* radiology */
call als_provider_type('radiology','px',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from radiology_care;

/* internal medicine, family medicine */
call als_provider_type('internal_family','office',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from internal_family_care;

/* neurology */
call als_provider_type('neurology','office',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from neurology_care;

/*neuropsych*/
call als_provider_type('neuropsych','office',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from neuropsych_care;

/*physical therapist*/
call als_provider_type('physical_rehab','px',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from physical_rehab_care;

/* gastroenterology */
call als_provider_type('gastroenterology','office',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from gastroenterology_care;

/*nurse practitioner/APP*/
call als_provider_type('nurse_assistant','px',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from nurse_assistant_care;

/*cardiology*/
call als_provider_type('cardiology','office',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from cardiology_care;

/*nutrition*/
call als_provider_type('nutrition','px',FALSE);
-- select * from temp_qry;
select count(*), count(patid) from nutrition_care;
