/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_sdoh.sql
# Description: extract other covariates for ALS case cohort
*/

select * from GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY limit 5;
select * from GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_GEOCODE limit 5; 

create or replace table ALS_ALL_GEOID as
-- census tract level 
select distinct
       a.patid, 
       b.address_period_start,
       datediff(day,a.index_date,b.address_period_start) as days_since_index,
       b.address_period_end,
       c.geocodeid,
       c.geo_accuracy
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY b on a.patid = b.patid
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_GEOCODE c on b.addressid = c.addressid
;

select count(distinct patid), count(*) from ALS_ALL_GEOID;
-- 20076

create or replace procedure get_sdoh_s(
    TGT_TABLE string,
    REF_COHORT string,
    REF_PKEY string,
    SDOH_TBLS array,
    DRY_RUN boolean,
    DRY_RUN_AT string
)
returns variant
language javascript
as
$$
/**
 * @param {string} TGT_TABLE: name of target sdoh collection table
 * @param {string} REF_COHORT: name of reference patient table (absolute path/full name), should at least include (patid)
 * @param {string} REF_PKEY: primary key column in REF_COHORT table for matchin with SDOH tables
 * @param {array} SDOH_TBLS: an array of tables in SDOH_DB
 * @param {boolean} DRY_RUN: dry run indicator. If true, only sql script will be created and stored in dev.sp_out table
 * @param {boolean} DRY_RUN_AT: A temporary location to store the generated sql query for debugging purpose. 
                                When DRY_RUN = True, provide absolute path (full name) to the table; when DRY_RUN = False, provide NULL 
**/
if (DRY_RUN) {
    var log_stmt = snowflake.createStatement({
        sqlText: `CREATE OR REPLACE TEMPORARY TABLE `+ DRY_RUN_AT +`(QRY VARCHAR);`});
    log_stmt.execute(); 
}

// collect all sdoh tables and their columns
var sdoh_tbl_quote = SDOH_TBLS.map(item => `'${item}'`)
var get_tbl_cols_qry = `
    SELECT a.table_schema, a.table_name, listagg(a.column_name,',') AS enc_col, b.var_type
    FROM SDOH_DB.information_schema.columns a
    JOIN SX_SDOH.S_SDH_SEL b
    ON a.table_schema = b.var_domain and 
       a.table_name = b.var_subdomain and 
       a.column_name = b.var
    WHERE a.table_name in (`+ sdoh_tbl_quote +`) 
    GROUP BY a.table_schema, a.table_name, b.var_type;`
var get_tbl_cols = snowflake.createStatement({sqlText: get_tbl_cols_qry});
var tables = get_tbl_cols.execute();

// loop over listed tables
while (tables.next()){
    var schema = tables.getColumnValue(1);
    var table = tables.getColumnValue(2);
    var cols = tables.getColumnValue(3).split(",");
    var cols_alias = cols.map(value => {return 'b.'+ value});
    var type = tables.getColumnValue(4);

    // keep records in original categorical format
    var sqlstmt = `
        insert into `+ TGT_TABLE +`(PATID,ADDRESS_PERIOD_START,DAYS_SINCE_INDEX,ADDRESS_PERIOD_END,GEOCODEID,GEO_ACCURACY,SDOH_VAR,SDOH_VAL,SDOH_TYPE,SDOH_SRC)
        select  PATID,
                ADDRESS_PERIOD_START,
                DAYS_SINCE_INDEX,
                ADDRESS_PERIOD_END,
                GEOCODEID,
                GEO_ACCURACY,
                SDOH_VAR,
                SDOH_VAL,
                '`+ type +`' as SDOH_TYPE,
                '`+ schema +`' as SDOH_SRC
        from (
            select  a.patid,
                    a.address_period_start,
                    a.days_since_index,
                    a.address_period_end, 
                    b.geocodeid,
                    b.geo_accuracy,
                    `+ cols_alias +`
            from `+ REF_COHORT +` a 
            join SDOH_DB.`+ schema +`.`+ table +` b 
            on startswith(a.`+ REF_PKEY +`,b.geocodeid)
            where length(b.geocodeid) > 9 -- excluding zip, fips-st, fips-cty
        )
        unpivot 
        (
            SDOH_VAL for SDOH_VAR in (`+ cols +`)
        )
        where SDOH_VAL is not null
    `;

    var run_sqlstmt = snowflake.createStatement({sqlText: sqlstmt});

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                    sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                    binds: [sqlstmt]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        try{run_sqlstmt.execute();} catch(error) {};
        commit_txn.execute();
    }
}
$$
;

/* test */
-- call get_sdoh_s(
--        'ALS_ALL_SDOH',
--        'ALS_ALL_GEOID',
--        'GEOCODEID',
--        array_construct(
--               'RUCA_TR_2010'
--              ,'SVI_TR_2020'
--        ),
--        True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;
create or replace table ALS_ALL_SDOH (
        PATID varchar(50) NOT NULL
       ,ADDRESS_PERIOD_START date 
       ,DAYS_SINCE_INDEX number
       ,ADDRESS_PERIOD_END date
       ,GEOCODEID varchar(15)
       ,GEO_ACCURACY varchar(3)
       ,SDOH_VAR varchar(100)
       ,SDOH_VAL varchar(1000)
       ,SDOH_TYPE varchar(2)
       ,SDOH_SRC varchar(10)
);
call get_sdoh_s(
       'ALS_ALL_SDOH',
       'ALS_ALL_GEOID',
       'GEOCODEID',
       array_construct(
              'ACS_TR_2015_2019'
             ,'ADI_BG_2020'
             ,'RUCA_TR_2010'
             ,'SVI_TR_2020'
             ,'FARA_TR_2019'
       --       ,'MUA_X_2024'
       --       ,'MUP_X_2024'
       --       ,'SLD_BG_2021'
       ),
       FALSE, NULL
);

select count(distinct patid),count(*) from ALS_ALL_SDOH;
-- 20076,28492975

select count(distinct patid)
from ALS_ALL_SDOH
where sdoh_var = 'ADI_NATRANK'
;

INSERT INTO ALS_ALL_SDOH(patid,address_period_start,days_since_index,address_period_end,geocodeid,geo_accuracy,sdoh_var,sdoh_val,sdoh_type,sdoh_src) 
select distinct
       a.patid, 
       b.enr_start_date as address_period_start,
       datediff(day,a.index_date,b.enr_start_date) as days_since_index,
       b.enr_end_date as address_period_end,
       '0' as geocodeid,
       'PS' as geo_accuracy, 
       'LIS_DUAL' as sdoh_var,
       1 as sdoh_val,
       'C' as sdoh_type,
       'CMS' as sdoh_src
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b
on a.patid = b.patid
where b.raw_basis in ('LIS','DUAL')
;

INSERT INTO ALS_ALL_SDOH(patid,address_period_start,days_since_index,address_period_end,geocodeid,geo_accuracy,sdoh_var,sdoh_val,sdoh_type,sdoh_src) 
select distinct
       a.patid, 
       b.enr_start_date as address_period_start,
       datediff(day,a.index_date,b.enr_start_date) as days_since_index,
       b.enr_end_date as address_period_end,
       '0' as geocodeid,
       'PS' as geo_accuracy, 
       'PART_C' as sdoh_var,
       1 as sdoh_val,
       'C' as sdoh_type,
       'CMS' as sdoh_src
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b
on a.patid = b.patid
where b.raw_basis = 'C'
;

INSERT INTO ALS_ALL_SDOH(patid,address_period_start,days_since_index,address_period_end,geocodeid,geo_accuracy,sdoh_var,sdoh_val,sdoh_type,sdoh_src) 
select distinct 
       a.patid, 
       b.enr_start_date as address_period_start,
       datediff(day,a.index_date,b.enr_start_date) as days_since_index,
       b.enr_end_date as address_period_end,
       '0' as geocodeid,
       'PS' as geo_accuracy, 
       'PART_D' as sdoh_var,
       1 as sdoh_val,
       'C' as sdoh_type,
       'CMS' as sdoh_src
from ALS_TABLE1 a
join GROUSE_DB.CMS_PCORNET_CDM.LDS_ENROLLMENT b
on a.patid = b.patid
where b.enr_basis = 'D'
;

select count(distinct patid), count(distinct sdoh_var), count(*) from ALS_ALL_SDOH;
-- 20077 200	28526514

create or replace table ALS_SEL_SDOH as
with cte_stk as(
       select patid, 
              address_period_start as OBSCOMM_DATE,
              DAYS_SINCE_INDEX,
              SDOH_VAR as OBSCOMM_CODE,
              SDOH_VAL as OBSCOMM_RESULT
       from ALS_ALL_SDOH
       where sdoh_var in (
              'LIS_DUAL',
              'PART_C',
              'PART_D',
              'ADI_NATRANK'
       )
       union 
       select distinct
              PATID,
              address_period_start as OBSCOMM_DATE,
              DAYS_SINCE_INDEX,
              SDOH_VAR as OBSCOMM_CODE,
              case when SDOH_VAL in ('1','2','3') then 'metro'
                   when SDOH_VAL in ('4','5','6') then 'micro'
                   when SDOH_VAL in ('7','8','9','10') then 'small'
              end as OBSCOMM_RESULT
       from ALS_ALL_SDOH
       where SDOH_VAR = 'RUCA_PRIMARY'
)
select distinct
       PATID,
       OBSCOMM_CODE,
       OBSCOMM_RESULT,
       OBSCOMM_DATE,
       DAYS_SINCE_INDEX
from cte_stk
;

select count(distinct patid), count(distinct OBSCOMM_CODE), count(*) from ALS_SEL_SDOH;