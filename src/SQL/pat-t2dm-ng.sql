/* 
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: pat-glp1.sql
# Description: identify all patients who have ever used GLP1 agonist or DPP4 inhibitor
# Dependency: vs-mmm-cde,vs-t2dm-cde
*/
create or replace table T2DM_EVENT_LONG (
    PATID varchar(50) NOT NULL,
    RXID varchar(200),
    EVENT_TYPE varchar(10) NOT NULL,
    AGE_AT_EVENT integer,
    RX_CLS varchar(5),
    RX_IN varchar(20),
    RX_START_DATE date, 
    RX_END_DATE date, 
    RX_DOSE varchar(50),
    RX_DOSE_UNIT varchar(50),
    RX_AMT integer,
    RX_SUP integer,
    RX_REFILL integer,
    RX_FREQUENCY varchar(5),
    RAW_RX_CODE varchar(20),
    RX_SRC varchar(10)
);
create or replace procedure get_t2dm_event_long(
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
    var sqlstmt_par_drx = `
        INSERT INTO T2DM_EVENT_LONG
        SELECT  a.patid,
                a.dispensingid as RXID,
                'DRX' AS event_type,
                round(datediff(day,b.birth_date,a.dispense_date)/365.25) AS age_at_event,
                d.RX_CLS,
                d.RX_IN,
                a.dispense_date AS RX_START_DATE,
                NULL as RX_END_DATE,
                a.dispense_dose_disp AS RX_DOSE,
                coalesce(a.dispense_dose_disp_unit,a.raw_dispense_dose_disp_unit) AS RX_DOSE_UNIT,
                try_to_number(a.dispense_amt::integer) AS RX_AMT,
                try_to_number(a.dispense_sup::integer) AS RX_SUP,         
                0 as RX_REFILL,
                NULL as RX_FREQUENCY,
                a.NDC as RAW_RX_CODE,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_DISPENSING a
        JOIN NDC_REF d ON a.NDC = d.NDC
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;
        `;
    
    var sqlstmt_par_prx = `
       INSERT INTO GLP1_EVENT_LONG
        SELECT  a.patid,
                a.prescribingid as RXID,
                'PRX' AS event_type,
                round(datediff(day,b.birth_date,coalesce(a.rx_order_date,a.rx_start_date))/365.25) AS age_at_event,
                p.RX_CLS,
                p.RX_IN,
                coalesce(a.rx_order_date,a.rx_start_date) AS RX_START_DATE,
                coalesce(a.rx_end_date,a.rx_start_date) AS RX_END_DATE,
                a.rx_dose_ordered AS RX_DOSE,
                coalesce(a.rx_dose_ordered_unit,a.raw_rx_dose_ordered_unit) AS RX_DOSE_UNIT,
                coalesce(try_to_number(a.rx_quantity::integer),try_to_number(a.raw_rx_quantity)) AS RX_AMT,
                try_to_number(a.rx_days_supply::integer) AS RX_SUP,  
                try_to_number(a.rx_refills::integer) AS RX_REFILL,
                coalesce(a.rx_frequency,a.raw_rx_frequency) AS RX_FREQUENCY,
                a.RXNORM_CUI as RAW_RX_CODE,
                '`+ site +`'
        FROM GROUSE_DB.`+ site_cdm +`.LDS_PRESCRIBING a
        JOIN RXCUI_REF p ON a.RXNORM_CUI = p.RXCUI
        JOIN GROUSE_DB.`+ site_cdm +`.LDS_DEMOGRAPHIC b ON a.patid = b.patid;
        `;

    if (DRY_RUN) {
        // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
        var log_stmt = snowflake.createStatement({
                        sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1), (:2);`,
                        binds: [sqlstmt_par_drx,sqlstmt_par_prx]});
        log_stmt.execute(); 
    } else {
        // run dynamic dml query
        var run_sqlstmt_par_drx = snowflake.createStatement({sqlText: sqlstmt_par_drx}); run_sqlstmt_par_drx.execute();
        
        if(site != 'CMS'){
            var run_sqlstmt_par_prx = snowflake.createStatement({sqlText: sqlstmt_par_prx}); run_sqlstmt_par_prx.execute();
        }
        var commit_txn = snowflake.createStatement({sqlText: `commit;`}); 
        commit_txn.execute();
    }
}
$$
;

/*test*/
-- call get_glp1_event_long(
--     array_construct(
--          'CMS'
--         ,'MU'
--     ),
--     TRUE,'TMP_SP_OUTPUT'
-- )
-- ;
-- select * from TMP_SP_OUTPUT;

truncate T2DM_EVENT_LONG;
call get_glp1_event_long(
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

