/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_px.sql
*/
-- select * from NPPES_NPI_REGISTRY.NPPES_FEB.NPI_TAXONOMY
-- where PROVIDER_TAXONOMY_CODE = '2084N0400X';
create or replace table NPI_TAXONOMY_DEDUP as
select PROVIDER_TAXONOMY_CODE, 
       listagg(distinct MEDICARE_PROVIDER_TYPE_DESCRIPTION, '|') within group (order by MEDICARE_PROVIDER_TYPE_DESCRIPTION) as provider_type
from NPPES_NPI_REGISTRY.NPPES_FEB.NPI_TAXONOMY
group by PROVIDER_TAXONOMY_CODE
;

create or replace procedure get_px_long(
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

       // collect all procedure codes
       sqlstmt = `
              INSERT INTO ALS_ALL_PX
              select  distinct
                      a.PATID
                     ,d.PROCEDURESID
                     ,datediff(day,a.index_date,NVL(d.PX_DATE::date,d.ADMIT_DATE::date)) as DAYS_SINCE_INDEX
                     ,d.PX
                     ,d.PX_TYPE
                     ,d.PPX
                     ,d.PX_DATE
                     ,d.ADMIT_DATE
                     ,d.ENC_TYPE
                     ,p.PROVIDER_NPI
                     ,t.provider_type as PROVIDER_SPECIALTY
                     ,case when d.PX in ('99201','99202','99203','99204','99205',
                                         '99206','99207','99208','99209','99210',
                                         '99211','99212','99213','99214','99215') then 1 
                      else 0 end as OFFICE_FLAG
                     ,'`+ site +`' PX_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_PROCEDURES d 
                     on a.PATID = d.PATID
              left join GROUSE_DB.`+ site_cdm +`.LDS_PROVIDER p 
                     on d.PROVIDERID = p.PROVIDERID
              left join NPI_TAXONOMY_DEDUP t 
                     on p.PROVIDER_SPECIALTY_PRIMARY = t.PROVIDER_TAXONOMY_CODE;
       `;

       if (DRY_RUN) {
              // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
              var log_stmt = snowflake.createStatement({
                            sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1);`,
                            binds: [sqlstmt]});
        log_stmt.execute(); 
       } else {
              // run dynamic dml query
              var run_sqlstmt = snowflake.createStatement({sqlText: sqlstmt}); run_sqlstmt.execute();
              var commit_txn = snowflake.createStatement({sqlText: `commit;`}); commit_txn.execute();
       }
}
$$
;

/* test */
-- call get_px_long(
--        'ALS_TABLE1',
--        array_construct(
--               'CMS'
--              ,'MU'
--        ),
--        True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;

/* main */
create or replace table ALS_ALL_PX (
    PATID varchar(50) NOT NULL,
    PROCEDURESID varchar(500) NOT NULL,
    DAYS_SINCE_INDEX number,
    PX varchar(20),
    PX_TYPE varchar(20),
    PPX varchar(20),
    PX_DATE date,
    ADMIT_DATE date,
    ENC_TYPE varchar(20),
    PROVIDER_NPI varchar(11),
    PROVIDER_SPECIALTY varchar(2000),
    OFFICE_FLAG number,
    PX_SRC varchar(20)
);
call get_px_long(
       'ALS_TABLE1',
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

select * from ALS_ALL_PX limit 5;

select PROVIDER_SPECIALTY, count(distinct patid)
from als_all_px
group by PROVIDER_SPECIALTY
order by count(distinct patid) desc;

select OFFICE_FLAG, count(distinct patid)
from als_all_px
group by OFFICE_FLAG
order by count(distinct patid) desc;

select count(distinct patid) from ALS_ALL_PX 
where px like '%F';

select PX, count(distinct patid) from ALS_ALL_PX
where px like '%F'
group by PX
order by count(distinct patid) desc;

select count(distinct patid) from ALS_ALL_PX 
where px like '%T';

select PX, count(distinct patid) from ALS_ALL_PX
group by PX
order by count(distinct patid) desc;

select count(distinct CPT_RANGE), count(*) from ONTOLOGY.GROUPER_VALUESETS.CPT_CCS;
create or replace table CPT_CCS_DEDUP as 
with cte_ord as (
       select a.*, row_number() over (partition by a.CPT_RANGE order by a.vrsn desc) rn
       from ONTOLOGY.GROUPER_VALUESETS.CPT_CCS a 
)
select CPT_RANGE,
       CCSLVL,
       CCSLVL_LABEL,
       CPT_LB,
       CPT_UB
from cte_ord 
where rn = 1
;

create or replace table ALS_ALL_PX_CCS as
with px_grp as (
    select b.*, a.ccslvl::varchar as ccs_pxgrpcd, a.ccslvl_label as ccs_pxgrp
    from ALS_ALL_PX b
    join CPT_CCS_DEDUP a 
    on to_double(b.PX) between to_double(a.cpt_lb) and to_double(a.cpt_ub) 
       and b.PX_TYPE = 'CH' 
       and regexp_like(b.PX,'^[[:digit:]]+$') 
       and regexp_like(a.cpt_lb,'^[[:digit:]]+$')
    union 
    select b.*, a.ccslvl::varchar as ccs_pxgrpcd, a.ccslvl_label as ccs_pxgrp
    from ALS_ALL_PX b 
    join CPT_CCS_DEDUP a 
    on b.PX = a.cpt_lb 
       and b.PX_TYPE = 'CH' 
       and not regexp_like(a.cpt_lb,'^[[:digit:]]+$')
    union
    select b.*, replace(a.ccs_slvl1,'''','') as ccs_pxgrpcd, a.ccs_slvl1label as ccs_pxgrp
    from ALS_ALL_PX b 
    join ONTOLOGY.GROUPER_VALUESETS.ICD9PX_CCS a 
    on replace(b.PX,'.','') = replace(a.ICD9,'''','') 
       and b.PX_TYPE = '09'
    -- union 
    -- select px_cte.*, replace(c.ccsr,'''','') as px_grpcd, c.ccsr_label as px_grp
    -- from px_cte join ONTOLOGY.GROUPER_VALUESETS.ICD10PCS_CCS c 
    -- on replace(px_cte.PX,'.','') = replace(c.ICD10PCS,'''','')  and px_cte.PX_TYPE = '10'
), ccs_fill_na as (
       select distinct px_grp.* from px_grp
       union 
       select distinct a.*,'00000','NI' from ALS_ALL_PX a
       where not exists (
              select 1 from px_grp where px_grp.proceduresid = a.proceduresid
       ) and a.PX_TYPE <> 'RE'
)
select distinct
       patid,
       px,
       px_type,
       ccs_pxgrpcd,
       ccs_pxgrp,
       px_date,
       days_since_index
from ccs_fill_na
;

select count(distinct patid), count(distinct ccs_pxgrpcd), count(*) from ALS_ALL_PX_CCS;
-- 23368	245	10523605

select * from ALS_ALL_PX_CCS;

select ccs_pxgrpcd, count(distinct patid) from ALS_ALL_PX_CCS
group by ccs_pxgrpcd
order by ccs_pxgrpcd;

select * from ALS_ALL_PX_CCS limit 5;

create or replace table ALS_SEL_PX_MDC as 
select distinct 
       PATID,
       '99999' as CCS_PXGRPCD,
       'possible mdc care' as CCS_PXGRP,
       PX_DATE,
       DAYS_SINCE_INDEX
from ALS_ALL_PX a  
where a.PX in (
       -- als multidisciplinary care updated or planned
       '0580F','H2000',
       -- interdisciplinary care
       '99366','99367','99368',
       -- complex chronic care coordination
       '99487','99488','99489',
       -- prolonged services
       '99354','99355'
)
;
select * from ALS_SEL_PX_MDC;
select count(distinct patid), count(*) from ALS_SEL_PX_MDC;

insert into ALS_ALL_PX_CCS(patid, ccs_pxgrpcd, ccs_pxgrp, px_date, days_since_index)
select * from ALS_SEL_PX_MDC
;

select ccs_pxgrpcd, count(distinct patid) from ALS_ALL_PX_CCS
group by ccs_pxgrpcd
order by ccs_pxgrpcd;