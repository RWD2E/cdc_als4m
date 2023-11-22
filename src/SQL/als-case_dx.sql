/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_dx.sql
# Dependency: ALS_TABLE1
*/
-- select * from NPPES_NPI_REGISTRY.NPPES_FEB.NPI_TAXONOMY
-- where PROVIDER_TAXONOMY_CODE = '2084N0400X';

create or replace table NPI_TAXONOMY_DEDUP as
select PROVIDER_TAXONOMY_CODE, 
       listagg(distinct MEDICARE_PROVIDER_TYPE_DESCRIPTION, '|') within group (order by MEDICARE_PROVIDER_TYPE_DESCRIPTION) as provider_type
from NPPES_NPI_REGISTRY.NPPES_FEB.NPI_TAXONOMY
group by PROVIDER_TAXONOMY_CODE
;

create or replace procedure get_dx_long(
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

       // collect all diagnosis codes
       sqlstmt = `
              INSERT INTO ALS_ALL_DX
              select  distinct
                      a.PATID
                     ,d.DIAGNOSISID
                     ,datediff(day,a.index_date,NVL(d.DX_DATE::date,d.ADMIT_DATE::date)) as DAYS_SINCE_INDEX
                     ,d.DX
                     ,d.DX_TYPE
                     ,d.PDX
                     ,d.DX_DATE
                     ,d.ADMIT_DATE
                     ,d.ENC_TYPE
                     ,p.PROVIDER_NPI
                     ,t.provider_type as PROVIDER_SPECIALTY
                     ,'`+ site +`' DX_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_DIAGNOSIS d 
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
-- call get_dx_long(
--        'ALS_TABLE1',
--        array_construct(
--               'CMS'
--              ,'MU'
--        ),
--        True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;

/* main */
create or replace table ALS_ALL_DX (
    PATID varchar(50) NOT NULL,
    DIAGNOSISID varchar(500) NOT NULL,
    DAYS_SINCE_INDEX number,
    DX varchar(20),
    DX_TYPE varchar(20),
    PDX varchar(20),
    DX_DATE date,
    ADMIT_DATE date,
    ENC_TYPE varchar(20),
    PROVIDER_NPI varchar(11),
    PROVIDER_SPECIALTY varchar(2000),
    DX_SRC varchar(20)
);
call get_dx_long(
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

select * from ALS_ALL_DX limit 5;

select PROVIDER_SPECIALTY, count(distinct patid)
from als_all_dx
group by PROVIDER_SPECIALTY
order by count(distinct patid) desc;

-- map to phecodes
create or replace table ALS_ALL_PHECD as
with phecd_map_cte as (
       select distinct dx.*,
              phe."phecode" as phecd_dxgrpcd, 
              ref."phecode_string" as phecd_dxgrp
       from ALS_ALL_DX dx 
       join ONTOLOGY.GROUPER_VALUESETS.ICD10CM_PHECODEX phe on dx.DX = phe."icd10" and dx.DX_TYPE = '10'
              and phe."phecode" not like '%.%'
       join ONTOLOGY.GROUPER_VALUESETS.PHECODEX_REF ref on phe."phecode" = ref."phecode" 
       union
       select distinct dx.*,
              phe."phecode" as phecd_dxgrpcd, 
              ref."phecode_string" as phecd_dxgrp
       from ALS_ALL_DX dx 
       join ONTOLOGY.GROUPER_VALUESETS.ICD9CM_PHECODEX phe on dx.DX = phe."icd9" and dx.DX_TYPE = '09'
              and phe."phecode" not like '%.%'
       join ONTOLOGY.GROUPER_VALUESETS.PHECODEX_REF ref on phe."phecode" = ref."phecode"
), phecd_fill_na as (
       select * from phecd_map_cte
       union 
       select dx.*,'00000', 'NI'
       from ALS_ALL_DX dx
       where not exists (select 1 from phecd_map_cte cte where cte.diagnosisid = dx.diagnosisid) 
             and dx.dx not in ('335.20','I12.21')
)
select distinct
       patid,
       dx,
       dx_type,
       phecd_dxgrpcd,
       phecd_dxgrp,
       dx_date,
       days_since_index
from phecd_fill_na
;

select count(distinct patid), count(distinct phecd_dxgrpcd) from ALS_ALL_PHECD;
-- 12490	678

-- map to CCS category
create or replace table ALS_ALL_DX_CCS as 
with ccs_map_cte as (
    select distinct dx.*,
           replace(ccs.ccs_slvl1,'''','') as ccs_dxgrpcd, 
           ccs.ccs_slvl1label as ccs_dxgrp
    from ALS_ALL_DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD10CM_CCS ccs 
    on replace(dx.DX,'.','') = replace(ccs.ICD10CM,'''','') and dx.DX_TYPE = '10'
    union
    select distinct dx.*,
           replace(icd9.ccs_slvl1,'''','') as ccs_dxgrpcd, 
           icd9.ccs_slvl1label as ccs_dxgrp
    from ALS_ALL_DX dx 
    join ONTOLOGY.GROUPER_VALUESETS.ICD9DX_CCS icd9 
    on rpad(replace(dx.DX,'.',''),5,'0') = replace(icd9.ICD9,'''','') and dx.DX_TYPE = '09'
), ccs_fill_na as (
    select * from ccs_map_cte
    union 
    select dx.*,'00000', 'NI'
    from ALS_ALL_DX dx
    where not exists (select 1 from ccs_map_cte cte where cte.diagnosisid = dx.diagnosisid) 
          and dx.dx not in ('335.20','I12.21')
)
select distinct
       patid,
       dx,
       dx_type,
       ccs_dxgrpcd,
       ccs_dxgrp,
       dx_date,
       days_since_index
from ccs_fill_na
;

select count(distinct patid) from ALS_ALL_DX_CCS;