/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_dx.sql
*/
-- create or replace table FDA_MEDS_RXCUI as 
-- select distinct     
--        STR
--       ,RXCUI
--       ,SUPPRESS
--       ,TTY
--       ,'Riluzole' as GN
-- from  ontology.rxnorm.rxnconso
-- where lower(STR) like '%riluz%'
-- --    and
-- --    (
-- --       -- https://www.nlm.nih.gov/research/umls/rxnorm/docs/appendix5.html
-- --       TTY like 'SCD%' OR --Semantic Clinical Drug or Group
-- --       TTY like 'SBD%' OR --Semantic Branded Drug or Group
-- --       TTY like '%PCK' OR -- Generic or Branded drug pack
-- --       TTY like '%IN'-- Ingredient
-- --    )
-- union
-- select distinct     
--          STR
--         ,RXCUI
--         ,SUPPRESS
--         ,TTY
--         ,'Edarovone' as GN
--     from  ontology.rxnorm.rxnconso
--     where lower(STR) like '%edarav%'
-- union
-- select distinct     
--          STR
--         ,RXCUI
--         ,SUPPRESS
--         ,TTY
--         ,'Tofersen' as GN
--     from  ontology.rxnorm.rxnconso
-- where lower(STR) like '%tofersen%'
-- union
--    select distinct     
--        STR
--       ,RXCUI
--       ,SUPPRESS
--       ,TTY
--       ,'Relyvrio' as GN
-- from  ontology.rxnorm.rxnconso
-- where (lower(STR) like '%sodium phenylbutyrate%' AND lower(STR) like '%taurursodiol%') OR 
--        lower(STR) like '%relyvrio%'
-- ;

-- create or replace table FDA_MEDS_NDC as
-- with rxcui_unique as (
--     select distinct GN, RXCUI 
--     from FDA_MEDS_RXCUI
-- )
-- select distinct
--        rxn.RXCUI
--       ,rxn.GN
--       ,rxmap.ATV as NDC
--       ,rxmap.SUPPRESS
-- from rxcui_unique rxn
-- join ontology.rxnorm.rxnsat rxmap
-- on rxn.RXCUI = rxmap.RXCUI and
--    rxmap.ATN = 'NDC'and rxmap.SAB = 'RXNORM' -- normalized 11-digit NDC codes
-- ;

-- create or replace table NDC_TO_GN as
-- select distinct
--        sat.ATV as NDC
--       ,conso.RXCUI 
--       ,conso.STR
--       ,conso.TTY
-- from  ontology.rxnorm.rxnsat sat 
-- join ontology.rxnorm.rxnconso conso
-- on sat.RXCUI = conso.RXCUI
-- where sat.ATN = 'NDC'and sat.SAB = 'RXNORM'
--       and (
--        conso.TTY like 'SCD%' OR
--        conso.TTY like 'SBD%' OR
--        conso.TTY like '%IN'
--       )
-- order by sat.ATV
-- ;

-- create or replace table AOT_MEDS_NDC as 
-- select NDC,
--        RXCUI,
--        STR,
--        'nuedexta' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%nuedexta%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'glutathione' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%glutathione%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'vitaminE' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%vitamin e%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'vitaminC' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%vitamin c%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'azathioprine' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%azathioprine%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'rifampin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%rifampin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'isoniazid' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%isoniazid%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'ethambutol' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%ethambutol%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'streptomycin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%streptomycin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'kanamycin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%kanamycin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'curcumin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%curcumin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'prednisone' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%prednisone%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'methylprednisolone' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%methylprednisolone%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'budesonide' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%budesonide%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'prednisolone' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%prednisolone%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'triamcinolone' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%triamcinolone%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'dexamethasone' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%dexamethasone%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'melatonin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%melatonin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'vitaminK' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%vitamin k%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'magnesium citrate' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%magnesium%citrate%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'medium-chain triglycerides' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%medium%chain%triglycerides%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'l-carnitine' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%l%carnitine%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'turmacin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%turmacin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'tamoxifen' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%tamoxifen%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'acetyl-l-carnitine' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%acetyl-l-carnitine%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'lunasin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%lunasin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'butyrates' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%butyrates%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'resveratrol' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%resveratrol%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'accilion' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%accilion%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'astaxanthin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%astaxanthin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'protandim' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%protandim%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'vitaminB12' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%vitamin b12%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'perampanel' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%perampanel%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'hydrocortisone' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%hydrocortisone%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'copper' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%copper%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'apoaequorin' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%apoaequorin%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'ayahuasca' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%ayahuasca%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'naltrexone' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%naltrexone%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'propofol' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%propofol%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'vinpocetine' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%vinpocetine%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'vitaminD' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%vitamin d%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'rituximab' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%rituximab%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'acuscope' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%acuscope%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'spirulina' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%spirulina%'
-- union
-- select NDC,
--        RXCUI,
--        STR,
--        'mototab' as GN
-- from NDC_TO_GN
-- where lower(STR) like '%mototab%'

-- -- anti-retrovirals
-- -- anti-fungals? 
-- -- insulin? 
-- ;


select * from FDA_MEDS_RXCUI limit 5;
select * from FDA_MEDS_NDC limit 5;
select * from AOT_MEDS_RXCUI limit 5;
select * from AOT_MEDS_NDC limit 5;
select * from RXCUI_REF limit 5;
select * from NDC_REF limit 5; 
create or replace procedure get_rx_long(
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
       var raw_rx_name = (site === 'CMS') ? 'COALESCE(d.RAW_RX_MED_NAME,g.STR)' : 'g.STR';

       // collect all dispensing records
       sqlstmt1 = `
              INSERT INTO ALS_ALL_DRX
              select  distinct
                      a.PATID
                     ,d.dispense_date::date
                     ,datediff(day,a.index_date,d.dispense_date::date) as DAYS_SINCE_INDEX 
                     ,d.NDC
                     ,d.DISPENSE_SUP
                     ,d.DISPENSE_AMT
                     ,d.DISPENSE_ROUTE
                     ,d.DISPENSE_DOSE_DISP
                     ,d.DISPENSE_DOSE_DISP_unit
                     ,g.RXCUI as RXNORM_CUI
                     ,`+ raw_rx_name +` as RAW_RX_MED_NAME
                     ,ing.INGREDIENT
                     ,f.GN as FDA_APPROVED
                     ,aot.GN as AOT_POSSIBLE
                     ,'`+ site +`' as PX_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_DISPENSING d
                     on a.patid = d.patid
              left join NDC_REF ing
                     on d.ndc = ing.ndc
              left join FDA_MEDS_NDC f 
                     on d.ndc = f.ndc
              left join AOT_MEDS_NDC aot
                     on d.ndc = aot.ndc
              left join NDC_TO_GN g 
                     on d.ndc = g.ndc
              ;
       `;
 
       // collect all prescribing records
       sqlstmt2 = `
              INSERT INTO ALS_ALL_PRX
              select  distinct
                      a.PATID
                     ,p.rx_start_date
                     ,p.rx_order_date
                     ,p.rx_end_date
                     ,datediff(day,a.index_date,COALESCE(p.rx_start_date, p.rx_order_date)) as DAYS_SINCE_INDEX 
                     ,p.rxnorm_cui
                     ,p.rx_basis
                     ,p.rx_quantity
                     ,p.rx_frequency
                     ,p.rx_refills
                     ,p.raw_rx_med_name
                     ,ing.INGREDIENT
                     ,f.GN as FDA_APPROVED
                     ,aot.GN as AOT_POSSIBLE
                     ,'`+ site +`' as RX_SRC
              from `+ REF_COHORT +` a
              join GROUSE_DB.`+ site_cdm +`.LDS_PRESCRIBING p
                     on a.patid = p.patid
              left join RXCUI_REF ing
                     on p.rxnorm_cui = ing.rxcui
              left join FDA_MEDS_RXCUI f 
                     on p.rxnorm_cui = f.rxcui
              left join AOT_MEDS_NDC aot
                     on p.rxnorm_cui = aot.rxcui
              ;
       `;

       if (DRY_RUN) {
              // preview of the generated dynamic SQL scripts - comment it out when perform actual execution
              var log_stmt = snowflake.createStatement({
                            sqlText: `INSERT INTO `+ DRY_RUN_AT +` (qry) values (:1),(:2);`,
                            binds: [sqlstmt1, sqlstmt2]});
        log_stmt.execute(); 
       } else {
              // run dynamic dml query
              var run_sqlstmt1 = snowflake.createStatement({sqlText: sqlstmt1}); run_sqlstmt1.execute();
              if(site !== 'CMS'){
                     var run_sqlstmt2 = snowflake.createStatement({sqlText: sqlstmt2}); run_sqlstmt2.execute();
              }else{
                     //skip prescription table
              }
              var commit_txn = snowflake.createStatement({sqlText: `commit;`}); commit_txn.execute();
       }
}
$$
;

/* test */
-- call get_rx_long(
--        'ALS_TABLE1',
--        array_construct(
--               'CMS'
--              ,'MU'
--        ),
--        True, 'TMP_SP_OUTPUT'
-- );
-- select * from TMP_SP_OUTPUT;

/* main */
create or replace table ALS_ALL_DRX (
       PATID varchar(50) NOT NULL
      ,DISPENSE_DATE date
      ,DAYS_SINCE_INDEX number
      ,NDC varchar(12)
      ,DISPENSE_SUP number
      ,DISPENSE_AMT number
      ,DISPENSE_ROUTE varchar(50)
      ,DISPENSE_DOSE_DISP varchar(500)
      ,DISPENSE_DOSE_DISP_unit varchar(500)
      ,RXNORM_CUI varchar(10)
      ,RAW_RX_MED_NAME varchar
      ,INGREDIENT varchar
      ,FDA_APPROVED varchar
      ,AOT_POSSIBLE varchar
      ,RX_SRC varchar(10)
);

create or replace table ALS_ALL_PRX(
       PATID varchar(50) NOT NULL
      ,RX_START_DATE date
      ,RX_ORDER_DATE date
      ,RX_END_DATE date
      ,DAYS_SINCE_INDEX number
      ,RXNORM_CUI varchar(10)
      ,RX_BASIS varchar(2)
      ,RX_QUANTITY number
      ,RX_FREQUENCY varchar(5)
      ,RX_REFILLS number 
      ,RAW_RX_MED_NAME varchar
      ,INGREDIENT varchar
      ,FDA_APPROVED varchar
      ,AOT_POSSIBLE varchar
      ,RX_SRC varchar(10)
);

call get_rx_long(
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

select * from ALS_ALL_DRX limit 5; 
select * from ALS_ALL_PRX limit 5;

select FDA_APPROVED, count(distinct patid) from ALS_ALL_DRX
group by FDA_APPROVED;

select AOT_POSSIBLE, count(distinct patid) from ALS_ALL_PRX
group by AOT_POSSIBLE;

create or replace table ALS_ALL_RX_IN as
with cte_stk as (
       select PATID,
              DISPENSE_DATE as RX_DATE,
              DAYS_SINCE_INDEX,
              INGREDIENT,
              FDA_APPROVED,
              AOT_POSSIBLE
       from ALS_ALL_DRX
       union
       select PATID,
              coalesce(RX_START_DATE, RX_ORDER_DATE) as RX_DATE,
              DAYS_SINCE_INDEX,
              INGREDIENT,
              FDA_APPROVED,
              AOT_POSSIBLE
       from ALS_ALL_PRX
)
select distinct
       PATID,
       RX_DATE,
       DAYS_SINCE_INDEX,
       INGREDIENT,
       case when FDA_APPROVED is not null then 'fda'
            when AOT_POSSIBLE is not null then 'aot'
            else 'ot'
       end as FDA_AOT
from cte_stk
;

select count(distinct patid), count(distinct INGREDIENT), count(*) 
from ALS_ALL_RX_IN;
-- 19958	1453	4634507

select FDA_AOT,count(distinct patid), count(distinct INGREDIENT), count(*) 
from ALS_ALL_RX_IN
group by FDA_AOT;


select INGREDIENT,count(distinct patid), count(distinct INGREDIENT), count(*) 
from ALS_ALL_RX_IN
where FDA_AOT = 'aot'
group by INGREDIENT;

select INGREDIENT,count(distinct patid)
from ALS_ALL_RX_IN
group by INGREDIENT
order by count(distinct patid) desc
;