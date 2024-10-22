/*
Description: CDC Algorithm for identifing Definite and Probable ALS 
             based on administrative database
Written by: Hamza Turabieh
Date: 01-15-2023
*/

use role GROUSE_ROLE_C_ANALYTICS;
use warehouse GROUSE_WH;
use database GROUSE_DEID_ANALYTICS_DB; -- write-premitted database
use schema ALS;


set cms_cdm_schema = 'CMS_PCORNET_CDM';
set diagnosis = $cms_cdm_schema || '.V_DEID_DIAGNOSIS';
set demographic = $cms_cdm_schema || '.V_DEID_DEMOGRAPHIC';
set death = $cms_cdm_schema || '.V_DEID_DEATH';
set procedures = $cms_cdm_schema || '.V_DEID_PROCEDURES';
set dispensing = $cms_cdm_schema || '.V_DEID_DISPENSING';
set encounter = $cms_cdm_schema || '.V_DEID_ENCOUNTER';
set enrollment = $cms_cdm_schema || '.V_DEID_ENROLLMENT';

/*=====================================================================================*/
/* Note:
if Table name has X, that means  DX is either G12.2X or 355.2X. For example:  ALS_X_INIT
if Table name without X, that means DX is either 'G12.21' or dx ='355.20'. For example:  ALS_INIT
=====================================================================================*/


/*=====================================================================================*/
/*ALS_X_INIT: first and last ALS diagnosis*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_INIT as
select patid,
       min(nvl(admit_date, dx_date)) as first_als_date, max(nvl(admit_date, dx_date)) as last_als_date
from identifier($diagnosis)
where dx like 'G12.2%' or dx like '335.2%'
group by patid;

/*=====================================================================================*/
/*ALS_X_DIAGNOSIS:  ALS diagnosis*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS as
select PATID, DX from "GROUSE_DEID_DB"."CMS_PCORNET_CDM"."V_DEID_DIAGNOSIS"
where DX like '335.2%' or DX like 'G12.2%';



/*=====================================================================================*/
/*ALS_X_DEMOGRAPHIC */
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DEMOGRAPHIC as
select PATID, SEX, RACE,Hispanic,Birth_date from identifier($demographic) 
inner join
(   select PATID as H from identifier($diagnosis) 
    where DX like '335.2%' or DX like 'G12.2%'  group by PATID
)
where PATID = H 


select   b.PATID from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS as b
where b.DX = '335.20' or b.DX = 'G12.21'

select distinct  b.PATID from GROUSE_DEID_DB.CMS_PCORNET_CDM.DEID_DIAGNOSIS as b
where b.DX = '335.20' or b.DX = 'G12.21'
group by patid;
/*=====================================================================================*/
/*ALS_X_TWO_DIAGNOSIS in two different years -Definite ALS -CDC algorithm*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_TWO_DIAGNOSIS_DEFINITE as
select PATID, min(nvl(admit_date, dx_date)) as first_DX_date, max(nvl(admit_date, dx_date)) as last_DX_date, floor(DATEDIFF(day,First_DX_date, Last_DX_date)/365.25)>=1 as Definite_ALS 
from identifier($diagnosis)
where DX like '335.2%' or DX like 'G12.2%'  group by PATID;

/*=====================================================================================*/
/*ALS_X_VISIT_NEUROLOGIST*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST as
select  a.PATID, a.DX as Dx_ALS , a.PROVIDERID as Provider_ID, a.DX_date as DX_Date
from identifier($diagnosis) as a
inner join
(   select distinct b.NPI as TT, b.HEALTHCARE_PROVIDER_TAXONOMY_CODE_1,c.MEDICARE_PROVIDER_TYPE_DESCRIPTION from "NPPES_NPI_REGISTRY"."NPPES_FEB"."NPIDATA" as b 
    inner join "NPPES_NPI_REGISTRY"."NPPES_FEB"."NPI_TAXONOMY" as c  
    on  b.HEALTHCARE_PROVIDER_TAXONOMY_CODE_1=c.PROVIDER_TAXONOMY_CODE
    where  lower(c.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%'
)
on  try_to_number(Provider_ID) = TT  and (Dx_ALS like '335.2%' or Dx_ALS like 'G12.2%')

/*=====================================================================================*/
/* NPI_NEUROLOGIST , select all NPI who are NEUROLOGIST*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.NPI_NEUROLOGIST as
select distinct a.NPI, a.HEALTHCARE_PROVIDER_TAXONOMY_CODE_1, b.MEDICARE_PROVIDER_TYPE_DESCRIPTION from "NPPES_NPI_REGISTRY"."NPPES_FEB"."NPIDATA" as a
inner join "NPPES_NPI_REGISTRY"."NPPES_FEB"."NPI_TAXONOMY" as b  on 
a.HEALTHCARE_PROVIDER_TAXONOMY_CODE_1=b.PROVIDER_TAXONOMY_CODE
where  lower(b.MEDICARE_PROVIDER_TYPE_DESCRIPTION) like '%neurology%'

/*=====================================================================================*/
/* ALS_X_ONE_DIAGNOSIS */
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_ONE_DIAGNOSIS as
select a.PATID from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS as a
group by a.PATID
having count(*) = 1

create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_ONE_DIAGNOSIS as
select a.PATID, a.DX from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS as a
inner join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_ONE_DIAGNOSIS as b
on a.PATID = b.PATID;

/*=====================================================================================*/
/* ALS_X_VISIT_NEUROLOGIST_DEFINITE , visit NEUROLOGIST at least 5 times */
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST_DEFINITE as
select PATID, count(PATID) as TOTAL_VISIT, count(PATID)>=5 as DEFINITE_ALS from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST
group by PATID;

/*=====================================================================================*/
/*ALS_ONE_DIAGNOSIS_NO_VISIT*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.Temp as
select a.PATID from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_DIAGNOSIS as a
MINUS
select b.PATID from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST as b

create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_ONE_DIAGNOSIS_NO_VISIT as
select a.PATID, c.DX from GROUSE_DEID_ANALYTICS_DB.ALS.TEMP as a
inner join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_ONE_DIAGNOSIS as c on a.PATID=c.PATID

drop table  GROUSE_DEID_ANALYTICS_DB.ALS.TEMP
/*=====================================================================================*/

/*ALS_DIAGNOSIS:  ALS diagnosis -  dx ='G12.20' or dx ='355.21'*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_DIAGNOSIS as
select * from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS
where dx ='G12.21' or dx ='335.20'

/*=====================================================================================*/
/*ALS_DEMOGRAPHIC */
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_DEMOGRAPHIC as
select PATID, SEX, RACE,Hispanic,Birth_date from identifier($demographic) 
inner join
(select PATID as H from identifier($diagnosis) 
where DX = '335.20' or DX = 'G12.21'  group by PATID)
where PATID = H 

/*=====================================================================================*/
/*ALS_INIT: first and last ALS diagnosis*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_INIT as
select patid,
       min(nvl(admit_date, dx_date)) as first_als_date, max(nvl(admit_date, dx_date)) as last_als_date
from identifier($diagnosis)
where dx = 'G12.21' or dx ='335.20'
group by patid;

/*=====================================================================================*/
/*ALS_ONE_DIAGNOSIS_NO_VISIT*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_ONE_DIAGNOSIS_NO_VISIT as
select * from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_ONE_DIAGNOSIS_NO_VISIT
where dx = 'G12.21' or dx ='335.20'

/*=====================================================================================*/
/*ALS_VISIT_NEUROLOGIST*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_VISIT_NEUROLOGIST  as
select * from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST 
where DX_ALS = 'G12.21' or DX_ALS ='335.20'



/*=====================================================================================*/
/* ALS_VISIT_NEUROLOGIST_DEFINITE , visit NEUROLOGIST at least 5 times */
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_VISIT_NEUROLOGIST_DEFINITE as
select PATID, count(PATID) as TOTAL_VISIT, count(PATID)>=5 as DEFINITE_ALS from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_VISIT_NEUROLOGIST
group by PATID;


/*=====================================================================================*/
/* ALS_ONE_DIAGNOSIS*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_ONE_DIAGNOSIS  as
select * from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_ONE_DIAGNOSIS
where dx = 'G12.21' or dx ='335.20'

/*=====================================================================================*/
/* ALS_TWO_DIAGNOSIS_DEFINITE */
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_TWO_DIAGNOSIS_DEFINITE as
select PATID, min(nvl(admit_date, dx_date)) as first_DX_date, max(nvl(admit_date, dx_date)) as last_DX_date, floor(DATEDIFF(day,First_DX_date, Last_DX_date)/365.25)>=1 as Definite_ALS 
from identifier($diagnosis)
where DX = '335.20' or DX = 'G12.21'  group by PATID ;


/*=====================================================================================*/
-- look up Rxnorm by generic name partial string matching
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.SEL_MED_RXCUI as
select distinct
       STR
      ,RXCUI
      ,RXAUI
      ,SAB
      ,CODE
      ,TTY
      ,SUPPRESS
from ontology.rxnorm.rxnconso 
where 
   (
    -- semantic name
      lower(STR) like '%riluzol%' OR
      lower(STR) like '%endaravon%'  OR
    -- brand name
      lower(STR) like '%rilutek%' OR
      lower(STR) like '%radicav%'
   )
   and
   (
      -- https://www.nlm.nih.gov/research/umls/rxnorm/docs/appendix5.html
      TTY like 'SCD%' OR --Semantic Clinical Drug
      TTY like 'SBD%' OR --Semantic Branded Drug
      TTY like '%IN'     --Ingreiant
   )
   and
   SAB = 'RXNORM'
;


-- look up NDC by provided RXCUI list
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.SEL_MED_NDC as
select distinct
       rxn.RXCUI
      ,rxn.STR
      ,rxmap.ATV as NDC
      ,rxmap.SUPPRESS
from GROUSE_DEID_ANALYTICS_DB.ALS.SEL_MED_RXCUI rxn
join ontology.rxnorm.rxnsat rxmap
on rxn.RXCUI = rxmap.RXCUI and
   rxmap.ATN = 'NDC'and rxmap.SAB = 'RXNORM' -- normalized 11-digit NDC codes
order by rxcui;



/*=====================================================================================*/

/*=====================================================================================*/
/* ALS_X_RILUZOLE_EDARAVONE*/
/*=====================================================================================*/
/* The table is temporary for collecting data for next step */
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.TEMP as
select AA.PATID, AA.NDC from 
(   select a.PATID, a.NDC from "GROUSE_DEID_DB"."CMS_PCORNET_CDM"."V_DEID_DISPENSING" as a
    inner join "GROUSE_DEID_ANALYTICS_DB"."ALS"."ALS_X_DIAGNOSIS" as b
    on a.PATID = b.PATID) as AA
inner join "GROUSE_DEID_ANALYTICS_DB"."ALS"."SEL_MED_NDC" as BB
on AA.NDC = BB.NDC

/* ALS_X_RILUZOLE_EDARAVONE*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_RILUZOLE_EDARAVONE as
select distinct a.PATID, a.DX, T.NDC from "GROUSE_DEID_ANALYTICS_DB"."ALS"."ALS_X_DIAGNOSIS"  as a
inner join "GROUSE_DEID_ANALYTICS_DB"."ALS"."TEMP" as T 
on  a.PATID = T.PATID
/* drop TEMP table*/
drop TABLE  GROUSE_DEID_ANALYTICS_DB.ALS.TEMP
/*=====================================================================================*/

/*=====================================================================================*/
/* ALS_RILUZOLE_EDARAVONE*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_RILUZOLE_EDARAVONE as
select * from "GROUSE_DEID_ANALYTICS_DB"."ALS"."ALS_X_RILUZOLE_EDARAVONE"
where dx = 'G12.21' or dx ='335.20'


/* rename some col. names to be standard as other tables. */
alter table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST rename column PROVIDER_ID to PROVIDERID

alter table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST rename column DX_ALS to DX
alter table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_VISIT_NEUROLOGIST rename column PROVIDER_ID to PROVIDERID
alter table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_VISIT_NEUROLOGIST rename column DX_ALS to DX

/*=====================================================================================*/
/* ALS_X_DEATH*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DEATH as
select  distinct a.PATID, DEATH_DATE from GROUSE_DEID_DB.CMS_PCORNET_CDM.V_DEID_DEATH as a
inner join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS as b
on a.patid = b.patid 

/*=====================================================================================*/
/* ALS_DEATH*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_DEATH as
select  distinct a.PATID, DEATH_DATE from  GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DEATH as a
inner join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS as b
on a.patid = b.patid and  b.dx = 'G12.21' or b.dx ='335.20'


/*=====================================================================================*/
/* ALS_X_Definte*/
/*=====================================================================================*/


create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_Case as
select a.PATID, a.Sex, a.race, a.hispanic, b.first_dx_date, b.last_dx_date as LAST_DX_DATE, a.birth_date, 
max(nvl(n.death_date,LAST_DX_DATE)) as DEATH_DATE,
g.dx, c.total_visit, case when f.ndc is not null then 1 else 0 end as RILUZOLE_EDARAVONE_FLAG, 
datediff(year,a.birth_date, Death_Date) as Age, (b.definite_als or c.definite_als or RILUZOLE_EDARAVONE_FLAG)= 1 as definite_alsa,
case when (b.first_dx_date = b.last_dx_date) AND  total_visit = 0  then true else false end as not_asl,
case when (definite_alsa = false OR definite_alsa is null) AND not_asl = false then true else false end as Possible_asl
from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DEMOGRAPHIC as a
full join  GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_TWO_DIAGNOSIS_DEFINITE as b
on  a.patid = b.patid
full join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_VISIT_NEUROLOGIST_DEFINITE as c
on  a.patid = c.patid
full join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_RILUZOLE_EDARAVONE as f
on a.patid = f.patid
full join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DIAGNOSIS as g
on  a.patid = g.patid
full join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_DEATH as n
on  a.patid = n.patid
full join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_ONE_DIAGNOSIS as DG
on  a.patid = DG.patid
full join GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_ONE_DIAGNOSIS_NO_VISIT as xa
on  a.patid = xa.patid
group by a.PATID, a.Sex, a.race, a.hispanic, b.first_dx_date, b.last_dx_date, c.total_visit, g.dx, n.death_date, a.birth_date, Age, b.definite_als,  f.ndc,  c.definite_als
order by a.PATID

/* The following Sql statements to standardize the final Table
1) if definite value is null converted to zero
2) update age based on date of death in case Age vale is null
3) update Total_VISIT of neurologist if its value equals null to zero
*/


alter table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_Case  rename column definite_alsa to definite_als;

UPDATE GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_Case 
SET definite_als=0
WHERE definite_als IS NULL;


UPDATE GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_Case  as a
SET Age= datediff(year,a.birth_date, a.Death_Date)
WHERE Age IS NULL;


UPDATE GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_Case  as a
SET a.Total_VISIT= 0
WHERE Total_VISIT IS NULL;


/*=====================================================================================*/
/* ALS_Definte*/
/*=====================================================================================*/
create or replace table GROUSE_DEID_ANALYTICS_DB.ALS.ALS_Case as
select * from GROUSE_DEID_ANALYTICS_DB.ALS.ALS_X_Case
where dx = '335.20' or dx = 'G12.21'





