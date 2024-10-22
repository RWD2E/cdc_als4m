/*
Descriptions: 
Author: 
Date: 
*/

create or replace table all_yr as
select SEQ4()+2011 as calyr
from TABLE(GENERATOR(ROWCOUNT => 10))
;

create or replace table CMS_ENR_GEO_LINE as
with calyr_cte as (
    select distinct
           patid,
           calyr,
           fips
    from 
    (
        select distinct
                a.patid, 
                year(a.address_period_start) as calyr_st,
                year(a.address_period_end) as calyr_ed,
                lpad(b.FIPS,5,'0') as fips
        from GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY a 
        join EPA_DB.METADATA.SSA_FIPS_ST_CTY_2020 b
        on a.address_state || a.address_county = lpad(SSA,5,'0')
    )
    unpivot 
    (
        calyr for calyr_type in (calyr_st, calyr_ed)
    )
), censor_yr as (
    select patid, 
           max(calyr) as max_calyr
    from calyr_cte
    group by patid
), all_pt_yr as (
    select a.patid, 
           all_yr.calyr
    from (
        select distinct patid 
        from GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY
    ) a 
    cross join all_yr 
    inner join censor_yr
    on a.patid = censor_yr.patid and 
       all_yr.calyr <= censor_yr.max_calyr
), ffill_yr as (
    select a.patid,
           a.calyr,
           b.fips,
           LAST_VALUE(b.fips IGNORE NULLS) OVER (partition by a.patid ORDER BY a.calyr ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS fips_fill
from all_pt_yr a
left join calyr_cte b
on a.patid = b.patid and a.calyr = b.calyr 
)
select distinct
       patid, 
       calyr, 
       coalesce(fips, fips_fill) as fips
from ffill_yr
where fips_fill is not null
;

create or replace table CMS_ENR_GEO_LINE as
with calyr_cte as (
    select distinct
           patid,
           calyr,
           fips
    from 
    (
        select distinct
                a.patid, 
                year(a.address_period_start) as calyr_st,
                year(a.address_period_end) as calyr_ed,
                lpad(b.FIPS,5,'0') as fips
        from GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY a 
        join EPA_DB.METADATA.SSA_FIPS_ST_CTY_2020 b
        on a.address_state || a.address_county = lpad(SSA,5,'0')
    )
    unpivot 
    (
        calyr for calyr_type in (calyr_st, calyr_ed)
    )
), censor_yr as (
    select patid, 
           max(calyr) as max_calyr
    from calyr_cte
    group by patid
), all_pt_yr as (
    select a.patid, 
           all_yr.calyr
    from (
        select distinct patid 
        from GROUSE_DB.CMS_PCORNET_CDM.LDS_ADDRESS_HISTORY
    ) a 
    cross join all_yr 
    inner join censor_yr
    on a.patid = censor_yr.patid and 
       all_yr.calyr <= censor_yr.max_calyr
), ffill_yr as (
    select a.patid,
           a.calyr,
           b.fips,
           LAST_VALUE(b.fips IGNORE NULLS) OVER (partition by a.patid ORDER BY a.calyr ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS fips_fill
from all_pt_yr a
left join calyr_cte b
on a.patid = b.patid and a.calyr = b.calyr 
)
select distinct
       patid, 
       calyr, 
       coalesce(fips, fips_fill) as fips
from ffill_yr
where fips_fill is not null
;

select calyr, count(distinct patid) as pat_cnt 
from 
(
    select patid, max(calyr) as calyr
    from CMS_ENR_GEO_LINE
    group by patid
)
group by calyr
;

create or replace table CMS_ENR_GEO as 
select calyr, fips, count(distinct patid) as CMS_TOTAL
from CMS_ENR_GEO_LINE
group by calyr, fips
;


create or replace table ALS_TABLE1_CMS_GEO_LINE as 
with als_cms as (
    select * from ALS_TABLE1
    where complt_flag in ('complete','cms_only') and 
          case_assert = 'confirmed'
), dx_yearly as (
    select distinct 
           a.patid, 
           year(dx.dx_date) as calyr
    from als_cms a
    join ALS_ALL_DX dx
    on a.patid = dx.patid
    where dx.dx in ('335.20','G12.21')
), end_year as (
    select distinct
           a.patid, 
           year(dth.stage_date) as calyr, 
           row_number() over (partition by a.patid order by dth.stage_date) as rn
    from als_cms a 
    join ALS_ENDPTS dth
    on a.patid = dth.patid
    where dth.stage in ('censor','death')
), calyr_stk as (
    select PATID, 
           year(als1dx_date) as calyr 
    from als_cms
    union
    select patid, calyr 
    from dx_yearly
    union
    select patid, calyr 
    from end_year
    where rn = 1
), add_geo as (
    select distinct
            a.patid, 
            a.calyr,
            b.fips
    from calyr_stk a 
    join CMS_ENR_GEO_LINE b 
    on a.patid = b.patid and a.calyr = b.calyr
), censor_yr as (
    select patid, 
           max(calyr) as max_calyr
    from calyr_stk
    group by patid
), all_pt_yr as (
    select a.patid, 
           all_yr.calyr
    from (
        select distinct patid 
        from calyr_stk
    ) a 
    cross join all_yr
    inner join censor_yr
    on a.patid = censor_yr.patid and 
       all_yr.calyr <= censor_yr.max_calyr
), ffill_yr as (
    select a.patid,
           a.calyr,
           b.fips,
           LAST_VALUE(b.fips IGNORE NULLS) OVER (partition by a.patid ORDER BY a.calyr ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS fips_fill
from all_pt_yr a
left join add_geo b
on a.patid = b.patid and a.calyr = b.calyr 
)
select distinct
       patid, 
       calyr, 
       coalesce(fips, fips_fill) as fips
from ffill_yr
where fips_fill is not null
;

create or replace table ALS_TABLE1_CMS_GEO as 
select calyr
      ,fips
      ,count(distinct patid) as ALS_TOTAL
from ALS_TABLE1_CMS_GEO_LINE
group by calyr, fips
;

select distinct chemical from TRI_TOXICS_TRACKER;

-- select release type and pivot TRI table
create or replace table TRI_ENCODER as 
with dedup_cte as (
select distinct
       CHEMICAL,
       "CAS#" as CAS_ID,
       "SRS ID" as SRS_ID,
       METAL as METAL_IND,
       CARCINOGEN as CARCINOGEN_IND,
       PBT as PBT_IND,
       PFAS as PFAS_IND,
       "UNIT OF MEASURE" as UNIT, 
       row_number() over (partition by "SRS ID" order by len(CAS_ID) desc, len(CHEMICAL) desc) as rn
from EPA_DB.STAGING_EPA.TRI_TOXICS_DATA
where SRS_ID is not null
), alias_cte as (
    select CHEMICAL, 
           substr(CHEMICAL,greatest(p1,p2)+1,p3-greatest(p1,p2)) as CAS_ID
    from (
        select distinct 
            CHEMICAL,
            regexp_instr(CHEMICAL,'\\(N') as p1,
            regexp_instr(CHEMICAL,'\\(\\d') as p2,
            regexp_instr(CHEMICAL,'\\d\\)') as p3
        from EPA_DB.STAGING_EPA.TRI_TOXICS_TRACKER
    )
)
select a.chemical,
       b.CHEMICAL as chemical_alias,
       a.cas_id,
       a.srs_id,
       a.metal_ind,
       a.carcinogen_ind,
       a.pbt_ind,
       a.pfas_ind,
       unit,
       'T' || row_number() over (order by a.chemical) as encoded
from dedup_cte a
left join alias_cte b 
on a.CAS_ID = b.CAS_ID
where a.rn = 1
;

select count(distinct chemical), count(distinct cas_id), count(*) 
from TRI_ENCODER;

select * from TRI_ENCODER limit 5;

create or replace table TRI_TRACKER_CBG as 
select a.YEAR,
       a.CENSUS_TRACT,
       a.CENSUS_BLOCK_GROUP,
       substr(a.CENSUS_TRACT, 1,5) as FIPS,
       e.ENCODED as CHEM_CD,
       a.TRI_FACILITY_ID as TRIFD,
       a.DEMOGRAPHIC_INDEX_PERCENTILE as DEMO_PT,
       a.SUPPLEMENTAL_DEMOGRAPHIC_INDEX_PERCENTILE as DEMOSUPP_PT,
       a.PEOPLE_OF_COLOR_PERCENTILE,
       a.LOW_INCOME_PERCENTILE,
       a.UNEMPLOYMENT_RATE_PERCENTILE,
       a.LIMITED_ENGLISH_SPEAKING_PERCENTILE,
       a.LESS_THAN_HIGH_SCHOOL_EDUCATION_PERCENTILE,
       a.UNDER_AGE_5_PERCENTILE,
       a.OVER_AGE_64_PERCENTILE,
       to_decimal(replace(a."Releases_(lb)",',','')) as RLTOT_LB,
       to_decimal(replace(a."Air_Releases_(lb)",',','')) as RLAIR_LB,
       to_decimal(replace(a."Water_Releases_(lb)",',','')) as RLWTR_LB,
       to_decimal(replace(a."Land_Releases_(lb)",',','')) as RLLND_LB,
       to_decimal(replace(a."Off-Site_Releases_(lb)",',','')) as RLOFF_LB,
       to_decimal(replace(a."Waste_Managed_(lb)",',','')) as WSTMNG_LB,
       case when a.RSEI_SCORE in ('No RSEI Score') then 0
            else to_decimal(replace(a.RSEI_SCORE,',','')) 
       end as RSEI_SCORE
from EPA_DB.STAGING_EPA.TRI_TOXICS_TRACKER a
join TRI_ENCODER e 
on a.CHEMICAL = e.chemical_alias
;

select * from TRI_TRACKER_CBG limit 5;

create or replace table TRI_DATA_CBG as 
with geo_map as (
    select distinct 
           TRI_FACILITY_ID as TRIFD,
           CENSUS_TRACT, 
           CENSUS_BLOCK_GROUP,
           lpad(ZIP_CODE,5,'0') as ZIP_CODE
    from EPA_DB.STAGING_EPA.TRI_TOXICS_TRACKER  
    where CENSUS_TRACT <> '-'  
)
select distinct 
        a.year, 
        a.trifd,
        b.CENSUS_TRACT,
        b.CENSUS_BLOCK_GROUP,
        substr(b.CENSUS_TRACT, 1,5) as FIPS,
        e.ENCODED as CHEM_CD,
        e.UNIT,
        to_decimal(replace(a."PRODUCTION WSTE (8.1-8.7)",',','')) as PRDWST,
        to_decimal(replace(a."TOTAL RELEASES",',','')) as RLTOT,
        to_decimal(replace(a."OFF-SITE RELEASE TOTAL",',','')) as RLOFF,
        to_decimal(replace(a."ON-SITE RELEASE TOTAL",',','')) as RLON
from EPA_DB.STAGING_EPA.TRI_TOXICS_DATA a 
join geo_map b on a.TRIFD = b.TRIFD
join TRI_ENCODER e on a."CAS#" = e.CAS_ID 
;

select * from TRI_DATA_CBG limit 5;

create or replace table TRI_CTY_LONG as 
with data_pvt as (
    select distinct 
           year, fips, CHEM_CD, VAR, val,
           CHEM_CD || '_' || VAR  as var2 
    from (
        select year, fips, CHEM_CD, 
            sum(PRDWST) as PRDWST, 
            sum(RLTOT) as RLTOT,
            sum(RLOFF) as RLOFF,
            sum(RLON) as RLON
        from TRI_DATA_CBG
        group by year, fips, CHEM_CD
    )
    unpivot
    (
        val for var in (PRDWST,RLTOT,RLOFF,RLON)
    )
), tracker_pvt as(
    select distinct 
           year, fips, CHEM_CD, VAR, val,
           CHEM_CD || '_' || VAR  as var2
    from (
        select year, fips, CHEM_CD, 
            sum(RLTOT_LB) as RLTOT_LB, 
            sum(RLAIR_LB) as RLAIR_LB,
            sum(RLWTR_LB) as RLWTR_LB,
            sum(RLLND_LB) as RLLND_LB,
            sum(RLOFF_LB) as RLOFF_LB,
            sum(RSEI_SCORE) as RSEI_SCORE
        from TRI_TRACKER_CBG
        where CENSUS_TRACT <> '-'
        group by year, fips, CHEM_CD
    )
    unpivot
    (
        val for var in (RLTOT_LB,RLAIR_LB,RLWTR_LB,RLLND_LB,RLOFF_LB,RSEI_SCORE)
    )
), stk_cte as (
    select distinct year, fips, var2, val from data_pvt
    union 
    select distinct year, fips, var2, val from tracker_pvt
), sp_cte as (
    select stk_cte.*, 
       count(stk_cte.val) over (partition by stk_cte.var2) as avail_cnt,
       n.N
    from stk_cte
    cross join (
        select count(distinct (year || fips)) as N 
        from stk_cte
    ) n
)
select year,
       fips, 
       var2, 
       val, 
       sum(val) over (partition by fips,var2 order by year asc rows between 3 preceding and 1 preceding) as val_cum3,
       sum(val) over (partition by fips,var2 order by year asc rows between 5 preceding and 1 preceding) as val_cum5,
       sum(val) over (partition by fips,var2 order by year asc rows between 10 preceding and 1 preceding) as val_cum10,
       avail_cnt, 
       N, 
       avail_cnt/N as avail_p
from sp_cte
;

select * from TRI_CTY_LONG limit 5;

select * from TRI_CTY_LONG
order by var2, fips, year
;

create or replace table TRI_CTY_WIDE as 
with cte as (
    select * 
    from (
        select year, fips, var2, val
        from TRI_CTY_LONG
        where avail_p >= 0.05
    )
    pivot(max(val) for var2 in (ANY ORDER BY var2))
), cte3 as (
    select * 
    from (
        select year, fips, var2 || '_CUM3' as var2, val_cum3
        from TRI_CTY_LONG
        where avail_p >= 0.05
    )
    pivot(max(val_cum3) for var2 in (ANY ORDER BY var2))
), cte5 as (
    select * 
    from (
        select year, fips, var2 || '_CUM5' as var2, val_cum5
        from TRI_CTY_LONG
        where avail_p >= 0.05
    )
    pivot(max(val_cum5) for var2 in (ANY ORDER BY var2))
), cte10 as (
    select * 
    from (
        select year, fips, var2 || '_CUM10' as var2, val_cum10
        from TRI_CTY_LONG
        where avail_p >= 0.05
    )
    pivot(max(val_cum10) for var2 in (ANY ORDER BY var2))
)
select cte.*, 
    --    cte3.* exclude (year,fips),
       cte5.* exclude (year,fips)
    --    cte10.* exclude (year,fips)
from cte 
-- join cte3 on cte.fips = cte3.fips and cte.year = cte3.year
join cte5 on cte.fips = cte5.fips and cte.year = cte5.year
-- join cte10 on cte.fips = cte10.fips and cte.year = cte10.year
;

select * from TRI_CTY_WIDE limit 5;


create or replace table PC_ENCODER as 
select compound, 
       'P' || row_number() over (order by compound) as CMP_CD
from 
(
    select distinct compound
    from EPA_DB.STAGING_USGS.PESTICIDE_USE_METADATA
)
;


create or replace table PC_CTY_WIDE as 
with cte as (
    select a.year, 
           a.STATE_FIPS_CODE || a.COUNTY_FIPS_CODE as fips, 
           b.CMP_CD,
           to_decimal(a.EPEST_HIGH_KG) as val
    from EPA_DB.STAGING_USGS.PESTICIDE_USE a 
    join PC_ENCODER b 
    on a.compound = b.compound
)
, cte1 as (
    select * from cte
    pivot 
    (
        max(val) for CMP_CD in (ANY ORDER BY CMP_CD)
    )
)
, cte3 as (
    select * from (
        select year,fips,
                CMP_CD || '_CUM3' as CMP_CD,
                sum(val) over (partition by fips,CMP_CD order by year asc rows between 3 preceding and 1 preceding) as val_cum
        from cte
    )
    pivot 
    (
        max(val_cum) for CMP_CD in (ANY ORDER BY CMP_CD)
    )
)
, cte5 as (
    select * from (
        select year,fips,
                CMP_CD || '_CUM5' as CMP_CD,
                sum(val) over (partition by fips,CMP_CD order by year asc rows between 5 preceding and 1 preceding) as val_cum
        from cte
    )
    pivot 
    (
        max(val_cum) for CMP_CD in (ANY ORDER BY CMP_CD)
    )
)
, cte10 as (
    select * from (
        select year,fips,
               CMP_CD || '_CUM10' as CMP_CD,
               sum(val) over (partition by fips,CMP_CD order by year asc rows between 10 preceding and 1 preceding) as val_cum
        from cte
    )
    pivot 
    (
        max(val_cum) for CMP_CD in (ANY ORDER BY CMP_CD)
    )
)
select distinct 
       cte1.*,
    --    cte3.* exclude (year,fips),
       cte5.* exclude (year,fips)
    --    cte10.* exclude (year,fips)
from cte1
-- join cte3 on cte1.year = cte3.year and cte1.fips = cte3.fips
join cte5 on cte1.year = cte5.year and cte1.fips = cte5.fips
-- join cte10 on cte1.year = cte10.year and cte1.fips = cte10.fips
;

select * from PC_CTY_WIDE limit 5;


-- put everything together
create or replace table ALS_GEO_FINAL as 
select a.calyr,
       a.fips,
       a.CMS_TOTAL,
       coalesce(b.ALS_TOTAL,0) as ALS_TOTAL,
       coalesce(b.ALS_TOTAL,0)/a.CMS_TOTAL as ALS_PROP,
       tri.* exclude (year,fips),
       pc.* exclude (year,fips)
from CMS_ENR_GEO a 
left join ALS_TABLE1_CMS_GEO b 
on a.calyr = b.calyr and a.fips = b.fips
left join TRI_CTY_WIDE tri 
on a.calyr = tri.year and a.fips = tri.fips
left join PC_CTY_WIDE pc
on a.calyr = pc.year and a.fips = pc.fips
where substr(a.fips, 1, 2) in (
    '19', --IA
    '20', --KS
    '27', --MN
    '29', --MO
    '31', --NE
    '48', --TX
    '49', --UT
    '55'  --WI
)
;

select * from ALS_GEO_FINAL limit 5;

select count(distinct calyr || fips), count(*) from ALS_GEO_FINAL;
-- 32310 -> 8620

create or replace table ALS_GEO_ST as 
select substr(fips,1,2) as fips_st, 
        calyr, 
        sum(CMS_TOTAL) as CMS_TOTAL, 
        sum(ALS_TOTAL) as ALS_TOTAL,
        count(distinct fips) as CNTY_CNT
from ALS_GEO_FINAL
group by calyr,substr(fips,1,2)
;

select * from ALS_GEO_ST order by fips_st,calyr;

select fips_st,CNTY_CNT,
       sum(CMS_TOTAL) as den, 
       sum(ALS_TOTAL) as num, 
       round(sum(ALS_TOTAL)/sum(CMS_TOTAL)*100000,1) as prev_rt
from ALS_GEO_ST
-- where calyr >= 2014
group by fips_st,CNTY_CNT
order by fips_st
;