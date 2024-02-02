/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-case_cdc-sialorrhea.sql
# Description: sialorrhea medication use among ALS patients
*/

use role GROUSE_ROLE_C_ANALYTICS;
use warehouse GROUSE_WH;
use database GROUSE_DEID_ANALYTICS_DB; -- write-premitted database
use schema SX_ALS_GPC;

select * from als_table1 limit 5;
select * from als_all_prx limit 5;
select * from als_all_drx limit 5;
select * from fda_meds_hs_rxcui limit 5;
select * from als_all_rx_in limit 5;


create or replace table ALS_SEL_PRX_HS as 
with cte_selrx as (
       select distinct a.patid, a.rx_order_date as rx_date, a.days_since_index, b.ing
       from als_all_prx a
       join fda_meds_hs_rxcui b on a.rxnorm_cui = to_char(b.rxcui)
       union
       select distinct a.patid, a.dispense_date as rx_date, a.days_since_index, b.ing
       from als_all_drx a
       join fda_meds_hs_rxcui b on a.rxnorm_cui = to_char(b.rxcui)
)
select distinct
       a.patid, 
       a.birth_date,
       a.age_at_index,
       a.sex,
       a.race,
       a.hispanic,
       a.case_assert,
       a.index_src,
       a.complt_flag,
       a.index_date,
       b.rx_date,
       extract('year',b.rx_date) as rx_year,
       b.days_since_index,
       b.ing, 
       row_number() over (partition by a.patid order by b.days_since_index) as rn,
       row_number() over (partition by a.patid, b.ing order by b.days_since_index) as rn_ing
from als_table1 a 
join cte_selrx b 
on a.patid = b.patid 
;

select round(avg(pat_n)), median(pat_n), round(stddev(pat_n))
from (
       select rx_year, count(distinct patid) as pat_n
       from ALS_SEL_PRX_HS
       group by rx_year
       order by rx_year
)
where rx_year between 2011 and 2019
;

