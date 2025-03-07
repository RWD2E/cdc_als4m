select * from PAT_TABLE1;
select * from ALS_TABLE1;
select * from T2DM_TABLE1;
select * from GLP1_EVENT_LONG;


CREATE OR REPLACE TABLE PAT_YEAR_ALL AS
    WITH RECURSIVE pat_year_expansion AS 
    (
        SELECT patid, 
               index_year AS year, 
               censor_year
        FROM PAT_TABLE1
        where partd_ind = 1 or ehr_ind = 1

        UNION ALL

        SELECT patid,
               year + 1,
               censor_year
        FROM pat_year_expansion
        WHERE year < censor_year
    )
    select patid, year 
    from pat_year_expansion
    where year between 2011 and 2020
    order by patid, year
;

CREATE OR REPLACE TABLE EHR_YEAR_ALL AS
    WITH RECURSIVE pat_year_expansion AS 
    (
        SELECT patid, 
               index_year AS year, 
               censor_year
        FROM PAT_TABLE1
        where ehr_ind = 1

        UNION ALL

        SELECT patid,
               year + 1,
               censor_year
        FROM pat_year_expansion
        WHERE year < censor_year
    )
    select patid, year 
    from pat_year_expansion
    where year between 2011 and 2020
    order by patid, year
;

CREATE OR REPLACE TABLE PARTD_YEAR_ALL AS
    WITH RECURSIVE pat_year_expansion AS 
    (
        SELECT patid, 
               index_year AS year, 
               censor_year
        FROM PAT_TABLE1
        where partd_ind = 1

        UNION ALL

        SELECT patid,
               year + 1,
               censor_year
        FROM pat_year_expansion
        WHERE year < censor_year
    )
    select patid, year 
    from pat_year_expansion
    where year between 2011 and 2020
    order by patid, year
;

CREATE OR REPLACE TABLE XWALK_YEAR_ALL AS
    WITH RECURSIVE pat_year_expansion AS 
    (
        SELECT patid, 
               index_year AS year, 
               censor_year
        FROM PAT_TABLE1
        WHERE xwalk_ind = 1 and partd_ind = 1

        UNION ALL

        SELECT patid,
               year + 1,
               censor_year
        FROM pat_year_expansion
        WHERE year < censor_year
    )
    select patid, year 
    from pat_year_expansion
    where year between 2011 and 2020
    order by patid, year
;

/* table 1 - overall */

select year, count(distinct patid) as pat_cnt 
from PAT_YEAR_ALL
group by year
union 
select 9999, count(distinct patid) as pat_cnt 
from PAT_YEAR_ALL
order by year
;


select year, count(distinct patid) as pat_cnt 
from EHR_YEAR_ALL
group by year
union 
select 9999, count(distinct patid) as pat_cnt 
from EHR_YEAR_ALL
order by year
;


select year, count(distinct patid) as pat_cnt 
from PARTD_YEAR_ALL
group by year
union 
select 9999, count(distinct patid) as pat_cnt 
from PARTD_YEAR_ALL
order by year
;


select year, count(distinct patid) as pat_cnt 
from XWALK_YEAR_ALL
group by year
union 
select 9999, count(distinct patid) as pat_cnt 
from XWALK_YEAR_ALL
order by year
;


/* table 1 - T2DM  */

select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from XWALK_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from XWALK_YEAR_ALL a 
where exists (select 1 from T2DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;


/* table 1 - ALS */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from XWALK_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from XWALK_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
order by year
;


/* table 1 - ALS + T2DM*/
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

select a.year, count(distinct a.patid) as pat_cnt 
from XWALK_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from XWALK_YEAR_ALL a 
where exists (select 1 from ALS_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;



/* table 2 - GLP1  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
order by year
;

/* table 2 - DPP4  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
order by year
;

/* table 2 - either  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
order by year
;


/* table 2 - T2DM+GLP1  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

/* table 2 - T2DM+DPP4  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

/* table 2 - T2DM+either  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

/* table 2 - ALS+GLP1  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

/* table 2 - ALS+DPP4  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

/* table 2 - ALS+either  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;


/* table 2 - T2DM+ALS+GLP1  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from T2DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

/* table 2 - T2DM+ALS+DPP4  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;

/* table 2 - T2DM+ALS+either  */
select a.year, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from ALS_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
order by year
;