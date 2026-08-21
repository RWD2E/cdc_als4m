select * from PAT_TABLE1;
select * from ALS_CASE_TABLE1;
select * from DM_TABLE1;
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
               year(ehr_start_date) AS year, 
               year(ehr_end_date) as censor_year
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
               year(partd_start_date) AS year, 
               year(partd_end_date) as censor_year
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


/* collect annual summaries - per-source*/

create or replace table patcnt_long (
    CALYR int, 
    DATA_SRC varchar(4),
    PAT_CNT_TYPE varchar(30),
    PAT_CNT int
)
;

-- denominators
insert into patcnt_long
select year, 'MIX', 'ALL', count(distinct patid)
from PAT_YEAR_ALL
group by year
union 
select 9999, 'MIX', 'ALL', count(distinct patid)
from PAT_YEAR_ALL
union 
select year, 'MIX', 'XWALK', count(distinct patid)
from XWALK_YEAR_ALL
group by year
union 
select 9999, 'MIX', 'XWALK', count(distinct patid)
from XWALK_YEAR_ALL
union
select year, 'EHR', 'ALL', count(distinct patid) 
from EHR_YEAR_ALL
group by year
union 
select 9999, 'EHR', 'ALL', count(distinct patid)
from EHR_YEAR_ALL
union 
select year, 'CMS', 'ALL', count(distinct patid)
from PARTD_YEAR_ALL
group by year
union 
select 9999, 'CMS', 'ALL', count(distinct patid)
from PARTD_YEAR_ALL
;

-- DM, non-DM, T1DM, T2DM
insert into patcnt_long
select a.year, 'MIX', 'DM', count(distinct a.patid)
from PAT_YEAR_ALL a 
where exists (select 1 from DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'DM',  count(distinct a.patid)
from PAT_YEAR_ALL a 
where exists (select 1 from DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
union
select a.year, 'MIX', 'T2DM', count(distinct a.patid)
from PAT_YEAR_ALL a 
where exists (select 1 from DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 0)
group by a.year
union 
select 9999, 'MIX', 'T2DM', count(distinct a.patid) 
from PAT_YEAR_ALL a 
where exists (select 1 from DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 0)
union 
select a.year, 'MIX', 'T1DM', count(distinct a.patid) 
from PAT_YEAR_ALL a 
where exists (select 1 from DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 1)
group by a.year
union 
select 9999, 'MIX', 'T1DM', count(distinct a.patid) 
from PAT_YEAR_ALL a 
where exists (select 1 from DM_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 1)
union 
select a.year, 'EHR','DM', count(distinct a.patid) 
from EHR_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR','DM', count(distinct a.patid)
from EHR_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
union
select a.year, 'EHR', 'T2DM', count(distinct a.patid)
from EHR_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 0)
group by a.year
union 
select 9999, 'EHR', 'T2DM', count(distinct a.patid) 
from EHR_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 0)
union 
select a.year, 'EHR', 'T1DM', count(distinct a.patid) 
from EHR_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 1)
group by a.year
union 
select 9999, 'EHR', 'T1DM', count(distinct a.patid) 
from EHR_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 1)
union
select a.year, 'CMS', 'DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
union
select a.year, 'CMS', 'T2DM', count(distinct a.patid)
from PARTD_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 0)
group by a.year
union 
select 9999, 'CMS', 'T2DM', count(distinct a.patid) 
from PARTD_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 0)
union 
select a.year, 'CMS', 'T1DM', count(distinct a.patid) 
from PARTD_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 1)
group by a.year
union 
select 9999, 'CMS', 'T1DM', count(distinct a.patid) 
from PARTD_YEAR_ALL a 
where exists (select 1 from DM_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 1)
;


-- ALS, new ALS
insert into patcnt_long
select a.year, 'MIX', 'ALS', count(distinct a.patid)
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'ALS', count(distinct a.patid)
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
union 
select a.year, 'MIX', 'newALS', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
group by a.year
union 
select 9999, 'MIX', 'newALS', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
union
select a.year, 'EHR', 'ALS', count(distinct a.patid)
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'ALS', count(distinct a.patid)
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
union
select a.year, 'EHR', 'newALS', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
group by a.year
union 
select 9999, 'EHR', 'newALS', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
union
select a.year, 'CMS', 'ALS', count(distinct a.patid)
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'ALS', count(distinct a.patid)
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
union
select a.year, 'CMS', 'newALS', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
group by a.year
union 
select 9999, 'CMS', 'newALS', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
;

-- ALS+DM, ALS+T2DM, ALS+T1DM, newALS+DM, newALS+T2DM, newALS+T1DM
insert into patcnt_long
select a.year, 'MIX','ALS_DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX','ALS_DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'MIX','ALS_T2DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
group by a.year
union 
select 9999, 'MIX','ALS_T2DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
union 
select a.year, 'MIX','ALS_T1DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
group by a.year
union 
select 9999, 'MIX','ALS_T1DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
union
select a.year, 'EHR', 'ALS_DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'ALS_DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'EHR','ALS_T2DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
group by a.year
union 
select 9999, 'EHR','ALS_T2DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
union 
select a.year, 'EHR','ALS_T1DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
group by a.year
union 
select 9999, 'EHR','ALS_T1DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
union
select a.year, 'CMS', 'ALS_DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'ALS_DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'CMS','ALS_T2DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
group by a.year
union 
select 9999, 'CMS','ALS_T2DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
union 
select a.year, 'CMS','ALS_T1DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
group by a.year
union 
select 9999, 'CMS','ALS_T1DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
union
select a.year, 'MIX', 'newALS_DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'newALS_DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'MIX','newALS_T2DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
group by a.year
union 
select 9999, 'MIX','newALS_T2DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
union 
select a.year, 'MIX','newALS_T1DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
group by a.year
union 
select 9999, 'MIX','newALS_T1DM', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1 b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
union
select a.year, 'EHR', 'newALS_DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'newALS_DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'EHR','newALS_T2DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
group by a.year
union 
select 9999, 'EHR','newALS_T2DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
union 
select a.year, 'EHR','newALS_T1DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
group by a.year
union 
select 9999, 'EHR','newALS_T1DM', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_EHR b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
union
select a.year, 'CMS', 'newALS_DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'newALS_DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'CMS','newALS_T2DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
group by a.year
union 
select 9999, 'CMS','newALS_T2DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 0)
union 
select a.year, 'CMS','newALS_T1DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
group by a.year
union 
select 9999, 'CMS','newALS_T1DM', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from ALS_INC_CASE_TABLE1_CMS b where a.patid = b.patid and year(b.index_date)=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year and d.t1dm_ind = 1)
;

-- DM + GLP1/DPP4
insert into patcnt_long
select a.year, 'MIX', 'DM_GLP1', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'DM_GLP1', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'MIX', 'DM_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'DM_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'MIX', 'DM_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'DM_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'EHR', 'DM_GLP1', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'DM_GLP1', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'EHR', 'DM_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'DM_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'EHR', 'DM_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'DM_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'CMS', 'DM_GLP1', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'DM_GLP1', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'CMS', 'DM_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'DM_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'CMS', 'DM_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'DM_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
;

-- DM+ALS + GLP1/DPP4
insert into patcnt_long
select a.year, 'MIX', 'DM_ALS_GLP1', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'DM_ALS_GLP1', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'MIX', 'DM_ALS_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'DM_ALS_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'MIX', 'DM_ALS_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from ALS_CASE_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'MIX', 'DM_ALS_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PAT_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1 b where a.patid = b.patid)
    and exists (select 1 from ALS_CASE_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1 d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'EHR', 'DM_ALS_GLP1', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'DM_ALS_GLP1', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'EHR', 'DM_ALS_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'DM_ALS_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'EHR', 'DM_ALS_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid)
    and exists (select 1 from ALS_CASE_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'EHR', 'DM_ALS_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from EHR_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_EHR b where a.patid = b.patid)
    and exists (select 1 from ALS_CASE_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_EHR d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'CMS', 'DM_ALS_GLP1', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'DM_ALS_GLP1', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.glp1_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
union 
select a.year, 'CMS', 'DM_ALS_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'DM_ALS_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid and b.dpp4_start_date is not null)
    and exists (select 1 from ALS_CASE_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
union
select a.year, 'CMS', 'DM_ALS_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid)
    and exists (select 1 from ALS_CASE_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
group by a.year
union 
select 9999, 'CMS', 'DM_ALS_GLP1_DPP4', count(distinct a.patid) as pat_cnt 
from PARTD_YEAR_ALL a 
where exists (select 1 from GLP1_DPP4_TABLE1_CMS b where a.patid = b.patid)
    and exists (select 1 from ALS_CASE_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
    and exists (select 1 from DM_TABLE1_CMS d where a.patid = d.patid and year(d.index_date)<=a.year)
;

select * from patcnt_long limit 5;

/* standardized population: census */
select * from REF_ACS_SEXBYAGE;

create or replace table adj_prev (
    CALYR int, 
    DATA_SRC varchar(4),
    STD_POP varchar(10),
    PREV_TYPE varchar(10),
    PREV_CRD double,
    PREV_ADJ double
)
;
-- Mix Cohort
insert into adj_prev (calyr, data_src, std_pop, prev_type, prev_crd, prev_adj)
with denom_cte as (
    select  distinct
            a.patid, 
            a.year, 
            a.year - year(c.birth_date) as age,
            c.sex, 
            r.age as age_grp,
            r.wt
    from PAT_YEAR_ALL a 
    join PAT_TABLE1 c on a.patid = c.patid
    join REF_ACS_SEXBYAGE r on a.year = r.yr and c.sex = r.sex and a.year - year(c.birth_date) between r.age_lb and r.age_ub
), denom_yr_cte as (
    select  year,sex,age_grp,wt,
            count(distinct patid) as denom
    from denom_cte 
    group by year,sex,age_grp,wt
    union 
    select 9999, year,sex,age_grp,wt,
           count(distinct patid) as denom
    from denom_cte 
    group by sex,age_grp,wt
), num_als_cte as (
    select a.*
    from denom_cte a 
    where exists (
        select 1 from ALS_CASE_TABLE1 c 
        where a.patid = c.patid and year(c.index_date)<=a.year
    )
), num_dm_cte as (
    select a.*
    from denom_cte a 
    where exists (
        select 1 from DM_TABLE1 c 
        where a.patid = c.patid and year(c.index_date)<=a.year
    )
), num_t1dm_cte as (
    select a.*
    from denom_cte a 
    where exists (
        select 1 from DM_TABLE1 c 
        where a.patid = c.patid and year(c.index_date)<=a.year and c.t1dm_ind = 1
    )
), num_t2dm_cte as (
    select a.*
    from denom_cte a 
    where exists (
        select 1 from DM_TABLE1 c 
        where a.patid = c.patid and year(c.index_date)<=a.year and c.t1dm_ind = 0
    )
), num_dm_als_cte as (
    select a.*
    from denom_cte a 
    where exists (
        select 1 from DM_TABLE1 b 
        where a.patid = b.patid and year(b.index_date)<=a.year
    ) and exists (
        select 1 from ALS_CASE_TABLE1 c 
        where a.patid = c.patid and year(c.index_date)<=a.year
    )
), num_t1dm_als_cte as (
    select a.*
    from denom_cte a 
    where exists (
        select 1 from DM_TABLE1 b 
        where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 1
    ) and exists (
        select 1 from ALS_CASE_TABLE1 c 
        where a.patid = c.patid and year(c.index_date)<=a.year
    )
), num_t2dm_als_cte as (
    select a.*
    from denom_cte a 
    where exists (
        select 1 from DM_TABLE1 b 
        where a.patid = b.patid and year(b.index_date)<=a.year and b.t1dm_ind = 0
    ) and exists (
        select 1 from ALS_CASE_TABLE1 c 
        where a.patid = c.patid and year(c.index_date)<=a.year
    )
), num_als_yr_cte as (
    select  year,sex,age_grp,wt,
            count(distinct patid) as cnt,
            sum(wt) as num
    from num_als_cte 
    group by year,sex,age_grp,wt
    union 
    select 9999,sex,age_grp,wt,
           count(distinct patid) as cnt,
           sum(wt) as num
    from num_als_cte 
    group by sex,age_grp,wt
), num_dm_yr_cte as (
    select  year,sex,age_grp,wt, 
            count(distinct patid) as cnt,
            sum(wt) as num
    from num_dm_cte 
    group by year,sex,age_grp,wt
    union 
    select 9999,sex,age_grp,wt
           count(distinct patid) as cnt,
           sum(wt) as num
    from num_dm_cte 
    group by sex,age_grp,wt
), num_t1dm_yr_cte as (
    select  year,sex,age_grp,wt 
            count(distinct patid) as cnt,
            sum(wt) as num
    from num_t1dm_cte 
    group by year,sex,age_grp,wt
    union 
    select 9999,sex,age_grp,wt,
           count(distinct patid) as cnt,
           sum(wt) as num
    from num_t1dm_cte 
    group by sex,age_grp,wt
), num_t2dm_yr_cte as (
    select  year,sex,age_grp,wt, 
            count(distinct patid) as cnt,
            sum(wt) as num
    from num_t2dm_cte 
    group by year,sex,age_grp,wt
    union 
    select 9999,sex,age_grp,wt,
           count(distinct patid) as cnt,
           sum(wt) as num
    from num_t2dm_cte 
    group by sex,age_grp,wt
), num_dm_als_yr_cte as (
    select  year,sex,age_grp,wt, 
            count(distinct patid) as cnt,
            sum(wt) as num
    from num_dm_als_cte 
    group by year,sex,age_grp,wt
    union 
    select 9999,sex,age_grp,wt
           count(distinct patid) as cnt,
           sum(wt) as num
    from num_dm_als_cte 
    group by sex,age_grp,wt
), num_t1dm_als_yr_cte as (
    select  year,sex,age_grp,wt, 
            count(distinct patid) as cnt,
            sum(wt) as num
    from num_t1dm_als_cte 
    group by year
    union 
    select 9999,
           count(distinct patid) as cnt,
           sum(wt) as num
    from num_t1dm_als_cte 
    order by year
), num_t2dm_als_yr_cte as (
    select  year, 
            count(distinct patid) as cnt,
            sum(wt) as num
    from num_t2dm_als_cte 
    group by year
    union 
    select 9999,
           count(distinct patid) as cnt,
           sum(wt) as num
    from num_t2dm_als_cte 
    order by year
)
select n.year,
       'MIX',
       'POP',
       'ALS',
       n.cnt/d.denom*100000 as prev_crd,
       n.num/d.denom*100000 as prev_adj
from num_als_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'MIX',
       'POP',
       'DM',
       n.cnt/d.denom*100000 as prev_crd,
       n.num/d.denom*100000 as prev_adj
from num_dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'MIX',
       'POP',
       'T1DM',
       n.cnt/d.denom*100000 as prev_crd,
       n.num/d.denom*100000 as prev_adj
from num_t1dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'MIX',
       'POP',
       'T2DM',
       n.cnt/d.denom*100000 as prev_crd,
       n.num/d.denom*100000 as prev_adj
from num_t2dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'MIX',
       'POP',
       'ALSinDM',
       n.cnt/d.denom*100000 as prev_crd,
       n.num/d.denom*100000 as prev_adj
from num_dm_als_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'MIX',
       'POP',
       'ALSinNONDM',
       n.cnt/d.denom*100000 as prev_crd,
       1-n.num/d.denom*100000 as prev_adj
from num_dm_als_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'MIX',
       'POP',
       'ALSinT1DM',
       n.cnt/d.denom*100000 as prev_crd,
       n.num/d.denom*100000 as prev_adj
from num_t1dm_als_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'MIX',
       'POP',
       'ALSinT2DM',
       n.cnt/d.denom*100000 as prev_crd,
       n.num/d.denom*100000 as prev_adj
from num_t2dm_als_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
;

select * from adj_prev 
where PREV_TYPE = 'ALS'
-- where PREV_TYPE = 'ALSinDM'
-- where PREV_TYPE = 'DM'
order by CALYR;


insert into adj_prev (calyr, data_src, std_pop, prev_type, prev_adj)
with denom_cte as (
    select  distinct
            a.patid, 
            a.year, 
            c.sex, 
            a.year - year(c.birth_date) as age,
            round(r.wt,6) as wt
    from EHR_YEAR_ALL a 
    join PAT_TABLE1 c on a.patid = c.patid
    join REF_ACS_SEXBYAGE r on a.year = r.yr and c.sex = r.sex and a.year - year(c.birth_date) between r.age_lb and r.age_ub
), denom_yr_cte as (
    select  year,
            count(distinct patid) as denom
    from denom_cte 
    group by year
    union 
    select 9999,
           count(distinct patid) as denom
    from denom_cte 
), num_als_cte as (
    select a.*
    from denom_cte a 
    join ALS_CASE_TABLE1_EHR c on a.patid = c.patid    
), num_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_als_cte 
    order by year
), num_dm_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_EHR c on a.patid = c.patid 
), num_dm_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_dm_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_dm_cte 
    order by year
), num_t1dm_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_EHR c on a.patid = c.patid 
    where c.t1dm_ind = 1
), num_t1dm_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t1dm_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t1dm_cte 
    order by year
), num_t2dm_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_EHR c on a.patid = c.patid 
    where c.t1dm_ind = 0
), num_t2dm_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t2dm_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t2dm_cte 
    order by year
), num_dm_als_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_EHR b on a.patid = b.patid 
    join ALS_CASE_TABLE1_EHR c on a.patid = c.patid 
), num_t1dm_als_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_EHR b on a.patid = b.patid 
    join ALS_CASE_TABLE1_EHR c on a.patid = c.patid 
    where b.t1dm_ind = 1
), num_t2dm_als_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_EHR b on a.patid = b.patid 
    join ALS_CASE_TABLE1_EHR c on a.patid = c.patid 
    where b.t1dm_ind = 0
), num_dm_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_dm_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_dm_als_cte 
    order by year
), num_t1dm_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t1dm_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t1dm_als_cte 
    order by year
), num_t2dm_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t2dm_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t2dm_als_cte 
    order by year
)
select n.year,
       'EHR',
       'POP',
       'ALS',
       n.num/d.denom*100000 as prev_adj
from num_als_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'EHR',
       'POP',
       'DM',
       n.num/d.denom*100000 as prev_adj
from num_dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'EHR',
       'POP',
       'T1DM',
       n.num/d.denom*100000 as prev_adj
from num_t1dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'EHR',
       'POP',
       'T2DM',
       n.num/d.denom*100000 as prev_adj
from num_t2dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'EHR',
       'POP',
       'ALSinDM',
       n.num/d.num*100000 as prev_adj
from num_dm_als_yr_cte n
join num_dm_yr_cte d 
on n.year = d.year 
union
select n.year,
       'EHR',
       'POP',
       'ALSinNONDM',
       (1-n.num/d.num)*100000 as prev_adj
from num_dm_als_yr_cte n
join num_dm_yr_cte d 
on n.year = d.year 
union
select n.year,
       'EHR',
       'POP',
       'ALSinT1DM',
       n.num/d.num*100000 as prev_adj
from num_t1dm_als_yr_cte n
join num_t1dm_yr_cte d 
on n.year = d.year 
union
select n.year,
       'EHR',
       'POP',
       'ALSinT2DM',
       n.num/d.num*100000 as prev_adj
from num_t2dm_als_yr_cte n
join num_t2dm_yr_cte d 
on n.year = d.year 
;

insert into adj_prev (calyr, data_src, std_pop, prev_type, prev_adj)
with denom_cte as (
    select  distinct
            a.patid, 
            a.year, 
            c.sex, 
            a.year - year(c.birth_date) as age,
            round(r.wt,6) as wt
    from PARTD_YEAR_ALL a 
    join PAT_TABLE1 c on a.patid = c.patid
    join REF_ACS_SEXBYAGE r on a.year = r.yr and c.sex = r.sex and a.year - year(c.birth_date) between r.age_lb and r.age_ub
), denom_yr_cte as (
    select  year,
            round(sum(wt)) as denom
    from denom_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as denom
    from denom_cte 
), num_als_cte as (
    select a.*
    from denom_cte a 
    join ALS_CASE_TABLE1_CMS c on a.patid = c.patid    
), num_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_als_cte 
    order by year
), num_dm_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_CMS c on a.patid = c.patid 
), num_dm_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_dm_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_dm_cte 
    order by year
), num_t1dm_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_CMS c on a.patid = c.patid 
    where c.t1dm_ind = 1
), num_t1dm_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t1dm_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t1dm_cte 
    order by year
), num_t2dm_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_CMS c on a.patid = c.patid 
    where c.t1dm_ind = 0
), num_t2dm_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t2dm_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t2dm_cte 
    order by year
), num_dm_als_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_CMS b on a.patid = b.patid 
    join ALS_CASE_TABLE1_CMS c on a.patid = c.patid 
), num_t1dm_als_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_CMS b on a.patid = b.patid 
    join ALS_CASE_TABLE1_CMS c on a.patid = c.patid 
    where b.t1dm_ind = 1
), num_t2dm_als_cte as (
    select a.*
    from denom_cte a 
    join DM_TABLE1_CMS b on a.patid = b.patid 
    join ALS_CASE_TABLE1_CMS c on a.patid = c.patid 
    where b.t1dm_ind = 0
), num_dm_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_dm_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_dm_als_cte 
    order by year
), num_t1dm_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t1dm_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t1dm_als_cte 
    order by year
), num_t2dm_als_yr_cte as (
    select  year, 
            round(sum(wt)) as num
    from num_t2dm_als_cte 
    group by year
    union 
    select 9999,
           round(sum(wt)) as num
    from num_t2dm_als_cte 
    order by year
)
select n.year,
       'CMS',
       'POP',
       'ALS',
       n.num/d.denom*100000 as prev_adj
from num_als_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'CMS',
       'POP',
       'DM',
       n.num/d.denom*100000 as prev_adj
from num_dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'CMS',
       'POP',
       'T1DM',
       n.num/d.denom*100000 as prev_adj
from num_t1dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'CMS',
       'POP',
       'T2DM',
       n.num/d.denom*100000 as prev_adj
from num_t2dm_yr_cte n
join denom_yr_cte d 
on n.year = d.year 
union
select n.year,
       'CMS',
       'POP',
       'ALSinDM',
       n.num/d.num*100000 as prev_adj
from num_dm_als_yr_cte n
join num_dm_yr_cte d 
on n.year = d.year 
union
select n.year,
       'CMS',
       'POP',
       'ALSinNONDM',
       (1-n.num/d.num)*100000 as prev_adj
from num_dm_als_yr_cte n
join num_dm_yr_cte d 
on n.year = d.year 
union
select n.year,
       'CMS',
       'POP',
       'ALSinT1DM',
       n.num/d.num*100000 as prev_adj
from num_t1dm_als_yr_cte n
join num_t1dm_yr_cte d 
on n.year = d.year 
union
select n.year,
       'CMS',
       'POP',
       'ALSinT2DM',
       n.num/d.num*100000 as prev_adj
from num_t2dm_als_yr_cte n
join num_t2dm_yr_cte d 
on n.year = d.year 
;

select * from adj_prev;
