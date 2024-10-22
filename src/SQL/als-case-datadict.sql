
select * from ALS_ALL_PHECD limit 5;
select * from ALS_ALL_PX_CCS limit 5;
select * from ALS_SEL_SDOH limit 5;
select * from ALS_ALL_RX_IN limit 5;
select * from SX_SDOH.S_SDH_SEL limit 5;
select * from ALS_SEL_DEVICE limit 5;
select * from ALS_PX_PRVDR limit 5;
select * from ALS_SEL_OBS limit 5;

create or replace table DATA_DICT(
    VAR varchar(500), 
    VAR_LABEL varchar(5000), 
    VAR_DOMAIN varchar(20)
);
-- dx
insert into DATA_DICT
select distinct 'PHECD_' || PHECD_DXGRPCD, PHECD_DXGRP, 'DX'
from ALS_ALL_PHECD
;
-- px
insert into DATA_DICT
select distinct 'PXCCS_' || CCS_PXGRPCD, CCS_PXGRP, 'PX'
from ALS_ALL_PX_CCS
;
-- rx
insert into DATA_DICT
select distinct 'RX_' || INGREDIENT, INGREDIENT, 'RX'
from ALS_ALL_RX_IN
;
-- sdh
insert into DATA_DICT
select distinct 'PAST_SDH_' || VAR, VAR_LABEL, 'SDH'
from SX_SDOH.S_SDH_SEL
;
insert into DATA_DICT
select distinct 'CURR_SDH_' || VAR, VAR_LABEL, 'SDH'
from SX_SDOH.S_SDH_SEL
;
-- obs
insert into DATA_DICT
select distinct 'LAB_' || OBS_NAME, OBS_NAME, 'LAB'
from ALS_SEL_OBS
;
-- provider
insert into DATA_DICT
select distinct 'PRVDR_' || SPECIALTY_GROUP, SPECIALTY_GROUP, 'PRVDR'
from ALS_PX_PRVDR
;
-- device
insert into DATA_DICT
select distinct 'TX_' || DEVICE, DEVICE, 'TX'
from ALS_SEL_DEVICE
;
select var_domain, count(distinct var)
from DATA_DICT
group by var_domain
order by var_domain;

-- DX	682
-- LAB	32
-- PRVDR	22
-- PX	246
-- RX	1453
-- SDH	590
-- TX	3
