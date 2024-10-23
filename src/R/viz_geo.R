rm(list=ls()); gc()
setwd("C:/repo/cdc_als4m")

# install.packages("pacman")
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  magrittr,
  dbplyr,
  ggrepel,
  stringr,
  ggpubr,
  kableExtra,
  maps,
  grid,
  gridExtra,
  ggthemes
)

# make db connection
# sf_conn <- DBI::dbConnect(
#   drv = odbc::odbc(),
#   dsn = Sys.getenv("ODBC_DSN_NAME"),
#   uid = Sys.getenv("SNOWFLAKE_USER"),
#   pwd = Sys.getenv("SNOWFLAKE_PWD")
# )
# 
# #==== Load ALS Geo Table ====
# dat<-tbl(sf_conn,in_schema("SX_ALS_GPC","ALS_GEO_FINAL")) %>% collect()
# nm<-colnames(dat)
# colnames(dat)<-gsub("'",'',nm)
# saveRDS(dat,file="./data/als_geo_tbl1.rds")

# get county and state map
us_county<-map_data("county") %>% 
  filter(long>=-116&long<=-85)
us_state<-map_data("state") %>% 
  filter(long>=-116&long<=-85)

# link with data
dat_prev<-readRDS("./data/als_geo_tbl1.rds") %>% 
  select(CALYR,FIPS,CMS_TOTAL,ALS_TOTAL,ALS_PROP) %>%
  group_by(FIPS) %>%
  summarise(
    ALS_TOTAL_mean = mean(ALS_TOTAL),
    ALS_TOTAL_sd = sd(ALS_TOTAL),
    ALS_PREV_mean = mean(ALS_PROP*100000),
    ALS_PREV_sd = sd(ALS_PROP*100000),
    .groups =
  ) %>%
  mutate(FIPS = as.numeric(FIPS)) %>%
  inner_join(
    county.fips,by=c("FIPS"="fips"),relationship = "many-to-many"
  ) %>%
  separate("polyname",c('region','subregion'),",") %>%
  left_join(
    us_county,by=c('region','subregion'),relationship = "many-to-many"
  ) %>% 
  filter(long>=-116&long<=-85) %>%
  mutate(
    ALS_TOTAL_mean = case_when(ALS_TOTAL_mean>0~ALS_TOTAL_mean,TRUE~ NA_integer_),
    ALS_PREV_mean = case_when(ALS_PREV_mean>0~ALS_PREV_mean,TRUE~ NA_integer_)
  )

# annotate state 
st_annotate<-data.frame(
  long=state.center$x,
  lat=state.center$y,
  state_abb=state.abb) %>% 
  filter(long>=-116&long<=-85)

# annotate county
# cnty_annotate<-us_county %>%
#   group_by(group,region,subregion) %>%
#   summarise(long=mean(range(long)),lat=mean(range(lat)),.groups="drop") %>% 
#   filter(long>=-116&long<=-85)

# thematic map
ggplot()+
  geom_polygon(
    data=dat_prev,
    aes(x=long,y=lat,group=group,fill=ALS_PREV_mean)
  )+
  geom_path(data = us_state,
            aes(x=long,y=lat,group=group),
            colour = "black", size = .3)+
  geom_path(data = dat_prev,
            aes(x=long,y=lat,group=group),
            size = .6, alpha = .8) +
  scale_fill_gradientn(
    "ALS Patient Counts \n (per 100K CMS population)",
    colours = topo.colors(11),
    trans = "reverse",
    breaks=seq(0,160,by=20),
    labels=seq(0,160,by=20)
  )+
  geom_text(data=st_annotate,aes(x=long,y=lat,label=state_abb),size=3)+
  labs(x="longitude", y="latitude")+
  theme_classic()+
  theme(
    legend.position="bottom",
    legend.text = element_text(angle = 90),
    text = element_text(face = "bold")
  )

ggsave(
  file=file.path(getwd(),"res","als_prev_10yr.pdf"),
  width=10,
  height=8
)

#==== Missing Rates ====
na_rt<-readRDS("./data/als_geo_tbl1.rds") %>% 
  summarise(across(everything(), ~ mean(is.na(.x)))) %>%
  pivot_longer(
    cols = -CALYR,
    names_to = "var",
    values_to = "missing_rt"
  )

hist(na_rt$missing_rt)
