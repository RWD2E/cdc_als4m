rm(list=ls()); gc()
setwd("C:/repo/GPC-Analytics-ALS-Cohort")

pacman::p_load(tidyverse,
               rvest,
               magrittr)

# ALSA certified center or clinic
alsa_long<-data.frame(var=as.character(),
                      val=as.character(),
                      idx=as.integer(),
                      stringsAsFactors = F)
html_obj<-"start"
pg<-0
center_idx<-1
while(length(html_obj)!=0){
  url<-sprintf("https://www.als.org/local-support/certified-centers-clinics?search_api_fulltext=&page=%s",pg)
  html_obj<-read_html(url) %>%
    # inspect element and identify html class tag
    html_nodes(".centers-clinics-teaser__text") %>%
    html_text()
  
  for(i in seq_along(html_obj)){
    df<-read_delim(html_obj[i],delim="\n",
                   trim_ws=TRUE,
                   col_names=FALSE,
                   show_col_types = FALSE) %>%
      mutate(rowid=case_when(!grepl("^\\:+",X1) ~ row_number(),
                             TRUE ~ NA_integer_)) %>%
      fill(rowid,.direction="down") %>% 
      mutate(X1=gsub(":","",X1)) %>%
      group_by(rowid) %>% 
      mutate(X1=paste0(tolower(X1),collapse = ":")) %>%
      slice(1:1) %>% ungroup %>%
      filter(!grepl("website",X1)) %>%
      separate(X1,c("var","val"),sep=":",extra="merge",fill="left") %>%
      mutate(var=case_when(!is.na(var) ~ var,
                           row_number()==1 ~ "chapter",
                           row_number()==2 ~ "provider_name",
                           row_number()==n() ~ "center_type",
                           str_detect(val,"(\\d[.-])?\\(?\\d{3}\\)?[-. ]+\\d{3}[-. ]?(\\d{4}|\\w{4})\\b") ~ "phone",
                           TRUE ~ "address"),
             val=case_when(str_detect(val,"(\\d[.-])?\\(?\\d{3}\\)?[-. ]+\\d{3}[-. ]?(\\d{4}|\\w{4})\\b") ~ str_extract(val,"(\\d[.-])?\\(?\\d{3}\\)?[-. ]+\\d{3}[-. ]?(\\d{4}|\\w{4})\\b"),
                           TRUE ~ trimws(val))) %>%
      group_by(var) %>% mutate(rowid=row_number()) %>% ungroup %>%
      mutate(var = case_when(var=="address"~paste0(var,"_",rowid),
                             TRUE ~ var)) %>%
      select(-rowid)
    
    alsa_long %<>% 
      bind_rows(df %>% mutate(idx=center_idx))
    
    center_idx<-center_idx+1
  }
  pg<-pg+1
}

# MDA care clinics
mda_long<-data.frame(center_type=as.character(),
                     provider_name=as.character(),
                     phone=as.character(),
                     address=as.character(),
                     address_line=as.character(),
                     idx=as.integer(),
                     stringsAsFactors = F)

url<-"https://www.mda.org/care/care-center-list"
html_obj<-read_html(url) 
title<-html_obj %>%
  # inspect element and identify html class tag
  html_nodes(".views-field-title") %>%
  html_text()

address<-html_obj %>%
  # inspect element and identify html class tag
  html_nodes(".views-field-address") %>%
  html_text()

phone<-html_obj %>%
  # inspect element and identify html class tag
  html_nodes(".views-field-field-phone") %>%
  html_text()

for(i in seq_along(title)[-1]){
  center_idx<-center_idx+1
  title_i<-read_delim(title[i],delim="\n",
                      trim_ws=TRUE,
                      col_names=FALSE,
                      show_col_types = FALSE) %>%
    mutate(X1=gsub("MDA","; MDA",X1)) %>%
    separate(X1,c("provider_name","center_type"),sep=";",extra="merge",fill="right") %>%
    mutate(provider_name=tolower(provider_name),
           center_type=case_when(grepl("als center",provider_name) ~ "als center",
                                 TRUE ~ tolower(trimws(center_type)))) %>%
    replace_na(list(center_type="mda clinic"))
  
  address_i<-read_delim(address[i],delim="\n",
                        trim_ws=TRUE,
                        col_names=FALSE,
                        show_col_types = FALSE) %>%
    slice(-1) %>%
    mutate(rowid=row_number(),
           var=case_when(str_detect(X1,"(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b")&!str_detect(X1, "[:alpha:]+$")~"zip",
                         grepl("^(,)+",X1)~"state")) %>%
    mutate(var2=var) %>% fill(var2,.direction="up") %>%
    mutate(var=case_when(is.na(var)&var2=="state" ~ "city",
                         !is.na(var) ~ var)) %>%
    mutate(X1=trimws(gsub(",","",tolower(X1)))) %>%
    filter(!grepl("(see map)+",X1)) %>% rename(address = X1)
  
  address_reorg<-address_i %>% 
    filter(!is.na(var)) %>% select(address,var) %>% spread(var,address) %>% 
    mutate(address=paste0(city,", ",state," ",zip))
  
  address_i %<>%
    filter(is.na(var)) %>% select(address) %>%
    bind_rows(address_reorg %>% select(address)) %>%
    mutate(address_line=paste0("address_",row_number()))
  
  phone_i<-read_delim(phone[i],delim="\n",
                      trim_ws=TRUE,
                      col_names=FALSE,
                      show_col_types = FALSE) %>%
    mutate(phone=str_extract(X1,"(\\d[.-])?\\(?\\d{3}\\)?[-. ]?\\d{3}[-. ]?(\\d{4}|\\w{4})\\b")) %>%
    select(phone)
  
  mda_long %<>%
    bind_rows(data.frame(cbind(title_i,address_i,phone_i)) %>%
                mutate(idx=center_idx))
}

center_tbl<-alsa_long %>% spread(var,val) %>%
  bind_rows(mda_long %>% spread(address_line,address)) %>%
  mutate(phone=gsub("(\\(|\\)|-|\\s)+","",gsub("als","257",phone))) %>%
  group_by(phone) %>% arrange(idx) %>% slice(1:1) %>% ungroup %>%
  mutate(phone=case_when(grepl("vanderbilt university medical center als clinic",provider_name) ~ "6159360060",
                         TRUE ~ phone),
         address_3 = case_when(is.na(address_3) ~ address_2,
                               TRUE ~ address_3),
         address_2 = case_when(address_2==address_3 ~ NA_character_,
                               TRUE ~ address_2)) %>%
  mutate(idx=row_number(),
         address_3=gsub(",",";",address_3),
         address_2=gsub(",",";",address_2),
         address_1=gsub(",",";",address_1),
         provider_name=gsub(",","-",provider_name)) %>%
  select(idx,provider_name,center_type,phone,address_1,address_2,address_3)

write.csv(center_tbl,file="./ref/als_mda_coa.csv",row.names = F)
