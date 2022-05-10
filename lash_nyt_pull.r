
rm(list=ls()); gc()
library(lubridate)
library(data.table)
library(tidyverse)
library(mice)

ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")
#### grab 18-20 for time series
ncands_files<-paste(ncands_path, list.files(ncands_path), sep = "")[17:19]

ncands<-lapply(ncands_files, fread)

### recode / process annual files, select neeeded variables
for(i in 1:length(ncands_files)){
  ncands[[i]]<-ncands[[i]]%>%
    rename_all(tolower) %>%
    mutate(race_ethn =
             ifelse(chracbl==1,
                    "Black",
                    ifelse(chracai==1, "AIAN",
                           ifelse(chracas==1 | chracnh==1,
                                  "API",
                                  ifelse(cethn==1, "Latinx",
                                         ifelse(chracwh == 1, "White",
                                                NA)))))) %>%
    select(chid, 
           subyr, 
           staterr,
           rptfips, 
           rptdt,
           chage, 
           rptsrc,
           race_ethn,
           chprior, 
           rptvictim,
           subyr,
           stfcid,
           fostercr,
           chmal1,
           chmal2,
           chmal3,
           chmal4)
}

ncands<-bind_rows(ncands)

## recode prenatal, miscoded ages, create year / month rptdt vars
ncands <- ncands %>% 
  rename(age = chage) %>% 
  mutate(age = ifelse(age==77, 0, age)) %>% 
  mutate(age = ifelse(age>18, 
                      NA,
                      age)) %>% 
  mutate(rptdt = ymd(rptdt),
         rpt_month = month(rptdt),
         rpt_year = year(rptdt))

### national TS on requested vars
rptsrc_ts<-ncands %>% 
  filter(rpt_year>=2016) %>% 
  mutate(rptsrc = 
      case_when(
        rptsrc==1 ~ "social services",
        rptsrc==2 ~ "medical",
        rptsrc==3 ~ "mental health",
        rptsrc==4 ~ "police",
        rptsrc==5 ~ "education",
        rptsrc==6 ~ "day care",
        rptsrc==7 ~ "substitute care ",
        rptsrc!=99 ~ "non-professional"
      )) %>% 
  group_by(rpt_month, rpt_year, rptsrc, age, rptvictim,
           fostercr, chmal1) %>% 
  summarize(n = n()) 
  
write_csv(rptsrc_ts, "./data/lash_national.csv")


## state TS on requested vars
rptsrc_state<-ncands %>% 
  filter(rpt_year>=2016) %>% 
  mutate(rptsrc = 
           case_when(
             rptsrc==1 ~ "social services",
             rptsrc==2 ~ "medical",
             rptsrc==3 ~ "mental health",
             rptsrc==4 ~ "police",
             rptsrc==5 ~ "education",
             rptsrc==6 ~ "day care",
             rptsrc==7 ~ "substitute care ",
             rptsrc!=99 ~ "non-professional"
           ))  %>% 
  group_by(staterr, rpt_month, rpt_year, rptsrc, age, rptvictim,
           fostercr, chmal1) %>% 
  summarize(n = n()) 

write_csv(rptsrc_state, "./data/lash_state_ts_rpt.csv")