### Identify first events for each state-chid pair in NCANDS 2000 - current
### Output index files with state-chid pairs and rptdts for each first event
### last edited 8/5/21
### configured for libra

library(data.table)
library(tidyverse)
library(lubridate)

### create list of files to read, pull IDs and victim flag
ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")
### fread in with column subset
ncands<-lapply(ncands_files, function(x){
  fread(x,
        select = c("subyr", "StaTerr", 
                   "RptFIPS",
                   "ChID", "RptDt",
                   "RptVictim"))
  })

### unlist
ncands<-ncands %>% 
  bind_rows() %>% 
  rename_all(tolower)

### create st_id as staterr, chid pair
ncands<-ncands %>% 
  mutate(st_id = paste(staterr, chid, sep = ""))

### use duplicated st_id to grab first event

first_report<-ncands %>% 
  filter(!duplicated(st_id)) %>% 
  select(st_id, rptdt, subyr, rptfips) 

first_victim<-ncands %>% 
  filter(rptvictim==1) %>% 
  filter(!duplicated(st_id)) %>% 
  select(st_id, rptdt, subyr, rptfips) 

### output indices

write.csv(first_report, 
          "./data/ncands_first_report_index.csv",
          row.names = F)

write.csv(first_victim, 
          "./data/ncands_first_victim_index.csv",
          row.names = F)

