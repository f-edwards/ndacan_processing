#### wrangle imputed ncands into geographic unit time-series for 
#### total and first events
#### last edit 8/5/21

library(tidyverse)
library(data.table)
library(mice)

### grab filenames and format
files<-list.files("./imputations")
ncands_imputations<-files[grep("ncands_imps", files)]
ncands_imputations<-paste("./imputations/", ncands_imputations, sep = "")

### subset to needed files for processing, this grabs 14-18
ncands_imputations<-ncands_imputations[6:9]

### batch process with mice::complete
load(ncands_imputations[1])
temp<-mice::complete(imps, action = "long", include = F)
ncands<-temp %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  select(-pct_aian, -pct_api, -pct_Black, -pct_Hispanic)


for(i in 2:length(ncands_imputations)){
  load(ncands_imputations[i])
  temp<-mice::complete(imps, action = "long", include = F)
  temp<-temp %>% 
    mutate(st_id = paste(staterr, chid, sep = "")) %>% 
    select(-pct_aian, -pct_api, -pct_Black, -pct_Hispanic) 
  ncands<-ncands %>% 
    bind_rows(temp)
}

### join first report index onto imputed
first_inv_index<-read_csv("./data/ncands_first_report_index.csv")

first_inv<-first_inv_index %>% 
  left_join(ncands) %>% 
  filter(!(is.na(.imp)))

### join first victim onto imputed
first_victim_index<-read_csv("./data/ncands_first_victim_index.csv")

first_victim<-first_victim_index %>% 
  left_join(ncands) %>% 
  filter(!(is.na(.imp)))

### Make national, state, county, TS

nat<-first_inv %>% 
  group_by(.imp, age, race_ethn, year) %>% 
  summarise(first_inv = n())

nat<-nat %>% 
  left_join(first_victim %>% 
  group_by(.imp, age, race_ethn, year) %>% 
  summarise(first_victim = n()))

state<-first_inv %>% 
  group_by(.imp, staterr, age, race_ethn, year) %>% 
  summarise(first_inv = n())

state<-state %>% 
  left_join(first_victim %>% 
  group_by(.imp, staterr, age, race_ethn, year) %>% 
  summarise(first_victim = n()))

#### backfill zeroes
temp<-expand_grid(
  .imp = unique(first_inv$.imp),
  staterr = unique(first_inv$staterr),
  age = unique(first_inv$age),
  race_ethn = unique(first_inv$race_ethn),
  year = unique(first_inv$year)
)

state<-temp %>% 
  left_join(state) %>% 
  mutate(first_inv = ifelse(is.na(first_inv),
                                  0,
                                  first_inv),
         first_victim = ifelse(is.na(first_victim),
                               0,
                               first_victim))
####
county<-first_inv %>% 
  group_by(.imp, rptfips, age, race_ethn, year) %>% 
  summarise(first_inv = n())

county<-county %>% 
  left_join(first_victim %>% 
  group_by(.imp, rptfips, age, race_ethn, year) %>% 
  summarise(first_victim = n()))

#### backfill zeroes
temp<-expand_grid(
  .imp = unique(first_inv$.imp),
  rptfips = unique(first_inv$rptfips),
  age = unique(first_inv$age),
  race_ethn = unique(first_inv$race_ethn),
  year = unique(first_inv$year)
)

county<-temp %>% 
  left_join(county) %>% 
  mutate(first_inv = ifelse(is.na(first_inv),
                            0,
                            first_inv),
         first_victim = ifelse(is.na(first_victim),
                               0,
                               first_victim))
### add state
state_county<-first_inv %>% 
  select(staterr, rptfips) %>% 
  distinct()

county<-county %>% 
  left_join(state_county)


write_csv(nat, "./data/ncands_first_event_national.csv")
write_csv(state, "./data/ncands_first_event_state.csv")
write_csv(county, "./data/ncands_first_event_county.csv")

