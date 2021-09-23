rm(list = ls()); gc()
library(data.table)
library(tidyverse)
library(mice)

##### 9/23/21 imputations
afcars<-mice::complete(
  readRDS("~/Projects/ndacan_processing/imputations/afcars_imp.rds"),
  action = "long")

### weirdly still a few NAs, about 10-30 per year per imputation
### that's negligible, but troubleshoot next time I run imps

afcars<-afcars %>% 
  filter(!(is.na(race_ethn)))

#### use entered + totalrem for first removals
### check n across imps
# 
# afcars %>% 
#   filter(totalrem==1, entered==1) %>% 
#   group_by(.imp) %>% 
#   summarise(first_entry = n())
### really small variance 
### min  = 3481425, max = 3481479 - looks fine to me


afcars_rem<-afcars %>% 
  filter(totalrem==1, entered==1) %>% 
  group_by(.imp, state, year, age, race_ethn) %>% 
  summarise(first_entry = n())

### this may have a slight upward bias
### because stfcid isn't valid for 04 - 07
### given age at TPR likely small though for 15-19 group

afcars_tpr<-afcars %>% 
  filter(istpr==1) %>% 
  filter(!(duplicated(stfcid))) %>% 
  group_by(.imp, state, year, age, race_ethn) %>% 
  summarise(tpr = n()) %>% 
  ungroup()

### join, then fill zeroes
afcars_out<-afcars_rem %>% 
  full_join(afcars_tpr)

### complete the zeroes for both outcomes
temp<-expand_grid(.imp = unique(afcars_out$.imp),
                  state = unique(afcars_out$state),
                  year = unique(afcars_out$year),
                  age = unique(afcars_out$age),
                  race_ethn = unique(afcars_out$race_ethn)) 

out<-left_join(temp,
                afcars_out) %>% 
  mutate(tpr = ifelse(is.na(tpr), 0, tpr),
         first_entry = ifelse(is.na(first_entry), 0, first_entry))

### merge into one file
write_csv(out, "./data/afcars_first_event_state.csv")