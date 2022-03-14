# generate table of moth counts (total over years) by species and functional group
library(readxl)
library(dplyr)
library(tidyr)
mothdata <- read_csv("/data/ecn/ECN_IM1.csv") %>% 
  mutate(TRAP = paste0(SITECODE,'_',LCODE), DATE=as.Date(SDATE,format= "%d-%b-%y")) %>% 
  select(DATE,TRAP,FIELDNAME,VALUE)

moth_groups <-  read_csv("/data/ecn/moth_groups.csv")


#read_excel('/data/ecn/MOTH_TRAIT_CODES_MASTER_4_Peter.xlsx') # metadata
moth_traits = read_excel('/data/ecn/MOTH_TRAIT_CODES_MASTER_4_Peter.xlsx', sheet='MOTH_TRAIT_CODES_MASTER') %>%
  mutate(IM_CODE = substr(IM_CODE,4,nchar(IM_CODE)))


# mothdata group by trap and IM_CODE

mothdata %>% group_by(TRAP,FIELDNAME) %>% summarise(count=n()) %>% 
  tidyr::spread(TRAP,count) %>% 
  left_join(moth_traits,by=c("FIELDNAME"="IM_CODE")) %>% 
  select(Genus,species,`COMMON NAME`,Family) %>% 
  write_csv('moth_days_by_species_ECN.csv')
