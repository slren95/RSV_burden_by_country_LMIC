# Load packages----
rm(list=ls())
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggsci)
library(metafor)
library(stringr)
library(readxl)
library(mapdata)
library(maps)
library(colorspace)
library(scales)
library(Amelia)
library(epiR)
library(rio)
source("functions.R")

# 0. Values by default----
N.imputation <- 100
N.MC <- 1000
RF_OR <- c(Prematurity = 1.85, 
           LackBreastfeeding = 2.05, 
           LBW = 2.07, 
           Siblings = 1.68, 
           MaternalAge = 1.46, 
           PassiveSmoking = 1.93, 
           MaternalSmoking = 1.33, 
           HIV = 3.74)

RF_OR_list<- list(Prematurity = c(1.85, 1.74,1.97),
           LackBreastfeeding = c(2.05, 1.08,3.03),
           LBW = c(2.07,1.51,2.85), 
           Siblings = c(1.68,1.14,2.49), 
           MaternalAge = c(1.46,1.39,1.53), 
           PassiveSmoking = c(1.93,1.30,2.87), 
           MaternalSmoking = c(1.33,1.22,1.45), 
           HIV = c(3.74,2.65,5.27))

recover_log_params <- function(est, lci, uci) {
  mu <- log(est)
  se <- (log(uci) - log(lci)) / (2 * 1.96)
  return(c('est'=mu,'se'=se))
}

RF_OR_new<-RF_OR_list %>%
  imap_dfr(~{
    res<-recover_log_params(..1[[1]],..1[[2]],..1[[3]])
    data.frame(var=..2,OR=..1[[1]],OR_lci=..1[[2]],OR_uci=..1[[3]],est=res[[1]],se=res[[2]])
  })


# 1. Read data----
file.mainData <- c("Data/Main_v1.61.xlsx")
all.raw <- read_excel(file.mainData, sheet = "1.1 Study descriptions", skip = 2,na = "NA")
r21.raw <- read_excel(file.mainData, sheet = "2.1 Community-based incidence", skip = 2, na = "NA")
r22.raw <- read_excel(file.mainData, sheet = "2.2 Hospital rates", skip = 2, na = "NA")

pop_region.raw <- read_excel("Data/Pop_Mor.xlsx", sheet = "Region")  # per 1000
pop_region.raw_new<-import('../rda/pop_region.raw_new.rds')
pop_region.raw_new<-pop_region.raw_new %>%
  left_join(pop_region.raw,by=c('AGEGR','Income2019'='Group')) %>%
  mutate(diff=Pop-pop)

pop_region.raw<-pop_region.raw_new %>% transmute(Group=Income2019,AGEGR,Pop=pop) # Latest pop data 

# RF.new <- read_excel("Data/RF/df_lmic_imputed.xlsx")  # prevalence of each risk factor
RF.new <- import("../rda/df_lmic_imputed.rds")  # prevalence of each risk factor

#country.income <- read_excel("Data/Country_Income_2019.xlsx", skip = 10)

# 2. Clean and transform data----
all.raw$studyLabel <- substr(all.raw$SID,1,1)
all.raw$studyLabel[all.raw$studyLabel=="U" & as.numeric(substr(all.raw$SID,2,4))<100] <-"T"
all.raw$USID <- substr(all.raw$SID,1,4)

export(all.raw,'rda/all.raw.rds')

# 3. Analysis----
# 3.1 Income-level community incidence rate----
source("code_01_estimate_incidence_rate_using_glmm.R")
# 3.2 Country-specific based on risk factors----
source("code_02_estimate_incidence_by_country_using_risk_factor.R")
# 3.3 Income-level hospitalisation rate----
source("code_03_estimate_hospitalisation_rate_using_glmm.R")

com_rate_average.minimal %>%
  select(SID,Author0,PubYear,Income,AGEGR,ALRI_N,ALRI_Deno,ViralTest,QA_all) %>%
  left_join(all.raw %>% select(SID,PubYear,Title,Country,Location0,ISOCountry), by = c("SID", "PubYear")) %>%
  mutate(AGEGR2=case_when(AGEGR %in% c('0-<6m','6-<12m','12-<60m','0-<60m')~AGEGR,
                          T~NA_character_), .after = AGEGR) %>%
  export('docs/com_rate_average.minimal_validate_copy.xlsx')

hos_rate_average.minimal %>%
  select(SID,Author0,PubYear,Income,AGEGR,HosALRI_N,HosALRI_Deno,ViralTest,QA_all) %>%
  left_join(all.raw %>% select(SID,PubYear,Title,Country,Location0,ISOCountry), by = c("SID", "PubYear")) %>%
  mutate(AGEGR2=case_when(AGEGR %in% c('0-<6m','6-<12m','12-<60m','0-<60m')~AGEGR,
                          T~NA_character_), .after = AGEGR) %>%
  export('docs/hos_rate_average.minimal_validate_copy.xlsx')

save.image('rda/code_master.RData')


com_rate_average.minimal %>%
  select(SID,Author0,PubYear,Income,AGEGR,ALRI_N,ALRI_Deno,ViralTest,QA_all) %>%
  left_join(all.raw %>% select(SID,PubYear,Title,Country,Location0,ISOCountry), by = c("SID", "PubYear")) %>%
  mutate(AGEGR2=case_when(AGEGR %in% c('0-<6m','6-<12m','12-<60m','0-<60m')~AGEGR,
                          T~NA_character_), .after = AGEGR) %>%
  filter(!is.na(AGEGR2)) %>%
  distinct(SID,Country,Location0,ISOCountry) %>% 
  export('docs/com.txt')

hos_rate_average.minimal %>%
  select(SID,Author0,PubYear,Income,AGEGR,HosALRI_N,HosALRI_Deno,ViralTest,QA_all) %>%
  left_join(all.raw %>% select(SID,PubYear,Title,Country,Location0,ISOCountry), by = c("SID", "PubYear")) %>%
  mutate(AGEGR2=case_when(AGEGR %in% c('0-<6m','6-<12m','12-<60m','0-<60m')~AGEGR,
                          T~NA_character_), .after = AGEGR) %>%
  filter(!is.na(AGEGR2)) %>%
  distinct(SID,Country,Location0,ISOCountry) %>% 
  export('docs/hos.txt')

# unpublished ----
com_rate_average.minimal %>%
  distinct(SID,.keep_all = T) %>%
  count(PubYear=='Unpub')

com_rate_average.minimal %>%
  count(PubYear=='Unpub')

hos_rate_average.minimal %>%
  distinct(SID,.keep_all = T) %>%
  count(PubYear=='Unpub')

hos_rate_average.minimal %>%
  count(PubYear=='Unpub')
