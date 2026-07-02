rm(list=ls())
library(rio)
library(tidyverse)

# Model estimate ----
df_sum_all_DeCoDe<-import('rda/df_sum_all_DeCoDe.rds',trust=T)
df_sum_all_NP<-import('rda/df_sum_all_NP.rds',trust=T)
RF.res.impute2<-import('rda/RF.res.impute2.rds',trust=T)
df_hos_by_country2_1000<-import("rda/df_hos_by_country2_1000.rds",trust=T)

df_lmic_imputed<-import('rda/df_lmic_imputed.rds',trust=T) %>%
  mutate(region = recode(CountryName,
                         "Korea, Dem. Rep." = "North Korea",
                         "Syrian Arab Republic" = "Syria",
                         "Yemen, Rep." = "Yemen",
                         "Gambia, The" = "Gambia",
                         "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                         "São Tomé and Príncipe" = "Sao Tome and Principe",
                         "Viet Nam" = "Vietnam",
                         "Micronesia, Fed. Sts." = "Micronesia",
                         "Egypt, Arab Rep." = "Egypt",
                         "Türkiye" = "Turkey",
                         "Russian Federation" = "Russia",
                         "St. Vincent and the Grenadines" = "Saint Vincent"
  ))

df_model_inc<-RF.res.impute2 %>%
  reframe(across(c(IR),list(q500=~quantile(.x,0.5),
                            q025=~quantile(.x,0.025),
                            q975=~quantile(.x,0.975))),.by = c(ISOCountry,Income2019,AGEGR)) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region)) %>%
  set_names(~ gsub("^IR_q", "inc_", .))

df_model_mort.att<-df_sum_all_DeCoDe %>%
  select(ISOCountry,Income2019,CountryName,contains('_R_')) %>%
  pivot_longer(
    cols = contains('_R_'),
    names_to = c("AGEGR", "stat"),
    names_pattern = "m(\\d+)_R_(025|500|975)",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = c(ISOCountry, Income2019, CountryName, AGEGR),
    names_from = stat,
    values_from = value,
    names_prefix = "mort.att_"
  ) %>%
  mutate(AGEGR=recode(AGEGR,"0006" = "0-<6m","0612" = "6-<12m","1260" = "12-<60m","0060"="0-<60m","0012"="0-<12m")) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region,CountryName)) %>%
  mutate(across(starts_with('mort.att'),~.x*100))

df_model_mort.ass<-df_sum_all_NP %>%
  select(ISOCountry,Income2019,CountryName,contains('_R_')) %>%
  pivot_longer(
    cols = contains('_R_'),
    names_to = c("AGEGR", "stat"),
    names_pattern = "m(\\d+)_R_(025|500|975)",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = c(ISOCountry, Income2019, CountryName, AGEGR),
    names_from = stat,
    values_from = value,
    names_prefix = "mort.ass_"
  ) %>%
  mutate(AGEGR=recode(AGEGR,"0006" = "0-<6m","0612" = "6-<12m","1260" = "12-<60m","0060"="0-<60m","0012"="0-<12m")) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region,CountryName)) %>%
  mutate(across(starts_with('mort.ass'),~.x*100))

df_model_hos<-df_hos_by_country2_1000 %>%
  select(ISOCountry,Income2019,contains('Rate_'),-c(`Rate_12-<60m_q500`,`Hos_12-<60m_q500`)) %>% 
  pivot_longer(
    cols = contains("Rate_"),
    names_to = c("AGEGR", "stat"),# 两个捕获组 → 两列
    names_pattern = "Rate_(.*)_q(.*)",    # 注意 [NR]，匹配 N 或 R
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = c(ISOCountry, Income2019, AGEGR),
    names_from = stat,
    values_from = value,
    names_prefix = "hos_"
  ) %>%
  mutate(hos_500=ifelse(AGEGR=='12-<60m',hos_500pos,hos_500),
         hos_025=ifelse(AGEGR=='12-<60m',hos_025pos,hos_025),
         hos_975=ifelse(AGEGR=='12-<60m',hos_975pos,hos_975)) %>%
  select(-ends_with('pos')) %>%
  mutate(AGEGR=recode(AGEGR,"0006" = "0-<6m","0612" = "6-<12m","1260" = "12-<60m","0060"="0-<60m","0012"="0-<12m")) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region,CountryName))

## export ----
rio::export(df_model_inc,'rda/df_model_inc.rds')
rio::export(df_model_hos,'rda/df_model_hos.rds')
rio::export(df_model_mort.att,'rda/df_model_mort.att.rds')
rio::export(df_model_mort.ass,'rda/df_model_mort.ass.rds')