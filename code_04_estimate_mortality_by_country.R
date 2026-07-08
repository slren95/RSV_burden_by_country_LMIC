rm(list=ls())
library(rio)
library(tidyverse)

## All Results for Community Mortality Rate ----
load("workspaceToBegin.RData")
rm(list = setdiff(ls(), c("mor_all_DeCoDe.predict","mor_all_NP.predict",
                          "mor_ALRI_DeCoDe.predict","mor_ALRI_NP.predict","pop_mor.raw")))


mor_all_DeCoDe.predict2<-import('mor_all_DeCoDe.predict.rds',trust=T)
df_lmic_imputed<-import('rda/df_lmic_imputed.rds',trust=T)
all.equal(mor_all_DeCoDe.predict2,mor_all_DeCoDe.predict)

mor_all_DeCoDe.predict %>% filter(index==1) %>% count(Income2019)

country_mort<-mor_all_DeCoDe.predict %>% filter(Income2019!='H',index==1) %>% select(ISOCountry,Country.Name,Income2019,Pop) # 133

setdiff(df_lmic_imputed$ISOCountry,country_mort$ISOCountry)
setdiff(country_mort$ISOCountry,df_lmic_imputed$ISOCountry)

df_lmic_imputed %>% filter(ISOCountry %in% c('GTM','ASM','XKX')) 

mor_all_DeCoDe.predict %>% filter(index==1) %>% filter(ISOCountry %in% c('GTM','ASM','XKX','NIU')) 

# Fix GTM Guatemala as UM country 

mor_all_DeCoDe.predict.new <- mor_all_DeCoDe.predict %>%
  mutate(Income2019 = if_else(ISOCountry == 'GTM', 'UM', Income2019))

mor_ALRI_DeCoDe.predict.new <- mor_ALRI_DeCoDe.predict %>%
  mutate(Income2019 = if_else(ISOCountry == 'GTM', 'UM', Income2019))



mor_all_DeCoDe.predict.new %>% filter(index==1) %>% count(Income2019)

df_lmic_imputed %>% filter(ISOCountry %in% c('ASM','XKX')) 

# Population Check ----
mor_all_DeCoDe.predict.new %>% filter(index==1) %>% 
  summarise(across(Y0:Y4,~sum(.x,na.rm=T)),.by=Income2019) %>%
  mutate(pop_0006=Y0/2,pop_0612=Y0/2,pop_1260=Y1+Y2+Y3+Y4,pop_U5=Y0+Y1+Y2+Y3+Y4) %>%
  filter(Income2019!='H')

df_lmic_imputed %>%
  ungroup() %>%
  summarise(across(matches('^(Y|pop_)'),sum),.by=Income2019) %>%
  mutate(pop_U5=Y0+Y1+Y2+Y3+Y4)

# Global mean ----
df_global_pop<-pop_mor.raw %>%
  summarise(across(starts_with('Y'),~sum(.x,na.rm=T))) %>%
  transmute(pop_0006=Y0/2,pop_0612=Y0/2,pop_1260=Y1+Y2+Y3+Y4,
            pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612)
  
df_global_mort_att<-mor_all_DeCoDe.predict.new %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm=T)),.by=index) %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    )
  ) %>%
  cross_join(df_global_pop) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

df_global_mort_ass<-mor_all_NP.predict %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm=T)),.by=index) %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    )
  ) %>%
  cross_join(df_global_pop) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

df_global_mean_mort<-bind_rows(list(mort_att=df_global_mort_att,mort_ass=df_global_mort_ass),.id = 'metric') %>%
  select(-starts_with('pop')) %>%
  pivot_longer(cols=-metric,names_pattern = "(.*)_(N|R)_(.*)",names_to = c("AGEGR",'type','.value')) %>%
  mutate(AGEGR = recode(AGEGR,
                        'm0006' = '0-<6m',
                        'm0612' = '6-<12m',
                        'm1260' = '12-<60m',
                        'm0060' = '0-<60m',
                        'm0012' = '0-<12m'
  )) %>%
  rename(est = `500`, lci = `025`, uci = `975`) %>%
  pivot_wider(
    id_cols = c(AGEGR),      # 行标识列
    names_from = c(metric,type),              # 将 AGEGR 变成列名
    values_from = c(est, lci, uci), # 要展开的值列
    names_glue = "{metric}_{type}_{.value}"
  )

rio::export(df_global_mean_mort,'rda/df_global_mean_mort.rds')

# 1️⃣133 LMIC country ----
df_mort_all_DeCoDe.lmic<-mor_all_DeCoDe.predict.new %>%
  filter(ISOCountry %in% df_lmic_imputed$ISOCountry)

df_mort_all_NP.lmic<-mor_all_NP.predict %>%
  mutate(Income2019 = if_else(ISOCountry == 'GTM', 'UM', Income2019)) %>%
  filter(ISOCountry %in% df_lmic_imputed$ISOCountry)

rio::export(df_mort_all_DeCoDe.lmic,'rda/df_mort_all_DeCoDe.lmic.rds')
rio::export(df_mort_all_NP.lmic,'rda/df_mort_all_NP.lmic.rds')

# Population LMIC ----
pop_region.raw_new<-import('rda/pop_region.raw_new.rds',trust=T)

pop_region.byincome<-bind_rows(
  pop_region.raw_new,
  pop_region.raw_new %>% mutate(Income2019='Global') %>%
    summarise(pop=sum(pop),.by = c(Income2019,Group,AGEGR))
) %>%
  pivot_wider(id_cols = Income2019,names_from = Group,values_from = pop) %>%
  rename(pop_0060=pop_U5)

# 2️⃣Summarise and calculate mortality ----

df_sum_all_DeCoDe<-df_mort_all_DeCoDe.lmic %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N,
            ISOCountry,Income2019) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(ISOCountry, Income2019)
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,
         m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

df_sum_all_NP<-df_mort_all_NP.lmic %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N,
            ISOCountry,Income2019) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(ISOCountry, Income2019)
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,
         m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
         ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

rio::export(df_sum_all_DeCoDe,'rda/df_sum_all_DeCoDe.rds')
rio::export(df_sum_all_NP,'rda/df_sum_all_NP.rds')

# 3️⃣By Income level ----
df_sum_all_DeCoDe.byincome<-bind_rows(
  df_mort_all_DeCoDe.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(Income2019,index)),
  df_mort_all_DeCoDe.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(index)) %>%
    mutate(Income2019='Global')
) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric) & !all_of("index"),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(Income2019)
  ) %>%
  left_join(pop_region.byincome,by = 'Income2019') %>%
  mutate(pop_0012=pop_0006+pop_0612) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  ) %>%
  mutate(Income2019=recode(Income2019,'Global'='LMIC'))

df_sum_all_NP.byincome<-bind_rows(
  df_mort_all_NP.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(Income2019,index)),
  df_mort_all_NP.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(index)) %>%
    mutate(Income2019='Global')
) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric) & !all_of("index"),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(Income2019)
  ) %>%
  left_join(pop_region.byincome,by = 'Income2019') %>%
  mutate(pop_0012=pop_0006+pop_0612) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  ) %>%
  mutate(Income2019=recode(Income2019,'Global'='LMIC'))

rio::export(df_sum_all_DeCoDe.byincome,'rda/df_sum_all_DeCoDe.byincome.rds')
rio::export(df_sum_all_NP.byincome,'rda/df_sum_all_NP.byincome.rds')
