rm(list=ls())
library(rio)
library(tidyverse)
library(officer)
source('functions.R')

df_lmic_imputed<-import('rda/df_lmic_imputed.rds',trust=T)

# Mortality  ----
df_sum_all_DeCoDe<-import('rda/df_sum_all_DeCoDe.rds',trust=T)
df_sum_all_NP<-import('rda/df_sum_all_NP.rds',trust=T)

df_sum_all_DeCoDe.byincome<-import('rda/df_sum_all_DeCoDe.byincome.rds',trust=T)
df_sum_all_NP.byincome<-import('rda/df_sum_all_NP.byincome.rds',trust=T)

c('df_sum_all_DeCoDe','df_sum_all_NP') %>%
  walk(~{
    filename<-sprintf('docs/Table_mortality_%s.docx',str_sub(.x,8))
    df_sum_all_DeCoDe<-get(.x)
    df_sum_all_DeCoDe %>%
      arrange(Income2019,CountryName) %>%
      mutate(across(contains('_N_'),
                    function(x) {
                      case_when(
                        x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                        x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                        TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                      )
                    }),
             across(contains('_R_'),~round(.x*100,1))) %>%
      transmute(Income2019,CountryName,ISOCountry,
                N_0006 = sprintf('%s (%s–%s)', m0006_N_500, m0006_N_025, m0006_N_975),
                N_0612 = sprintf('%s (%s–%s)', m0612_N_500, m0612_N_025, m0612_N_975),
                N_1260 = sprintf('%s (%s–%s)', m1260_N_500, m1260_N_025, m1260_N_975),
                N_0060 = sprintf('%s (%s–%s)', m0060_N_500, m0060_N_025, m0060_N_975),
                N_0012 = sprintf('%s (%s–%s)', m0012_N_500, m0012_N_025, m0012_N_975),
                R_0006 = sprintf('%.1f (%.1f–%.1f)', m0006_R_500, m0006_R_025, m0006_R_975),
                R_0612 = sprintf('%.1f (%.1f–%.1f)', m0612_R_500, m0612_R_025, m0612_R_975),
                R_1260 = sprintf('%.1f (%.1f–%.1f)', m1260_R_500, m1260_R_025, m1260_R_975),
                R_0060 = sprintf('%.1f (%.1f–%.1f)', m0060_R_500, m0060_R_025, m0060_R_975),
                R_0012 = sprintf('%.1f (%.1f–%.1f)', m0012_R_500, m0012_R_025, m0012_R_975)) %>%
      pivot_longer(cols=matches('^[RN]_'),names_to = c("name", ".value"),
                   names_pattern = "([NR])_(.*)") %>%
      arrange(Income2019,CountryName,desc(name)) %>%
      rename(`0-<6m`=`0006`,`6-<12m`=`0612`,`12-<60m`=`1260`,`0-<60m`=`0060`,`0-<12m`=`0012`,Country=CountryName) %>%
      mutate(name=recode(name,'N'='Number of deaths','R'='Mortality rate')) %>%
      left_join(df_lmic_imputed %>% select(ISOCountry,CountryName)) %>%
      relocate(Income2019,Country,name,`0-<6m`,`6-<12m`,`0-<12m`,`12-<60m`,`0-<60m`) %>%
      split(.,.$Income2019) %>%
      imap_dfr(~{
        bind_rows(data.frame(Country=switch(.y,'L'='Lower-income','LM'='Lower-middle-income','UM'='Upper-middle-income')),
                  .x)
      }) %>%
      mutate(Country=ifelse(name!='Number of deaths' | is.na(name),paste0('',Country),'')) %>%
      select(Country,name,contains('-')) %>% 
      rename_with(~" ",.cols='name') %T>%  
      saveRDS(str_replace(filename,'\\.docx','\\.rds')) %T>%  
      rio::export(str_replace(filename,'\\.docx','\\.xlsx')) %>%
      export_flextable_word(filename,orientation='land')
  })

## by income ----

c('df_sum_all_DeCoDe.byincome','df_sum_all_NP.byincome') %>%
  walk(~{
    filename<-sprintf('docs/Table_mortality_%s.docx',str_sub(.x,8))
    df_sum_all_DeCoDe<-get(.x)
    df_sum_all_DeCoDe %>%
      mutate(across(contains('_N_'),
                    function(x) {
                      case_when(
                        x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                        x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                        TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                      )
                    }),
             across(contains('_R_'),~round(.x*100,2))) %>%
      transmute(`Income Level`=paste0(Income2019,if_else(Income2019=='LMIC','','IC')),
                N_0006 = sprintf('%s (%s–%s)', m0006_N_500, m0006_N_025, m0006_N_975),
                N_0612 = sprintf('%s (%s–%s)', m0612_N_500, m0612_N_025, m0612_N_975),
                N_0012 = sprintf('%s (%s–%s)', m0012_N_500, m0012_N_025, m0012_N_975),
                N_1260 = sprintf('%s (%s–%s)', m1260_N_500, m1260_N_025, m1260_N_975),
                N_0060 = sprintf('%s (%s–%s)', m0060_N_500, m0060_N_025, m0060_N_975),
                R_0006 = sprintf('%.1f (%.1f–%.1f)', m0006_R_500, m0006_R_025, m0006_R_975),
                R_0612 = sprintf('%.1f (%.1f–%.1f)', m0612_R_500, m0612_R_025, m0612_R_975),
                R_0012 = sprintf('%.1f (%.1f–%.1f)', m0012_R_500, m0012_R_025, m0012_R_975),
                R_1260 = sprintf('%.1f (%.1f–%.1f)', m1260_R_500, m1260_R_025, m1260_R_975),
                R_0060 = sprintf('%.1f (%.1f–%.1f)', m0060_R_500, m0060_R_025, m0060_R_975)) %>%
      mutate(
        `0-<6m`  = paste(N_0006, R_0006, sep = "\n"),
        `6-<12m`  = paste(N_0612, R_0612, sep = "\n"),
        `0-<12m`  = paste(N_0012, R_0012, sep = "\n"),
        `12-<60m` = paste(N_1260, R_1260, sep = "\n"),
        `0-<60m`  = paste(N_0060, R_0060, sep = "\n")
      ) %>%
      select(`Income Level`, `0-<6m`, `6-<12m`,`0-<12m`,`12-<60m`, `0-<60m`) %T>%
      rio::export(str_replace(filename,'\\.docx','\\.xlsx')) %T>%
      rio::export(str_replace(filename,'\\.docx','\\.rds')) %>%
      export_flextable_word(filename,orientation='land')
  })

# Incidence  ----
df_base<-data.frame(Country=c('Low-income','Lower-middle-income','Upper-middle-income'))
RF.res.impute<-import('rda/RF.res.impute.rds',trust=T)
RF.res.impute2<-import('rda/RF.res.impute2.rds',trust=T)

## Summarise ----

RF.res.impute2 %>%
  reframe(across(c(IR,N),list(q500=~quantile(.x,0.5),
                              q025=~quantile(.x,0.025),
                              q975=~quantile(.x,0.975))),.by = c(ISOCountry,Income2019,AGEGR)) %>%
  mutate(across(starts_with('IR_'),~round(.x,1)),
         across(starts_with('N_'),
                function(x) {
                  case_when(
                    x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                    x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                    TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                  )
                })) %>%
  mutate(N=sprintf('%s (%s–%s)', N_q500, N_q025, N_q975),
         R=sprintf('%.1f (%.1f–%.1f)',IR_q500, IR_q025, IR_q975),
         str=paste0(N,'\n',R)) %>%
  pivot_longer(cols = c(R,N)) %>%
  mutate(name=recode(name,'N'='Number of episodes','R'='Incidence rate')) %>%
  pivot_wider(id_cols = c(Income2019,ISOCountry,name),names_from = AGEGR,values_from = value) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName)) %>%
  arrange(Income2019,CountryName) %>%
  relocate(Income2019,Country=CountryName,name,`0-<6m`,`6-<12m`,`0-<12m`,`12-<60m`,`0-<60m`) %>%
  split(.,.$Income2019) %>%
  imap_dfr(~{
    bind_rows(data.frame(Country=switch(.y,'L'='Lower-income','LM'='Lower-middle-income','UM'='Upper-middle-income')),
              .x)
  }) %>%
  mutate(Country=ifelse(name!='Number of episodes' | is.na(name),Country,'')) %>%
  select(Country,name,contains('-')) %>% 
  rename_with(~" ",.cols='name') %T>%  
  saveRDS('docs/Table_incidence.rds') %T>%  
  rio::export('docs/Table_incidence.xlsx') %>%
  export_flextable_word('docs/Table_incidence.docx',orientation='land')

## by income ----

bind_rows(
  RF.res.impute2 %>%
    select(ISOCountry,Income2019,AGEGR,pop,N,index) %>%
    summarise(across(where(is.numeric),sum),.by=c(Income2019,AGEGR,index)),
  RF.res.impute2 %>%
    select(ISOCountry,Income2019,AGEGR,pop,N,index) %>%
    summarise(across(where(is.numeric),sum),.by=c(AGEGR,index)) %>% mutate(Income2019='LMIC')
) %>%
  mutate(IR=N/pop) %>%
  reframe(across(c(IR,N),list(est=~quantile(.x,0.5),
                              lci=~quantile(.x,0.025),
                              uci=~quantile(.x,0.975)),.names = "{.col}.{.fn}"),.by = c(Income2019,AGEGR)) %>%
  mutate(across(starts_with('IR.'),~round(.x,2)),
         across(starts_with('N.'),
                function(x) {
                  case_when(
                    x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                    x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                    TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                  )
                })) %>%
  mutate(N=sprintf('%s (%s–%s)', N.est, N.lci, N.uci),
         R=sprintf('%.1f (%.1f–%.1f)',IR.est, IR.lci, IR.uci),
         str=paste0(N,'\n',R)) %>%
  pivot_wider(id_cols = c(Income2019),names_from = AGEGR,values_from = str) %>%
  transmute(`Income Level`=paste0(Income2019,if_else(Income2019=='LMIC','','IC')),`0-<6m`, `6-<12m`,`0-<12m`,`12-<60m`, `0-<60m`) %T>%
  rio::export('docs/Table_incidence.byincome.xlsx') %T>%
  rio::export('docs/Table_incidence.byincome.rds') %>%
  export_flextable_word('docs/Table_incidence.byincome.docx',orientation='land')

# China ----
Table_mortality_all_NP <- readRDS("D:/NJMU/RSV by country/RSVbyCountry/docs/Table_mortality_all_NP.rds")
Table_mortality_all_DeCoDe <- readRDS("D:/NJMU/RSV by country/RSVbyCountry/docs/Table_mortality_all_DeCoDe.rds")
Table_incidence <- readRDS("D:/NJMU/RSV by country/RSVbyCountry/docs/Table_incidence.rds")
Table_Hospitalisation_1000 <- readRDS("D:/NJMU/RSV by country/RSVbyCountry/docs/Table_Hospitalisation_1000.rds")

bind_rows(Table_incidence[184:185,] %>% bind_cols(Metric='RSV-associated ALRI incidence',.),
          Table_Hospitalisation_1000[184:185,] %>% bind_cols(Metric='RSV-associated ALRI hospital admission',.),
          Table_mortality_all_NP[182:183,] %>% bind_cols(Metric='RSV-associated all cause deaths',.),
          Table_mortality_all_DeCoDe[182:183,] %>% bind_cols(Metric='RSV-attributable all cause deaths',.)) %>%
  rio::export('docs/China.xlsx')
