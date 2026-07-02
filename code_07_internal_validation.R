rm(list=ls())
library(tidyverse)
library(rio)
library(ggsci)
library(janitor)
library(ggh4x)
library(flextable)
library(officer)

# Incidence ----

RF.res.impute2<-import('rda/RF.res.impute2.rds',trust=T)
RF.res.impute2_sum<-RF.res.impute2 %>%
  reframe(across(c(IR,N),list(est=~quantile(.x,0.5),
                              lci=~quantile(.x,0.025),
                              uci=~quantile(.x,0.975)),.names = "{.col}.{.fn}"),.by = c(ISOCountry,Income2019,AGEGR))

df_valid_com<-import('rda/df_valid_com2.rds',trust=T) %>%
  mutate(CountryName_SID=paste(Country,SID,sep='_'))

df_valid_com2<-df_valid_com %>%
  select(CountryName_SID,SID,AGEGR,Country,ISOCountry,StudyMY,ALRI_N,ALRI_Deno,
         valid_est=est,valid_lci=lci,valid_uci=uci) %>%
  left_join(RF.res.impute2_sum %>% select(ISOCountry,Income2019,AGEGR,starts_with('IR.')) %>%
              set_names(~str_replace(.x,'IR.','model_'))) %>%
  mutate(overlap = pmax(valid_lci, model_lci, na.rm = TRUE) <= pmin(valid_uci, model_uci, na.rm = TRUE)) %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','12-<60m','0-<60m')))

df_valid_com2 %>% count(CountryName_SID,AGEGR)

df_valid_com2_long<-df_valid_com2 %>%
  pivot_longer(
    cols = matches('(model|valid)_'),
    names_to = c("source", ".value"),
    names_pattern = "(.*)_(.*)"
  )

df_valid_com2 %>%
  tabyl(AGEGR, overlap) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") 

# AGEGR     FALSE       TRUE
# 0-<60m 1 (12.5%)  7 (87.5%)
# 0-<6m 3 (16.7%) 15 (83.3%)
# 12-<60m 1 (16.7%)  5 (83.3%)
# 6-<12m 4 (21.1%) 15 (78.9%)
# Total 9 (17.6%) 42 (82.4%)

## plot ----
df_valid_com2_long %>% 
  ggplot(aes(CountryName_SID,y = est,colour = source)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "y")+
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported\n(internal validation)')) +
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping")) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"))+
  labs(x = "Study", y = "RSV-associated ALRI incidence rate\n(/1,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_internal_inc.tiff', width = 12,height = 6,dpi = 300)

## Bland ----

df_ba_inc <- df_valid_com2 %>%
  filter(!is.na(valid_est), !is.na(model_est),
         valid_est > 0, model_est > 0) %>%
  mutate(
    diff_log = log(model_est) - log(valid_est),
    mean_log = (log(model_est) + log(valid_est)) / 2,
    ratio    = exp(diff_log)
  )

ba_stats_hos_inc <- df_ba_inc %>%
  summarise(
    mean_diff = mean(diff_log),
    sd_diff   = sd(diff_log),
    loa_low   = mean_diff - 1.96 * sd_diff,
    loa_high  = mean_diff + 1.96 * sd_diff
  )

ba_stats_hos_inc

ggplot(df_ba_inc, aes(x = mean_log, y = diff_log)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = ba_stats_hos_inc$mean_diff, linetype = "solid") +
  geom_hline(yintercept = c(ba_stats_hos_inc$loa_low, ba_stats_hos_inc$loa_high),
             linetype = "dashed") +
  labs(
    x = "Average of log-transformed incidence rate",
    y = "Difference in log-transformed incidence rate \n (Model estimated − Study reported)"
  ) +
  theme_minimal()+
  theme(plot.background = element_rect(colou='white'))

ggsave('plot/Bland_inc.png',dpi = 200,width = 2000,height = 1200,units = 'px')

ggplot(df_ba_inc, aes(diff_log)) +
  #geom_histogram(bins = 30) +
  geom_density()+
  geom_vline(xintercept = 0, linetype = 2)

quantile(df_ba_inc$diff_log, c(0.05, 0.25, 0.5, 0.75, 0.95))

# Hospitalisation ----

df_hos_by_country2_1000<-rio::import("rda/df_hos_by_country2_1000.rds",trust=T)

df_wide<-df_hos_by_country2_1000 %>%
  select(ISOCountry,Income2019,starts_with('Rate')) %>%
  pivot_longer(cols = starts_with('Rate'),
               names_pattern = "^Rate_(.*)_(.*)",
               names_to = c("AGEGR",'.value')) %>%
  mutate(HR.est=case_when(AGEGR=='12-<60m'~q500pos,T~q500),
         HR.lci=case_when(AGEGR=='12-<60m'~q025pos,T~q025),
         HR.uci=case_when(AGEGR=='12-<60m'~q975pos,T~q975))


df_valid_hos<-import('rda/df_valid_hos2.rds',trust=T) %>% # from Community_rate_code/code_04
  mutate(CountryName_SID=paste(Country,SID,sep='_')) %>%
  filter(ISOCountry!='USA')

df_valid_hos2<-df_valid_hos %>%
  select(CountryName_SID,SID,AGEGR,Country,ISOCountry,StudyMY,HosALRI_N,HosALRI_Deno,
         valid_est=est,valid_lci=lci,valid_uci=uci) %>%
  left_join(df_wide %>% select(ISOCountry,Income2019,AGEGR,starts_with('HR.')) %>%
              set_names(~str_replace(.x,'HR.','model_'))) %>%
  mutate(overlap = pmax(valid_lci, model_lci, na.rm = TRUE) <= pmin(valid_uci, model_uci, na.rm = TRUE)) %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','12-<60m','0-<60m')))

df_valid_hos2 %>% count(CountryName_SID,AGEGR)

df_valid_hos2_long<-df_valid_hos2 %>%
  pivot_longer(
    cols = matches('(model|valid)_'),
    names_to = c("source", ".value"),
    names_pattern = "(.*)_(.*)"
  )

df_valid_hos2 %>%
  tabyl(AGEGR, overlap) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")

# AGEGR      FALSE       TRUE
# 0-<6m  1  (7.1%) 13 (92.9%)
# 6-<12m  1  (7.1%) 13 (92.9%)
# 12-<60m  1  (7.7%) 12 (92.3%)
# 0-<60m  7 (41.2%) 10 (58.8%)
# Total 10 (17.2%) 48 (82.8%)

## plot ----

df_valid_hos2_long %>% 
  ggplot(aes(CountryName_SID,y = est,colour = source)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "y")+
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported\n(internal validation)')) +
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping")) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"))+
  labs(x = "Study", y = "RSV-associated ALRI hospital admission rate\n(/1,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_internal_hos.tiff', width = 12,height = 6,dpi = 300)


## Bland ----

df_ba_hos <- df_valid_hos2 %>%
  filter(
    valid_est > 0,
    model_est > 0
  ) %>%
  mutate(
    mean_log = (log(valid_est) + log(model_est)) / 2,
    diff_log = log(model_est) - log(valid_est)
  )

ba_stats_hos <- df_ba_hos %>%
  summarise(
    mean_diff = mean(diff_log),
    sd_diff   = sd(diff_log),
    loa_low   = mean_diff - 1.96 * sd_diff,
    loa_high  = mean_diff + 1.96 * sd_diff
  )

ba_stats_hos

ggplot(df_ba_hos, aes(x = mean_log, y = diff_log)) +
  geom_point(alpha = 0.7) +
  geom_hline(
    yintercept = ba_stats_hos$mean_diff,
    linetype = "solid"
  ) +
  geom_hline(
    yintercept = c(ba_stats_hos$loa_low, ba_stats_hos$loa_high),
    linetype = "dashed"
  ) +
  labs(
    x = "Average of log-transformed admission rate",
    y = "Difference in log-transformed admission rate \n (Model estimated − Study reported)"
  ) +
  theme_minimal()+
  theme(plot.background = element_rect(colou='white'))

ggsave('plot/Bland_hos.png',dpi = 200,width = 2000,height = 1200,units = 'px')

ggplot(df_ba_hos, aes(diff_log)) +
  #geom_histogram(bins = 30) +
  geom_density()+
  geom_vline(xintercept = 0, linetype = 2)

quantile(df_ba_hos$diff_log, c(0.05, 0.25, 0.5, 0.75, 0.95))


# Table ----

tab <- bind_rows(
  tibble(AGEGR = "RSV-associated ALRI incidence rate", Yes = "", No = ""),
  df_valid_com2 %>%
    tabyl(AGEGR, overlap) %>%
    adorn_totals("row", name = "overall") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(1) %>%
    adorn_ns("front") %>%
    rename(Yes = `TRUE`, No = `FALSE`) %>%
    mutate(AGEGR=paste('  ',AGEGR)),
  
  tibble(AGEGR = "RSV-associated ALRI hospital admission rate", Yes = "", No = ""),
  df_valid_hos2 %>%
    tabyl(AGEGR, overlap) %>%
    adorn_totals("row", name = "overall") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(1) %>%
    adorn_ns("front") %>%
    rename(Yes = `TRUE`, No = `FALSE`) %>%
    mutate(AGEGR=paste('  ',AGEGR))
) %>%
  rename(Age=AGEGR)

flextable(tab) %>%
  add_header_row(
    values = c("Age", "Uncertainty interval overlap"),
    colwidths = c(1, 2)
  ) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header", j = 1) %>%
  merge_at(i = 1, j = 1:3,part = "body") %>%
  merge_at(i = 7, j = 1:3,part = "body") %>%
  bold(i = c(1,7), part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:3, align = "right", part = "body") %>%
  align(align = "center", part = "header") %>%
  border_remove() %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx(path = "docs/Valid_internal_overlap.docx")

# Study ----

df_valid_com %>%
  rename(valid_est=est,valid_lci=lci,valid_uci=uci) %>%
  left_join(RF.res.impute2_sum %>% select(ISOCountry,Income2019,AGEGR,starts_with('IR.')) %>%
              set_names(~str_replace(.x,'IR.','model_')),by = join_by(AGEGR, ISOCountry)) %>%
  mutate(overlap = pmax(valid_lci, model_lci, na.rm = TRUE) <= pmin(valid_uci, model_uci, na.rm = TRUE)) %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','12-<60m','0-<60m'))) %>% 
  transmute(SID,References=sprintf('%s et al. %s',Author0.x,PubYear),Country,Location=Location0.x,`Study Period`=StudyPeriod,`Income level`=Income,Age=AGEGR,
            valid=sprintf('%.1f\n(%.1f-%.1f)',valid_est,valid_lci,valid_uci),
            model=sprintf('%.1f\n(%.1f-%.1f)',model_est,model_lci,model_uci),
            `Uncertainty interval overlap`=ifelse(overlap,'Yes','No')) %>%
  rio::export('docs/Study_internal_inc.xlsx')


df_valid_hos %>%
  rename(valid_est=est,valid_lci=lci,valid_uci=uci) %>%
  left_join(df_wide %>% select(ISOCountry,Income2019,AGEGR,starts_with('HR.')) %>%
              set_names(~str_replace(.x,'HR.','model_')),by = join_by(AGEGR, ISOCountry)) %>%
  mutate(overlap = pmax(valid_lci, model_lci, na.rm = TRUE) <= pmin(valid_uci, model_uci, na.rm = TRUE)) %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','12-<60m','0-<60m'))) %>%
  transmute(SID,References=sprintf('%s et al. %s',Author0.x,PubYear),Country,Location=Location0.x,`Study Period`=StudyPeriod,`Income level`=Income,Age=AGEGR,
            valid=sprintf('%.1f\n(%.1f-%.1f)',valid_est,valid_lci,valid_uci),
            model=sprintf('%.1f\n(%.1f-%.1f)',model_est,model_lci,model_uci),
            `Uncertainty interval overlap`=ifelse(overlap,'Yes','No')) %>%
  rio::export('docs/Study_internal_hos.xlsx')

df_valid_com2 %>% distinct(SID)
df_valid_hos2 %>% distinct(SID)
