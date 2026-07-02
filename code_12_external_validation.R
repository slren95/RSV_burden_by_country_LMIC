rm(list=ls())
library(tidyverse)
library(rio)
library(fs)
library(janitor)
library(ggsci)
library(epiR)
library(tidytext)
library(patchwork)
library(ggh4x)
library(openxlsx)
library(writexl)
library(flextable)
library(officer)

# Risk and rates of hospitalisation in young  children: A prospective study of a South  African birth cohort
round(c(276,242,314)*38/(93+27+24+31+60),1)

round(c(406,348,471)*30/(62+13+14+26+60),1)

round(c(142,107,183)*8/(31+14+10+5),1)

epi.conf(cbind(11, 40427), ctype = "inc.rate", method = "exact")*1000*100/5

#E154
epi.conf(matrix(c(245,34029,170,6759),nrow = 2,byrow = T), ctype = "inc.rate", method = "exact")*1000


# Clean validation data ----

df_geo_screen<-import('validation/zotero_export.csv') %>%
  mutate(exclude_reason=case_when(
    str_detect(Tags,'Included')~NA_character_,
    str_detect(Tags,'Ineligible study design')~'Ineligible study design',
    str_detect(Tags,'Ineligible study setting')~'Ineligible study setting',
    str_detect(Tags,'Ineligible study period')~' Ineligible study period',
    str_detect(Tags,'Inextractable outcome data')~'Inextractable outcome data',
    str_detect(Tags,'Special population')~'Special population'
  ))

count(df_geo_screen,Tags,exclude_reason)

count(df_geo_screen,exclude_reason)


df_geo_extract<-df_geo_screen %>%
  filter(is.na(exclude_reason)) %>%
  mutate(SID=sprintf('G%02d',row_number()),.before = 1) %>%
  rename(PubYear=Year,Author0=`First Author`)

1:nrow(df_geo_extract) %>%
  walk(~{
    str<-str_sub(df_geo_extract[.x,'Title'],1,20)
    files_name<-dir('validation/Zotero_PDF')
    index<-which(files_name %>% str_detect(str))
    new_name<-sprintf('%s_%s.pdf',df_geo_extract[.x,'SID'],str_sub(files_name[index],1,50))
    print(index)
    file.copy(paste0('validation/Zotero_PDF/',files_name[index]),
              paste0('validation/Final_PDF/',new_name),overwrite=T)
  })

df_geo<-import('validation/Geohealth_RSV_by_country_validatation.xlsx') %>%
  row_to_names(row_number = 2) %>%
  arrange(SID) %>%
  mutate(Database='Geohealth',Included=1) %>%
  select(-c(Author0,PubYear,Journal)) %>%
  left_join(df_geo_extract %>% select(SID,Author0,PubYear,Journal))

df_emb<-import_list('validation/Embase_full-text screening.xlsx')[[3]] %>%
  row_to_names(row_number = 4)

df_E041<-import_list('validation/Embase_full-text screening.xlsx')[[4]] %>%
  select(1:6) %>%
  setNames(c('Year','0-1','1-2','2-3','3-4','4-5'))

median(round(df_E041$`0-1` *1000,1))



df_med<-import_list('validation/Medline+WOS_full-text screening_extraction_SD.xlsx')[[3]] %>%
  select(-1) %>%
  row_to_names(row_number = 4)

df_unp<-import('validation/Unpublished.xlsx') %>%
  row_to_names(row_number = 2) 

df_final<-bind_rows(
  df_geo %>% mutate(Included=as.character(Included),PubYear=as.character(PubYear)),
  df_emb %>% filter(SID!='E041'),
  df_emb %>% filter(SID=='E041') %>% mutate(ComparableMetricValue='13.2'),
  df_med,
  df_unp
) %>%
  mutate(across(c(Included,PubYear,Numerator,Denominator,ComparableMetricValue,`95%LCI`,`95%UCI`),as.numeric))


df_final %>% count(SID)

# 1️⃣Final validation dataset ----
rio::export(df_final,'validation/df_final.xlsx')

## df_final_2 ----
df_valid<-import('validation/df_final_2.xlsx')

df_valid.2<-df_valid %>%
  filter(Included==1) %>%
  select(-ExcludeReason) %>%
  separate(ComparableMetric,into = c('AGEGR','metric'),sep='_',remove=F) %>%
  mutate(ComparableMetricValue=as.numeric(ComparableMetricValue))

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
  

df_valid.3<-df_valid.2 %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region,Income2019),by=c('CountryName'='region')) %>%
  filter(!is.na(ISOCountry))

# Model estimated (code_10.3)----

df_model_inc<-import('rda/df_model_inc.rds',trust=T)
df_model_hos<-import('rda/df_model_hos.rds',trust=T)
df_model_mort.att<-import('rda/df_model_mort.att.rds',trust=T)
df_model_mort.ass<-import('rda/df_model_mort.ass.rds',trust=T)



df_model<-df_valid.3 %>%
  distinct(ISOCountry,AGEGR) %>%
  left_join(df_model_inc %>% select(ISOCountry,AGEGR,starts_with('inc_'))) %>%
  left_join(df_model_hos %>% select(ISOCountry,AGEGR,starts_with('hos_'))) %>%
  left_join(df_model_mort.att %>% select(ISOCountry,AGEGR,starts_with('mort.att_'))) %>%
  left_join(df_model_mort.ass %>% select(ISOCountry,AGEGR,starts_with('mort.ass_')))

df_model_long<-df_model %>%
  pivot_longer(cols=-c(ISOCountry,AGEGR), names_to = c("metric", "stat"),
               names_pattern = "(.*)_(025|500|975)",
               values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value,
    names_prefix = "model_"
  )

df_valid.4<-df_valid.3 %>%
  left_join(df_model_long) %>%
  rename(valid_est=ComparableMetricValue,valid_lci=`95%LCI`,valid_uci=`95%UCI`,
         model_est=model_500,model_lci=model_025,model_uci=model_975) %>%
  mutate(overlap = pmax(valid_lci, model_lci, na.rm = TRUE) <= pmin(valid_uci, model_uci, na.rm = TRUE)) %>%
  filter(AGEGR!='0-<24m')

## 2️⃣df_valid_4----
rio::export(df_valid.4,'rda/df_valid.4.rds')

# Plot validation ----

df_plot <- df_valid.4 %>% 
  select(SID,ISOCountry, CountryName, AGEGR, metric, starts_with("valid_"), starts_with("model_"),overlap) %>% 
  filter(!is.na(model_est)) %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','0-<12m','12-<60m','0-<60m'))) %>%
  mutate(CountryName_SID=paste0(CountryName,'_',SID))

df_plot_long <- df_plot %>% 
  pivot_longer(c(valid_est:model_uci), names_to = c("source", ".value"), names_sep = "_") %>% 
  mutate(source = recode(source, valid = "Validation", model = "Model"))

df_plot_long %>% 
  filter(metric == "inc") %>% 
  mutate(CountryName = reorder_within(CountryName, est, AGEGR)) %>% 
  ggplot(aes(CountryName, est, colour = source)) +
  geom_pointrange(
    aes(ymin = lci, ymax = uci,
        linetype = factor(overlap, levels = c(TRUE, FALSE))),
    position = position_dodge(.55)
  ) +
  facet_grid(metric ~ AGEGR, scales = "free_x", space = "free_x") +
  scale_x_reordered() +
  scale_color_lancet(name = NULL) +
  scale_linetype_discrete(name = NULL, labels = c("Overlapping", "Non-overlapping")) +
  labs(x = "Country", y = "Estimate") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

## inc ----

df_plot_long %>% 
  filter(metric=='inc') %>%
  ggplot(aes(CountryName_SID,y = est,colour = source)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),
  ) +
  #facet_grid(. ~ AGEGR, scales = "free_x",space = "free_x") +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "y")+
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported\n(external validation)'))+
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping")) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"))+
  labs(x = "Study", y = "RSV-associated ALRI incidence rate\n(/1,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_external_inc.tiff', width = 10,height = 6,dpi = 300)


## hos ----
df_plot_long %>% 
  filter(metric=='hos') %>%
  ggplot(aes(CountryName_SID,y = est,colour = source)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "y")+
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported\n(external validation)'))+
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping")) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"))+
  labs(x = "Study", y = "RSV-associated ALRI hospital admission rate\n(/1,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_external_hos.tiff', width = 10,height = 6,dpi = 300)

## mort ----

p_ass<-df_plot_long %>% 
  filter(metric %in% c('mort.ass')) %>%
  ggplot(aes(CountryName_SID,y = est,colour = source)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),
  ) +
  #facet_grid(. ~ AGEGR, scales = "free_x",space = "free_x") +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "y")+
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported\n(external validation)'))+
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping"), drop = FALSE) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"), drop = FALSE)+
  labs(x = "Study", y = "RSV-associated all-cause mortality rate\n(/10,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_att<-df_plot_long %>% 
  filter(metric %in% c('mort.att')) %>%
  ggplot(aes(CountryName_SID,y = est,colour = source)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),
  ) +
  facet_grid(. ~ AGEGR, scales = "free_x",space = "free_x") +
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported\n(external validation)')) +
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping")) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"))+
  labs(x = "Study", y = "RSV-attributable all-cause mortality rate\n(/10,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

    
p_ass + p_att +
  plot_layout(widths = c(6, 2),axes = "collect_x")

ggsave('plot/Valid_external_mort.tiff', width = 10,height = 6,dpi = 300)

# Table ----
res <- df_plot %>% tabyl(AGEGR, overlap, metric) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(1) %>%
  adorn_ns("front")

wb <- createWorkbook()

purrr::iwalk(res, ~{
  addWorksheet(wb, .y)
  writeData(wb, .y, .x)
})

saveWorkbook(wb, "docs/Valid_external_overlap.xlsx", overwrite = TRUE)

metric_map <- c(
  inc      = "RSV-associated ALRI incidence rate",
  hos      = "RSV-associated ALRI hospital admission rate",
  mort.att = "RSV-attributable all-cause mortality rate",
  mort.ass = "RSV-associated all-cause mortality rate"
)

res <- df_plot %>%
  tabyl(AGEGR, overlap, metric) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(1) %>%
  adorn_ns("front")

tab <- bind_rows(lapply(names(metric_map), function(nm){
  
  x <- res[[nm]] %>%
    rename(Yes = `TRUE`, No = `FALSE`) %>%
    select(AGEGR, Yes, No)
  
  # 删除全0 / (-) 行
  x <- x %>%
    filter(!(Yes %in% c("0 (0.0%)", "0 (-)") &
               No  %in% c("0 (0.0%)", "0 (-)")))
  
  # 加标题行
  bind_rows(
    tibble(
      AGEGR = metric_map[[nm]],
      Yes = "",
      No = ""
    ),
    x
  )
})) %>%
  rename(Age=AGEGR)

group_idx <- which(tab$Age %in% metric_map)

flextable(tab) %>%
  add_header_row(
    values = c("Age", "Uncertainty interval overlap"),
    colwidths = c(1, 2)
  ) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header", j = 1) %>%
  align(align = "center", part = "header") %>%
  merge_at(i = 1, j = 1:3, part = "body") %>%
  merge_at(i = 8, j = 1:3, part = "body") %>%
  merge_at(i = 15, j = 1:3, part = "body") %>%
  merge_at(i = 22, j = 1:3, part = "body") %>%
  bold(i = group_idx, part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:3, align = "right", part = "body") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx(path = "docs/Valid_external_overlap.docx")

# Study ----

df_valid.4 %>% 
  filter(!is.na(model_est)) %>%
  transmute(SID,References=sprintf('%s et al. %s',Author0,PubYear),Country=CountryName,Location=Region,`Study Period`=StudyPeriod,`Income level`=Income2019,Age=AGEGR,
            valid=sprintf('%.1f\n(%.1f-%.1f)',valid_est,valid_lci,valid_uci),
            model=sprintf('%.1f\n(%.1f-%.1f)',model_est,model_lci,model_uci),
            `Uncertainty interval overlap`=ifelse(overlap,'Yes','No'))

out <- imap(metric_map, function(label, m){
  
  df_valid.4 %>%
    filter(metric == m, !is.na(model_est)) %>%
    transmute(
      SID,
      References = sprintf('%s et al. %s', Author0, PubYear),
      Country = CountryName,
      Location = Region,
      `Study Period` = StudyPeriod,
      `Income level` = Income2019,
      Age = AGEGR,
      `Study-reported` = sprintf('%.1f\n(%.1f-%.1f)', valid_est, valid_lci, valid_uci),
      `Model-estimated` = sprintf('%.1f\n(%.1f-%.1f)', model_est, model_lci, model_uci),
      `Uncertainty interval overlap` = ifelse(overlap, 'Yes', 'No')
    )
})

write_xlsx(out, "docs/Study_external.xlsx")

wb <- createWorkbook()
addWorksheet(wb, "summary")

base_cols <- c(
  "SID","References","Country","Location",
  "Study Period","Income level","Age",
  "Study-reported","Model-estimated","Uncertainty interval overlap"
)

row <- 1

# ✔ 1. 只写一次表头
writeData(wb, 1, as.data.frame(t(base_cols)), startRow = row, colNames = FALSE)
row <- row + 1

for (m in names(metric_map)) {
  
  df <- df_valid.4 %>%
    filter(metric == m, !is.na(model_est)) %>%
    transmute(
      SID,
      References = sprintf('%s et al. %s', Author0, PubYear),
      Country = CountryName,
      Location = Region,
      `Study Period` = StudyPeriod,
      `Income level` = Income2019,
      Age = AGEGR,
      valid = sprintf('%.1f\n(%.1f-%.1f)', valid_est, valid_lci, valid_uci),
      model = sprintf('%.1f\n(%.1f-%.1f)', model_est, model_lci, model_uci),
      Overlap = ifelse(overlap, "Yes", "No")
    )
  
  # ✔ 2. section title（合并整行）
  writeData(wb, 1, metric_map[[m]], startRow = row, startCol = 1)
  mergeCells(wb, 1, rows = row, cols = 1:length(base_cols))
  
  row <- row + 1
  
  # ✔ 3. 数据（不写 header）
  writeData(wb, 1, df, startRow = row, colNames = FALSE)
  
  row <- row + nrow(df)
}

saveWorkbook(wb, "docs/Study_external_combined.xlsx", overwrite = TRUE)

df_plot %>% count(metric)
