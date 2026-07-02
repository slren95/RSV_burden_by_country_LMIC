## Exclusive breastfeeding (% of children under 6 months)
## https://data.worldbank.org/indicator/SH.STA.BFED.ZS


## Births, preterm, rate per 100 live births
## https://www.who.int/data/gho/data/indicators/indicator-details/GHO/preterm-birth-rate-per-100-live-births

## Under-five mortality rate
## https://data.unicef.org/topic/child-survival/under-five-mortality/

## Neonatal mortality rate
## https://data.unicef.org/topic/child-survival/neonatal-mortality/

## Infant mortality rate
## https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=CME&ver=1.0&dq=.CME_MRY0._T.&startPeriod=2016&endPeriod=2025

## Hospital beds (per 1,000 people)
## https://data.worldbank.org/indicator/SH.MED.BEDS.ZS

rm(list=ls())
library(tidyverse)
library(rio)
library(janitor)
library(psych)
library(corrplot)
library(Hmisc)
library(ggcorrplot)
library(patchwork)
library(tidygraph)
library(ggraph)

load("workspaceToBegin.RData") ## pop_mor.raw

rm(list = setdiff(ls(), c("pop_mor.raw","mor_all_DeCoDe.predict")))

## 1️⃣135 LMIC Country List ----
df_country.1<-openxlsx::read.xlsx('data/2019 calendar year LMIC.xlsx',startRow = 10) %>%
  setNames(c('ISOCountry','CountryName','Income2019')) %>%
  filter(!is.na(ISOCountry))

df_lmic.1<-df_country.1 %>% filter(Income2019!='H')

df_lmic.1 %>%
  arrange(CountryName) %>%
  mutate(Income2019=recode(Income2019,'UM'='Upper-middle','LM'='Lower-middle','L'='Low')) %>%
  transmute(Country=CountryName,`ISO Code`=ISOCountry,`Income level`=Income2019) %>%
  export('docs/List_of_LMIC.xlsx')
  

## Population 2019 ----
df_lmic.2<-df_lmic.1 %>%
  left_join(pop_mor.raw %>% select(ISOCountry,WHORegion,Y0,Y1,Y2,Y3,Y4) 
  )

df_lmic.2 %>% filter(is.na(Y0))
## ISOCountry      CountryName Income2019 WHORegion Y0 Y1 Y2 Y3 Y4
## 1        ASM   American Samoa         UM      <NA> NA NA NA NA NA
## 2        DMA         Dominica         UM       Amr NA NA NA NA NA
## 3        XKX           Kosovo         UM      <NA> NA NA NA NA NA
## 4        MHL Marshall Islands         UM       Wpr NA NA NA NA NA
## 5        TUV           Tuvalu         UM       Wpr NA NA NA NA NA

#### pop data in 2019 (from U.N.)----
df_li<-openxlsx::read.xlsx("data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx",sheet=1,startRow = 17)
df_li <-df_li %>% clean_names()

df_pop_un_2019<-df_li %>%
  filter(year==2019,type=='Country/Area') %>%
  dplyr::select(year,region_subregion_country_or_area,type,x0,x1,x2,x3,x4,x5) %>%
  mutate(across(starts_with('x'),~round(as.numeric(.x))))

df_pop_5<-df_pop_un_2019 %>%
  filter(str_detect(region_subregion_country_or_area,'(American Samoa|Dominica$|Kosovo|Marshall|Tuvalu)')) %>%
  arrange(region_subregion_country_or_area) %>%
  transmute(ISOCountry=c('ASM','DMA','XKX','MHL','TUV'),Y0=x0,Y1=x1,Y2=x2,Y3=x3,Y4=x4)

# Fix population of Tuvalu https://stats.gov.tv/category/population-and-social/ 250 birth per year in 2019
df_pop_5[df_pop_5$ISOCountry=='TUV',2:6]<-0.25
df_pop_5

#### complete Popultaiton data ----
df_lmic.3<-bind_rows(
  df_lmic.2 %>% filter(!is.na(Y0)),
  df_lmic.2 %>% filter(is.na(Y0)) %>% select(-c(Y0:Y4)) %>%
    left_join(df_pop_5) %>%
    mutate(WHORegion=case_when(ISOCountry=='ASM'~'Wpr',ISOCountry=='XKX'~'Eur',T~WHORegion))
)

## U5MR ----
df_u5mr.1<-openxlsx::read.xlsx('data/Under-five_Mortality_Rates_2024.xlsx',startRow = 14)

df_u5mr.2<-df_u5mr.1 %>%
  filter(`Uncertainty.Bounds*`=='Median') %>%
  select(ISO.Code,Country.Name,U5MR2019=`2019.5`)

df_u5mr.2 %>% filter(str_detect(Country.Name,'Kosovo'))

## NMR ----
df_nmr.1<-openxlsx::read.xlsx('data/Neonatal_Mortality_Rates_2024.xlsx',startRow = 14)

df_nmr.2<-df_nmr.1 %>%
  filter(`Uncertainty.Bounds*`=='Median') %>%
  select(ISO.Code,Country.Name,NMR2019=`2019.5`)

## American Samoa
## https://ourworldindata.org/grapher/child-mortality-rate-ihme?tab=chart&country=ASM

df_lmic.4<-df_lmic.3 %>%
  left_join(df_u5mr.2 %>% transmute(ISOCountry=ISO.Code,U5MR2019)) %>%
  mutate(U5MR2019=case_when(CountryName=='Kosovo'~11.15073,
                             CountryName=='American Samoa'~10.2,
                             T~U5MR2019))


## Prematurity ----
df_preterm.1<-openxlsx::read.xlsx('data/Preterm rate per 100 live births.xlsx',startRow = 3)

df_preterm.2<-df_preterm.1 %>%
  mutate(Period=as.integer(Period)) %>%
  filter(Period<=2019) %>%
  slice_max(Period,by=SpatialDimValueCode) %>%
  transmute(SpatialDimValueCode,Location,Period,prematurity=FactValueNumeric/100)

df_preterm.3<-read_csv("data/deepseek_prematurity.csv")


## LBW ----
df_lbw.1<-openxlsx::read.xlsx('data/unicef-who-lbw-estimates-2023-edition-to-2024-july-update.xlsx',sheet = 2,,startRow = 2)

df_lbw.2<-df_lbw.1 %>% filter(Estimate=='LBW estimate') %>%
  mutate(across(`2000`:`2020`,~parse_number(.x))) %>%
  filter(!is.na(`2019`))

## Breastfeeding ----
df_feeding.1<-read_csv('data/API_SH.STA.BFED.ZS_DS2_en_csv_v2_14364/API_SH.STA.BFED.ZS_DS2_en_csv_v2_14364.csv',skip = 4) %>%
  clean_names() %>%
  select(country_code,country_name,x2009:x2019)

sum(duplicated(df_feeding.1$country_code))

## Keep the last not NA value 
get_last_na<-function(vector,name){
  if(all(is.na(vector))){
    return(list(name=NA,value=NA))
  } else {
    idx=max(which(!is.na(vector)))
    return(list(name=name[idx],value=vector[idx]))
  }
}

df_feeding.2<-df_feeding.1 %>%
  rowwise() %>%
  mutate(
    last_info = list(get_last_na(c_across(x2009:x2019), seq(2009,2019))),
    x_latest = last_info$name,
    value = last_info$value
  ) %>%
  ungroup()

df_feeding.3<-df_feeding.2 %>%
  transmute(ISOCountry=country_code,country_name,year=x_latest,not_bf=1-value/100)

## Beds per 1000 person ----
df_beds.1<-read_csv('data/API_SH.MED.BEDS.ZS_DS2_en_csv_v2_10288/API_SH.MED.BEDS.ZS_DS2_en_csv_v2_10288.csv',skip = 3) %>%
  clean_names() %>%
  select(country_code,country_name,x2009:x2019)

sum(duplicated(df_beds.1$country_code))

df_beds.2<-df_beds.1 %>%
  rowwise() %>%
  mutate(
    last_info = list(get_last_na(c_across(x2009:x2019), seq(2009,2019))),
    x_latest = last_info$name,
    value = last_info$value
  ) %>%
  ungroup()

df_beds.3<-df_beds.2 %>%
  transmute(ISOCountry=country_code,country_name,year=x_latest,BEDS=value)

## Nurses and midwives (per 1,000 people) ----
df_numw.1<-read_csv('data/API_SH.MED.NUMW.P3_DS2_en_csv_v2_11138/API_SH.MED.NUMW.P3_DS2_en_csv_v2_11138.csv',skip = 3) %>%
  clean_names() %>%
  select(country_code,country_name,x2009:x2019)

df_numw.2<-df_numw.1 %>%
  rowwise() %>%
  mutate(
    last_info = list(get_last_na(c_across(x2009:x2019), seq(2009,2019))),
    x_latest = last_info$name,
    value = last_info$value
  ) %>% 
  transmute(ISOCountry=country_code,country_name,year=x_latest,NUMW=value)

## Physicians (per 1,000 people) ----
df_phys.1<-read_csv('data/API_SH.MED.PHYS.ZS_DS2_en_csv_v2_10977/API_SH.MED.PHYS.ZS_DS2_en_csv_v2_10977.csv',skip = 3) %>%
  clean_names() %>%
  select(country_code,country_name,x2009:x2019)

df_phys.2<-df_phys.1 %>%
  rowwise() %>%
  mutate(
    last_info = list(get_last_na(c_across(x2009:x2019), seq(2009,2019))),
    x_latest = last_info$name,
    value = last_info$value
  ) %>% 
  transmute(ISOCountry=country_code,country_name,year=x_latest,PHYS=value)

## Maternal mortality ratio (modeled estimate, per 100,000 live births) ----
df_mmrt.1<-read_csv('data/API_SH.STA.MMRT_DS2_en_csv_v2_130234/API_SH.STA.MMRT_DS2_en_csv_v2_130234.csv',skip = 3) %>%
  clean_names() %>%
  select(country_code,country_name,x2009:x2019)

df_mmrt.2<-df_mmrt.1 %>%
  rowwise() %>%
  mutate(
    last_info = list(get_last_na(c_across(x2009:x2019), seq(2009,2019))),
    x_latest = last_info$name,
    value = last_info$value
  ) %>% 
  transmute(ISOCountry=country_code,country_name,year=x_latest,MMRT=value)

## People using safely managed drinking water services (% of population) ----
df_smdw.1<-read_csv('data/API_SH.H2O.SMDW.ZS_DS2_en_csv_v2_125869/API_SH.H2O.SMDW.ZS_DS2_en_csv_v2_125869.csv',skip = 3) %>%
  clean_names() %>%
  select(country_code,country_name,x2009:x2019)

df_smdw.2<-df_smdw.1 %>%
  rowwise() %>%
  mutate(
    last_info = list(get_last_na(c_across(x2009:x2019), seq(2009,2019))),
    x_latest = last_info$name,
    value = last_info$value
  ) %>% 
  transmute(ISOCountry=country_code,country_name,year=x_latest,SMDW=value)

## Previous mortality Country List ----

df.country<-mor_all_DeCoDe.predict %>%
  distinct(ISOCountry,Country.Name,Country.Name2,Income2019,WHORegion) %>%
  mutate(is_LMIC=Income2019!='H') %>%
  arrange(desc(is_LMIC))

df.country %>%
  count(is_LMIC)
## <lgl>   <int>
## 1 FALSE      62
## 2 TRUE      133

df_lmic.4 %>%
  filter(!ISOCountry %in% df.country$ISOCountry)

## DHS Data ----
df_dhs.1<-openxlsx::read.xlsx('data/DHS_data_LMIC.xlsx',sheet = 1) %>%
  clean_names() %>%
  rename(ISOCountry=is0_country) %>%
  select(ISOCountry,country,income,contains('_p_weighted')) %>%
  set_names(~ifelse(str_detect(.x,'_p_weighted'),toupper(str_remove(.x,'_p_weighted')),.x))

names(df_dhs.1)


df_dhs.2 <-df_dhs.1 %>%
  filter(!if_all(VIS:PAS,is.na))


## Maternal Smoking ----
# df_smoking<-tabulapdf::extract_tables("data/mmc1.pdf",pages = 19:21)
# saveRDS(df_smoking,'data/df_smoking.rds')
df_smoking<-readRDS('data/df_smoking.rds')

df_smoking.1<-bind_rows(df_smoking[[1]],df_smoking[[2]],df_smoking[[3]]) %>%
  transmute(Country=...1,smoking=`Prevalence (%)`) %>%
  mutate(smoking=str_replace(smoking,'·','.'),
         smoking=parse_number(smoking)/100,
         Country=str_remove(Country,'\\*')) %>%
  filter(!is.na(smoking))

df_smoking.2<-df_smoking.1 %>%
  left_join(df_country.1 %>% select(-Income2019),by=c('Country'='CountryName'))

df_smoking.2 %>%
  filter(is.na(ISOCountry)) %>%
  write_csv('docs/df_smoking.2_na.csv')

df_smoking.2_new<-read_csv('docs/df_smoking.2_na_new.csv')

df_smoking.3<-bind_rows(
  df_smoking.2 %>%
    filter(!is.na(ISOCountry)),
  df_smoking.2_new
)

df_smoking.3 %>% count(ISOCountry,sort=T)

## HIV ----
df_lmic.4 %>% select(ISOCountry,CountryName,WHORegion) %>% write.csv('docs/df_lmic.4.csv')
df_hiv<-read_csv("data/HIV.csv") %>% clean_names()

list_asia_pacific = c('AFG', 'ARM', 'AZE', 'BGD', 'BTN', 'CHN', 'FJI', 'GEO', 'IDN', 'IND', 'IRN', 'KAZ', 'KGZ', 'KHM', 'KIR', 'LAO', 'LKA', 'MDV', 'MHL', 'MMR', 'MNG', 'MYS', 'NPL', 'PAK', 'PHL', 'PNG', 'PRK', 'SLB', 'THA', 'TJK', 'TLS', 'TON', 'TKM', 'TUV', 'UZB', 'VNM', 'VUT', 'WSM', 'ASM')
list_caribbean = c('BLZ', 'CUB', 'DMA', 'DOM', 'GRD', 'HTI', 'JAM', 'LCA', 'VCT')
list_east_sou_africa = c('AGO', 'BWA', 'BDI', 'COM', 'ERI', 'ETH', 'KEN', 'LSO', 'MDG', 'MWI', 'MOZ', 'NAM', 'RWA', 'SYC', 'SOM', 'ZAF', 'SSD', 'SWZ', 'TZA', 'UGA', 'ZMB', 'ZWE')
list_latin_america = c('ARG', 'BOL', 'BRA', 'COL', 'CRI', 'ECU', 'SLV', 'GTM', 'GUY', 'HND', 'MEX', 'NIC', 'PAN', 'PRY', 'PER', 'SUR', 'VEN')
list_me_nafrica = c('DZA', 'DJI', 'EGY', 'IRQ', 'JOR', 'LBN', 'LBY', 'MAR', 'PSE', 'SAU', 'SDN', 'SYR', 'TUN', 'YEM')
list_west_cen_africa = c('BEN', 'BFA', 'CMR', 'CAF', 'TCD', 'COG', 'COD', 'CIV', 'GNQ', 'GAB', 'GMB', 'GHA', 'GIN', 'GNB', 'LBR', 'MLI', 'MRT', 'NER', 'NGA', 'STP', 'SEN', 'SLE', 'TGO')

df_hiv
df_hiv.2<-df_hiv %>%
  transmute(region_country,HIV=prevalence_of_children_who_are_heu_percent/100,iso_country_code,
            ISOs=case_when(!is.na(iso_country_code) ~ iso_country_code,
                           region_country=='Asia and the Pacific'~paste(list_asia_pacific,collapse=','),
                           region_country=='Caribbean'~paste(list_caribbean,collapse=','),
                           region_country=='Eastern & southern Africa'~paste(list_east_sou_africa,collapse=','),
                           region_country=='Latin America'~paste(list_latin_america,collapse=','),
                           region_country=='Middle East & North Africa'~paste(list_me_nafrica,collapse=','),
                           region_country=='Western & central Africa'~paste(list_west_cen_africa,collapse=','),
                           region_country=='Global'~paste(df_lmic.4$ISOCountry,collapse=',')
                           ))

df_hiv.3_region<-df_hiv.2 %>% filter(is.na(iso_country_code))
df_hiv.3_country<-df_hiv.2 %>% filter(!is.na(iso_country_code))

df_hiv.4<-df_lmic.1 %>%
  left_join(df_hiv.3_country %>% select(iso_country_code,HIV),by=c('ISOCountry'='iso_country_code'))

df_hiv.4 %>% count(is.na(HIV)) %>% adorn_totals() %>%
  adorn_percentages()

df_hiv.5<-bind_rows(
  df_hiv.4 %>% filter(!is.na(HIV)) %>% mutate(HIV_type=ISOCountry),
  df_hiv.4 %>% filter(is.na(HIV)) %>%
    mutate(HIV=case_when(str_detect(df_hiv.3_region[[2,'ISOs']],ISOCountry)~df_hiv.3_region[[2,'HIV']],
                         str_detect(df_hiv.3_region[[3,'ISOs']],ISOCountry)~df_hiv.3_region[[3,'HIV']],
                         str_detect(df_hiv.3_region[[4,'ISOs']],ISOCountry)~df_hiv.3_region[[4,'HIV']],
                         str_detect(df_hiv.3_region[[5,'ISOs']],ISOCountry)~df_hiv.3_region[[5,'HIV']],
                         str_detect(df_hiv.3_region[[6,'ISOs']],ISOCountry)~df_hiv.3_region[[6,'HIV']],
                         str_detect(df_hiv.3_region[[7,'ISOs']],ISOCountry)~df_hiv.3_region[[7,'HIV']],
                         str_detect(df_hiv.3_region[[1,'ISOs']],ISOCountry)~df_hiv.3_region[[1,'HIV']]),
           HIV_type=case_when(str_detect(df_hiv.3_region[[2,'ISOs']],ISOCountry)~df_hiv.3_region[[2,'region_country']],
                              str_detect(df_hiv.3_region[[3,'ISOs']],ISOCountry)~df_hiv.3_region[[3,'region_country']],
                              str_detect(df_hiv.3_region[[4,'ISOs']],ISOCountry)~df_hiv.3_region[[4,'region_country']],
                              str_detect(df_hiv.3_region[[5,'ISOs']],ISOCountry)~df_hiv.3_region[[5,'region_country']],
                              str_detect(df_hiv.3_region[[6,'ISOs']],ISOCountry)~df_hiv.3_region[[6,'region_country']],
                              str_detect(df_hiv.3_region[[7,'ISOs']],ISOCountry)~df_hiv.3_region[[7,'region_country']],
                              str_detect(df_hiv.3_region[[1,'ISOs']],ISOCountry)~df_hiv.3_region[[1,'region_country']]))
)

# GBD_2016 ----
df_outpatient<-import('data/cursor/extracted/Table_S10_outpatient_flat.csv') %>%
  mutate(ISOCountry=ifelse(location=='Niger','NER',ISOCountry))
df_inpatient<-import('data/cursor/extracted/Table_S11_inpatient_flat.csv') %>%
  mutate(ISOCountry=ifelse(location=='Niger','NER',ISOCountry))

df_inpatient %>% 
  filter(!ISOCountry=='') %>% 
  dplyr::select(where(~is.numeric(.x))) %>% 
  mutate(across(c(everything()),~.x)) %>%
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )

## Link Covariate with ISOCountry ----

df_lmic.5<-df_lmic.4 %>%
  left_join(df_preterm.2 %>% transmute(ISOCountry=SpatialDimValueCode,PREM=prematurity)) %>%
  left_join(df_feeding.3 %>% transmute(ISOCountry,NOTBF=not_bf)) %>%
  left_join(df_dhs.1 %>% transmute(ISOCountry,VIS,LBW,SIB,M25,CRO,PAS)) %>%
  left_join(df_smoking.3 %>% transmute(ISOCountry,SMOK=smoking)) %>%
  left_join(df_hiv.5 %>% transmute(ISOCountry,HIV,HIV_type)) %>%
  left_join(df_nmr.2 %>% transmute(ISOCountry=ISO.Code,NMR2019)) %>%
  left_join(df_beds.3 %>% transmute(ISOCountry,BEDS)) %>%
  left_join(df_phys.2 %>% transmute(ISOCountry,PHYS)) %>%
  left_join(df_numw.2 %>% transmute(ISOCountry,NUMW)) %>%
  left_join(df_mmrt.2 %>% transmute(ISOCountry,MMRT)) %>%
  left_join(df_smdw.2 %>% transmute(ISOCountry,SMDW)) %>%
  left_join(df_outpatient %>% transmute(ISOCountry,OUTP=rate_2016)) %>%
  left_join(df_inpatient %>% transmute(ISOCountry,INPA=rate_2016)) %>%
  left_join(df_lbw.2 %>% transmute(ISOCountry=ISO3,LBW2=`2019`/100)) %>%
  left_join(df_preterm.3 %>% transmute(ISOCountry=ISO3,PREM2=PretermRate2020/100)) %>%
  mutate(across(c(PREM,NOTBF,VIS,LBW,SIB,M25,CRO,PAS,SMOK,BEDS,NMR2019,PHYS,NUMW,MMRT,SMDW,OUTP,INPA,LBW2,PREM2),list('type'=~ifelse(is.na(.x),'Imputed',NA_character_))),
         pop_0006=Y0/2,pop_0612=Y0/2,pop_1260=Y1+Y2+Y3+Y4)

names(df_lmic.5)[32:50] %>%
  map(~count(df_lmic.5,!!sym(.x)))


# 2️⃣Imputation ---- 

## Variables that need to be imputed
vars_to_impute <- c("PREM","NOTBF","VIS", "LBW", "SIB","M25","CRO","PAS","SMOK","NMR2019","BEDS","PHYS","NUMW","MMRT","SMDW","OUTP",'INPA','LBW2','PREM2') 

df_lmic.5 %>% select(U5MR2019,all_of(vars_to_impute)) %>%
  mutate(across(everything(),log)) %>%
  pairs.panels(method = "pearson",
              hist.col = "#00AFBB",
              density = TRUE,
              ellipses = TRUE)

data_analysis <- df_lmic.5 %>%
  select(U5MR2019, all_of(vars_to_impute)) %>%
  mutate(across(all_of(vars_to_impute), ~ log(.))) 

# 计算相关系数矩阵和 p 值
cor_results <- corr.test(data_analysis, 
                         method = "pearson",
                         use = "pairwise")  # 成对删除缺失

# 只看 U5MR2019 与其他变量的结果
cor_with_U5MR <- cor_results$r["U5MR2019", vars_to_impute]
p_with_U5MR <- cor_results$p["U5MR2019", vars_to_impute]

summary_table <- data.frame(
  Variable = vars_to_impute,
  Correlation = round(cor_with_U5MR, 3),
  P_value = round(p_with_U5MR, 4),
  Significance = case_when(
    p_with_U5MR < 0.001 ~ "***",
    p_with_U5MR < 0.01 ~ "**",
    p_with_U5MR < 0.05 ~ "*",
    TRUE ~ ""
  ),
  Missing_rate = round(sapply(df_lmic.5[, vars_to_impute], function(x) mean(is.na(x)) * 100), 1)
) %>%
  arrange(desc(abs(Correlation)))


## Function: for a given variable, impute missing values
## within each group based on the closest U5MR value
impute_by_closest <- function(df, var) {
  df %>%
    arrange(U5MR2019) %>%     ## Sort within group by U5MR
    mutate(
      !!var := map_dbl(row_number(), function(i) {
        
        ## If value is not missing, keep it
        value <- .data[[var]][i]
        if (!is.na(value)) return(value)
        
        ## For missing values:
        ## 1. compute distance in U5MR between this row and all rows
        dist <- abs(U5MR2019 - U5MR2019[i])
        
        ## 2. Exclude rows where the target variable is NA
        ##    by assigning distance = Inf
        valid_dist <- ifelse(is.na(.data[[var]]), Inf, dist)
        
        ## 3. Find the row with minimum distance
        idx <- which.min(valid_dist)
        
        ## 4. Use the non-missing nearest value to impute
        return(.data[[var]][idx])
      })
    )
}

impute_by_mean <- function(df, var) {
  
  ## 步骤1：计算交叉组均值 (Income2019 + WHOregion)
  df_cross <- df %>%
    group_by(Income2019, WHORegion) %>%
    summarise(cross_mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  ## 步骤2：计算仅 Income2019 组均值
  df_income <- df %>%
    group_by(Income2019) %>%
    summarise(income_mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  ## 步骤3：将两个均值合并回原数据
  df %>%
    left_join(df_cross, by = c("Income2019", "WHORegion")) %>%
    left_join(df_income, by = "Income2019") %>%
    mutate(
      ## 填补逻辑：优先用交叉组均值，若为 NaN 则用 Income 组均值
      !!var := ifelse(
        is.na(.data[[var]]),
        ifelse(
          is.nan(cross_mean),
          income_mean,
          cross_mean
        ),
        .data[[var]]
      )
    ) %>%
    select(-cross_mean, -income_mean)  # 删除辅助列
}

## Main pipeline:
## Group by income (L / LM / UM),
## sort by U5MR within each group,
## and apply the imputation for each variable in vars_to_impute
df_lmic_imputed <- df_lmic.5 %>%
  group_by(Income2019) %>%
  arrange(U5MR2019) %>%
  do({
    tmp <- .
    for (v in vars_to_impute) {
      # tmp <- impute_by_closest(tmp, v)
      tmp <- impute_by_mean(tmp, v)
    }
    tmp
  }) %>%
  ungroup() %>%
  arrange(Income2019,U5MR2019) %>%
  select(-PREM2,-PREM2_type,-LBW,-LBW_type) %>%
  rename(LBW=LBW2,LBW_type=LBW2_type)

export(df_lmic_imputed,'docs/df_lmic_imputed.xlsx')
export(df_lmic_imputed,'rda/df_lmic_imputed.rds')
export(df_lmic.5,'rda/df_lmic.5.rds')

vars_to_impute  %>%
  set_names() %>%           
  map_int(~sum(is.na(df_lmic_imputed[[.x]])))

df_lmic_imputed %>%
  select(ends_with('type')) %>%
  summarise(across(everything(), ~ mean(replace(.x, is.na(.x), 0) == "Imputed", na.rm = TRUE))) %>%
  t()

# Population by income level ----
pop_region.raw_new<-df_lmic_imputed %>%
  ungroup() %>%
  summarise(across(matches('^(Y|pop_)'),sum),.by=Income2019) %>%
  mutate(pop_U5=Y0+Y1+Y2+Y3+Y4) %>%
  pivot_longer(-Income2019,names_to = 'Group',values_to = 'pop') %>%
  filter(str_detect(Group,'pop_')) %>%
  mutate(AGEGR=recode(Group,'pop_0006'='0-<6m','pop_0612'='6-<12m','pop_1260'='12-<60m','pop_U5'='0-<60m'))

export(pop_region.raw_new,'rda/pop_region.raw_new.rds')

# 135 Country List ----
df_lmic_imputed %>%
  arrange(Income2019,CountryName) %>%
  group_by(Income2019) %>%
  mutate(Country = paste(CountryName, collapse = ', ')) %>%
  ungroup() %>%
  add_count(Income2019) %>%
  distinct(Income2019,Country,n) %>%
  mutate(Income = case_when(
    Income2019 == "L"  ~ "Low income",
    Income2019 == "LM" ~ "Lower-middle income",
    Income2019 == "UM" ~ "Upper-middle income",
    TRUE ~ "Unknown"  # 如果有其他值
  ),.after=1) %>%
  select(-1)  %>%
  rio::export('docs/list_lmic_135.xlsx')

# Imputation justification ----
df_lmic.6<-df_lmic.5 %>% rename(U5MR=U5MR2019)
RF <- c("U5MR","PREM","LBW2",'SIB',"SMOK","M25","PAS","NOTBF")

df_lmic.6 %>% select(all_of(RF)) %>%
  skimr::skim() %>%
  mutate(missing_rate=round((1-complete_rate)*100,1))

df_lmic_imputed %>% ungroup() %>%
  count(!is.na(HIV_type)) %>% adorn_percentages()


df_log<-df_lmic.6 %>%
  mutate(Income2019_num = as.numeric(factor(Income2019,levels=c('L','LM','UM')))) %>% 
  dplyr::select(all_of(RF), Income2019 = Income2019_num) %>%
  mutate(across(all_of(RF), ~.x))


split(df_lmic.6,df_lmic.5$Income2019) %>%
  map(~{
    title<-recode(.x[1,'Income2019'],'L'='Low','LM'='Lower-middle','UM'='Upper-middle')
    .x %>%
      dplyr::select(all_of(RF)) %>%
      mutate(across(all_of(RF), ~.x)) %>%
      cor(method = "spearman",use = "pairwise.complete.obs") %>%
      ggcorrplot(method = "circle", 
                 type = "lower",
                 lab = TRUE, lab_size = 2.5,tl.cex=8,
                 colors = c("blue", "white", "red"),
                 title = paste0(title,' income'))+
      theme(title=element_text(size=8))
  }) %>%
  wrap_plots(ncol=2)+
  plot_layout(guide='collect')

ggsave("plot/U5MR_corr.tiff", width = 7, height = 7, dpi = 300,compression = "lzw")


save.image('rda/code_01_prevalance_of_risk_factor.RData')
