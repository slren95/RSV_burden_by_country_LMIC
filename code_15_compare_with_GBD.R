rm(list=ls())
library(tidyverse)
library(rio)
library(janitor)
library(ggsci)
library(patchwork)
library(scales)
library(echarts4r)
library(psych)
library(ggstatsplot)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(rstatix)
library(ggh4x)

# GBD 2019 Etiology Mortality Data RSV----
df_gbd2019<-import('data/IHME-GBD_2023_DATA-9e0ff278-1/IHME-GBD_2023_DATA-9e0ff278-1.csv')

df_gbd2019_2<-df_gbd2019 %>%
  filter(metric_name %in% c('Rate','Number','Percent'))

df_hie<-import('data/GBD/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX') %>%
  clean_names()

df_hie_country<- df_hie %>% filter(level==3)

df_gbd2019_3<-df_gbd2019_2 %>%
  left_join(df_hie_country) %>%
  relocate(level,.after=location_name)

df_country2019<-df_gbd2019_3 %>% 
  filter(!is.na(level) | location_name %in% c('Türkiye',"Côte d'Ivoire"))

iso3_map <- function(){c(
  "Saint Lucia"="LCA",
  "Northern Mariana Islands"="MNP",
  "Slovenia"="SVN",
  "Kenya"="KEN",
  "Palau"="PLW",
  "Kiribati"="KIR",
  "Bermuda"="BMU",
  "Burkina Faso"="BFA",
  "Belgium"="BEL",
  "Saint Vincent and the Grenadines"="VCT",
  "Morocco"="MAR",
  "Maldives"="MDV",
  "Azerbaijan"="AZE",
  "Barbados"="BRB",
  "Georgia"="GEO",
  "Brunei Darussalam"="BRN",
  "Algeria"="DZA",
  "Puerto Rico"="PRI",
  "Suriname"="SUR",
  "Chile"="CHL",
  "Trinidad and Tobago"="TTO",
  "Philippines"="PHL",
  "Cabo Verde"="CPV",
  "Malawi"="MWI",
  "Denmark"="DNK",
  "Afghanistan"="AFG",
  "Belarus"="BLR",
  "Bangladesh"="BGD",
  "Micronesia (Federated States of)"="FSM",
  "Saint Kitts and Nevis"="KNA",
  "Cuba"="CUB",
  "Kazakhstan"="KAZ",
  "Lesotho"="LSO",
  "Oman"="OMN",
  "Bhutan"="BTN",
  "Egypt"="EGY",
  "Austria"="AUT",
  "Greenland"="GRL",
  "Kyrgyzstan"="KGZ",
  "Bolivia (Plurinational State of)"="BOL",
  "India"="IND",
  "Mozambique"="MOZ",
  "Latvia"="LVA",
  "San Marino"="SMR",
  "Republic of Korea"="KOR",
  "Thailand"="THA",
  "Mongolia"="MNG",
  "Tajikistan"="TJK",
  "Tokelau"="TKL",
  "France"="FRA",
  "Dominican Republic"="DOM",
  "Samoa"="WSM",
  "Saudi Arabia"="SAU",
  "Iraq"="IRQ",
  "Nepal"="NPL",
  "Tuvalu"="TUV",
  "Republic of Moldova"="MDA",
  "Indonesia"="IDN",
  "Peru"="PER",
  "Cameroon"="CMR",
  "Seychelles"="SYC",
  "Cyprus"="CYP",
  "Viet Nam"="VNM",
  "United States of America"="USA",
  "Uzbekistan"="UZB",
  "Pakistan"="PAK",
  "Turkmenistan"="TKM",
  "Greece"="GRC",
  "Sierra Leone"="SLE",
  "Ecuador"="ECU",
  "Kuwait"="KWT",
  "Tonga"="TON",
  "United States Virgin Islands"="VIR",
  "Ukraine"="UKR",
  "Angola"="AGO",
  "Burundi"="BDI",
  "Colombia"="COL",
  "Albania"="ALB",
  "Malaysia"="MYS",
  "Congo"="COG",
  "Fiji"="FJI",
  "United Republic of Tanzania"="TZA",
  "Romania"="ROU",
  "Ireland"="IRL",
  "Ghana"="GHA",
  "Chad"="TCD",
  "Libya"="LBY",
  "Equatorial Guinea"="GNQ",
  "Slovakia"="SVK",
  "Finland"="FIN",
  "Central African Republic"="CAF",
  "Zambia"="ZMB",
  "Myanmar"="MMR",
  "El Salvador"="SLV",
  "Poland"="POL",
  "Bulgaria"="BGR",
  "Djibouti"="DJI",
  "Bosnia and Herzegovina"="BIH",
  "Italy"="ITA",
  "South Sudan"="SSD",
  "Bahamas"="BHS",
  "Marshall Islands"="MHL",
  "Japan"="JPN",
  "Guyana"="GUY",
  "Serbia"="SRB",
  "Paraguay"="PRY",
  "Guinea-Bissau"="GNB",
  "American Samoa"="ASM",
  "Botswana"="BWA",
  "Estonia"="EST",
  "Palestine"="PSE",
  "Sri Lanka"="LKA",
  "Costa Rica"="CRI",
  "Czechia"="CZE",
  "Sweden"="SWE",
  "Germany"="DEU",
  "Croatia"="HRV",
  "Malta"="MLT",
  "Democratic Republic of the Congo"="COD",
  "Papua New Guinea"="PNG",
  "Gambia"="GMB",
  "Honduras"="HND",
  "Mali"="MLI",
  "Jamaica"="JAM",
  "Ethiopia"="ETH",
  "Qatar"="QAT",
  "Guatemala"="GTM",
  "Lithuania"="LTU",
  "Cook Islands"="COK",
  "Singapore"="SGP",
  "Nicaragua"="NIC",
  "Democratic People's Republic of Korea"="PRK",
  "Gabon"="GAB",
  "North Macedonia"="MKD",
  "Hungary"="HUN",
  "Senegal"="SEN",
  "Guinea"="GIN",
  "Iceland"="ISL",
  "Norway"="NOR",
  "Belize"="BLZ",
  "Bahrain"="BHR",
  "Guam"="GUM",
  "Timor-Leste"="TLS",
  "Solomon Islands"="SLB",
  "China"="CHN",
  "Montenegro"="MNE",
  "Mexico"="MEX",
  "Russian Federation"="RUS",
  "Niger"="NER",
  "Comoros"="COM",
  "Madagascar"="MDG",
  "United Kingdom"="GBR",
  "New Zealand"="NZL",
  "Venezuela (Bolivarian Republic of)"="VEN",
  "Togo"="TGO",
  "Spain"="ESP",
  "Sudan"="SDN",
  "Israel"="ISR",
  "Eritrea"="ERI",
  "Taiwan"="TWN",
  "Panama"="PAN",
  "Dominica"="DMA",
  "Liberia"="LBR",
  "Andorra"="AND",
  "Sao Tome and Principe"="STP",
  "Armenia"="ARM",
  "Iran (Islamic Republic of)"="IRN",
  "Tunisia"="TUN",
  "Argentina"="ARG",
  "Switzerland"="CHE",
  "South Africa"="ZAF",
  "Australia"="AUS",
  "Mauritius"="MUS",
  "Monaco"="MCO",
  "Brazil"="BRA",
  "Namibia"="NAM",
  "Jordan"="JOR",
  "Nauru"="NRU",
  "Mauritania"="MRT",
  "Niue"="NIU",
  "Cambodia"="KHM",
  "United Arab Emirates"="ARE",
  "Rwanda"="RWA",
  "Zimbabwe"="ZWE",
  "Luxembourg"="LUX",
  "Uruguay"="URY",
  "Vanuatu"="VUT",
  "Nigeria"="NGA",
  "Syrian Arab Republic"="SYR",
  "Eswatini"="SWZ",
  "Benin"="BEN",
  "Somalia"="SOM",
  "Grenada"="GRD",
  "Lebanon"="LBN",
  "Lao People's Democratic Republic"="LAO",
  "Uganda"="UGA",
  "Canada"="CAN",
  "Haiti"="HTI",
  "Netherlands"="NLD",
  "Portugal"="PRT",
  "Antigua and Barbuda"="ATG",
  "Yemen"="YEM",
  "Türkiye"="TUR",
  "Côte d'Ivoire"="CIV"
)}

df_country2019<-df_country2019 %>%
  mutate(ISOCountry=iso3_map()[location_name]) %>%
  relocate(ISOCountry,.after = location_name)

df_country_lite <-df_country2019 %>%
  dplyr::select(ISOCountry,location_id,location_name,level,year,age_name,year,metric_name,val,upper,lower) %>%
  mutate(metric_name=recode(metric_name,'Rate'='R.GBD','Number'='N.GBD','Percent'='P.GBD')) %>% 
  rename(est=val,lci=lower,uci=upper) %>%
  pivot_wider(names_from = metric_name,values_from = c(est,lci,uci), names_glue = '{metric_name}_{.value}') %>%
  relocate(1:6,starts_with('N'),starts_with('R')) %>%
  mutate(pop_GBD=round(N.GBD_est/R.GBD_est*100),.after = location_name) %>%
  mutate(age_group=recode(age_name,'<1 year'='0-<12m','<5 years'='0-<60m'),.after = age_name)

df_country_lite %>%
  ggplot(aes(P.GBD_est,fill=age_name))+
  geom_density(alpha = 0.5) +
  labs(x = "Proportion RSV", y = "Density", fill='Age group'
  ) +
  theme_minimal() +
  theme(legend.position = "top")

## GBD etiology rule ----
18883.89/426982.14*100
# Etiology of RSV Percent(4.42% of LRI(cause))
# Cause of detahs or injury(LRI 426982.14 <1years)


# Previous Mortality ----

df_sum_all_DeCoDe<-import('rda/df_sum_all_DeCoDe.rds',trust=T)

df_mort_sum<-df_sum_all_DeCoDe %>%
  transmute(ISOCountry, CountryName, Income2019,pop_0012,pop_0060,
    R_lci_0012 = m0012_R_025*100,
    R_est_0012 = m0012_R_500*100,
    R_uci_0012 = m0012_R_975*100,
    N_lci_0012 = m0012_N_025,
    N_est_0012 = m0012_N_500,
    N_uci_0012 = m0012_N_975,
    R_lci_0060 = m0060_R_025*100,
    R_est_0060 = m0060_R_500*100,
    R_uci_0060 = m0060_R_975*100,
    N_lci_0060 = m0060_N_025,
    N_est_0060 = m0060_N_500,
    N_uci_0060 = m0060_N_975
  ) %>%
  pivot_longer(
    cols = -c(ISOCountry, CountryName, Income2019),
    names_to = c(".value", "age_group"),
    names_pattern = "^(.*)_(0012|0060)$"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "0012" ~ "0-<12m",
      age_group == "0060" ~ "0-<60m"
    )
  )

df_compare<-inner_join(df_mort_sum,df_country_lite)

# Compare ----

df_compare %>%
  select(CountryName, pop_GBD, pop,age_group) %>%
  pivot_longer(cols = c(pop_GBD, pop),
               names_to = "Population_Type",
               values_to = "Population") %>%
  ggplot(aes(x = CountryName, y = Population, fill = Population_Type)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Country", y = "Population", fill = "Population Source") +
  scale_fill_manual(values = c("pop_GBD" = "steelblue", "pop_0012" = "orange"))+
  coord_flip()+
  facet_wrap(age_group~.)

## rate ----
df_compare %>%
  dplyr::select(ISOCountry, Income2019,age_group,starts_with("R.GBD_"), starts_with("R_")) %>%
  pivot_longer(
    cols = -c(ISOCountry, Income2019,age_group),
    names_to = c("metric", "stat"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  ggplot(aes(x = ISOCountry, y = est, color = metric)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_linerange(aes(ymin = lci, ymax = uci),
                 position = position_dodge(width = 0.5),
                 linewidth = 1) +
  facet_grid2(age_group~Income2019, scales = "free_x",space="free_x",independent = "none") +
  scale_color_lancet(name=NULL,labels=c('Model-estimated','GBD-estimated'))+
  theme_bw(base_size = 10) +
  labs(y='RSV-attributable mortality rate(per 100,000 person-years)',x='Country')+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

ggsave('plot/Compare_mortality_with_GBD_rate.tiff',width = 14, height = 7.5, dpi = 300,compression = "lzw")

## number ----
df_compare %>%
  filter(age_group=='0-<12m') %>%
  dplyr::select(ISOCountry, Income2019,age_group,starts_with("N.GBD_"), starts_with("N_")) %>%
  pivot_longer(
    cols = -c(ISOCountry, Income2019,age_group),
    names_to = c("metric", "stat"),
    names_sep = "_"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  ggplot(aes(x = ISOCountry, y = est, color = metric)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_linerange(aes(ymin = lci, ymax = uci),
                 position = position_dodge(width = 0.5),
                 linewidth = 1.2) +
  facet_grid2(age_group~Income2019, scales = "free_y",ind='y') +
  scale_color_lancet()+
  theme_bw() +
  labs(y='RSV-attributable all cause deaths')+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

#ggsave('plot/Compare_mortality_with_GBD_rate.png',width = 3000,height = 1800,dpi=200,unit='px')

## dist ----
df_compare %>%
  pivot_longer(cols=c(R_est,R.GBD_est)) %>%
  ggplot(aes(value,fill=name))+
  geom_density(alpha = 0.5) +
  labs(x = "Mortality rate", y = "Density", fill='Age group'
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  facet_grid(~age_group)

df_compare %>%
  pivot_longer(cols=c(N_est,N.GBD_est)) %>%
  ggplot(aes(value,fill=name))+
  geom_density(alpha = 0.5) +
  labs(x = "Number of deaths", y = "Density", fill='Age group'
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  facet_grid(~age_group)

## normal test ----
df_compare %>%
  mutate(across(c(R_est, R.GBD_est), log, .names = "log_{.col}")) %>%
  group_by(age_group) %>%
  shapiro_test(log_R_est, log_R.GBD_est)

df_compare %>%
  mutate(across(c(R_est, R.GBD_est), log, .names = "log_{.col}")) %>%
  ggqqplot(x = "log_R_est", facet.by = "age_group", 
           title = "Q-Q Plot: log(R_est) by age_group")

df_compare %>%
  mutate(across(c(R_est, R.GBD_est), log, .names = "log_{.col}")) %>%
  ggqqplot(x = "log_R.GBD_est", facet.by = "age_group",
           title = "Q-Q Plot: log(R.GBD_est) by age_group")

## correlation ----
cor.test(df_compare$R_est,df_compare$R.GBD_est,method = 'spearman')
cor.test(df_compare$N_est,df_compare$N.GBD_est,method = 'spearman')
cor.test(df_compare$pop,df_compare$pop_GBD,method = 'spearman')

## diagonal ----

df_compare %>%
  dplyr::select(ISOCountry, Income2019, starts_with("N.GBD_"), starts_with("N_")) %>% 
  pivot_longer(
    cols = -c(ISOCountry, Income2019),
    names_to = c("metric", "stat"),
    names_sep = "_"
  ) %>%
  group_by(ISOCountry, Income2019, metric, stat) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  ggplot(aes(x = ISOCountry, y = est, color = metric)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_linerange(aes(ymin = lci, ymax = uci),
                 position = position_dodge(width = 0.5),
                 linewidth = 1.2) +
  facet_wrap(~Income2019, scales = "free") +
  scale_color_lancet()+
  theme_bw() +
  ylim(c(0,3000))+
  #labs(y='RSV-attributable mortality(<1years,per 100,000 person-years)')+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

df_compare %>%
  select(R_est,R.GBD_est,N_est,N.GBD_est,pop,pop_GBD) %>%
  mutate(across(everything(),~log(.x+1))) %>%
  pairs.panels(method = "pearson",
              hist.col = "#00AFBB",lm = T,
              density = TRUE,
              ellipses = TRUE)

grouped_ggscatterstats(
  data = df_compare %>% mutate(across(c(R_est,R.GBD_est),log)),
  grouping.var = age_group,
  x = R_est,
  y = R.GBD_est,
  type = "non-parametric",        # 使用Pearson相关系数
  marginal = TRUE,            # 显示边缘分布
  digits = 2,
  bf.message=FALSE,
  xsidehistogram.args = list(fill = "#009E73",color='black',linewidth=.5),
  ysidehistogram.args = list(fill = "#D55E00",color='black'),
  xlab = "Model-estimated RSV-attributable all cause mortality rate\n(per 100,000 person-years,log-transformed)",   # x轴标签
  ylab = "GBD-estimated RSV-attributable mortality rate\n(per 100,000 person-years,log-transformed)" # y轴标签
) & theme(ggside.panel.scale = .3)

ggsave('plot/Compare_GBD.tiff', width = 12, height = 6, dpi = 300,compression = "lzw")


ggplot(df_compare,aes(N_est,N.GBD_est))+
  geom_point(aes(color=Income2019))+
  geom_smooth(method = 'lm',formula=as.formula(y~x+0))+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme_bw()+
  facet_grid(.~age_group)+
  labs(x= "Model-estimated RSV-attributable all cause deaths (log-transformed)",
       y = "GBD-estimated RSV-attributable all cause deaths (log-transformed)",
       color='Income level')

ggplot(df_compare,aes(R_est,R.GBD_est))+
  geom_point(aes(color=Income2019))+
  geom_smooth(method = 'lm',formula=as.formula(y~x+0))+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme_bw()+
  facet_grid(.~age_group)+
  labs(x= "Model-estimated RSV-attributable all cause mortality rate\n(per 100,000 person-years,log-transformed)",
       y = "GBD-estimated RSV-attributable all cause mortality rate\n(per 100,000 person-years,log-transformed)",
       color='Income level')

