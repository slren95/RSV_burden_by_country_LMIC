library(shiny)
library(bslib)
library(dplyr)
library(rio)
library(scales)
library(leaflet)
#library(sf)
library(rnaturalearth)
library(plotly)
library(gt) 

# tryCatch({
#   df_sum_all_DeCoDe <- import('rda/df_sum_all_DeCoDe.rds', trust=TRUE)
#   df_sum_all_NP <- import('rda/df_sum_all_NP.rds', trust=TRUE)
#   RF.res.impute2 <- import('rda/RF.res.impute2.rds', trust=TRUE)
#   df_hos_by_country2_1000 <- import("rda/df_hos_by_country2_1000.rds", trust=TRUE)
#   df_lmic_imputed <- import('rda/df_lmic_imputed.rds', trust=TRUE)
# }, error = function(e) {
#   stop("Error loading data files. Please check file paths: ", e$message)
# })
# 
# save.image('shiny/shiny_data.RData')
# load('shiny/shiny_data.RData')
load('shiny_data.RData')

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3 = iso_a3_eh, iso_a2, economy, geometry) %>%
  filter(!is.na(iso_a3)) %>%
  mutate(iso_a3 = toupper(trimws(iso_a3)))


# 统一提取各指标的 R 和 N 矩阵点估计值
df_inc <- RF.res.impute2 %>%
  reframe(across(c(IR, N), list(q500 = ~quantile(.x, 0.5, na.rm = TRUE))), .by = c(ISOCountry, AGEGR)) %>%
  transmute(ISOCountry, AGEGR, metric = "RSV-associated ALRI incidence", R = IR_q500, N = N_q500)

df_hos <- df_hos_by_country2_1000 %>%
  select(ISOCountry, contains('q500'), -c(`Rate_12-<60m_q500`, `Hos_12-<60m_q500`)) %>% 
  pivot_longer(cols = contains("_"), names_to = c("type", "AGEGR"), names_pattern = "(.*)_(.*)_q500.*", values_to = "value") %>%
  mutate(type = case_when(type == "Hos" ~ "N", type == "Rate" ~ "R")) %>%
  pivot_wider(id_cols = c(ISOCountry, AGEGR), names_from = type, values_from = value) %>%
  mutate(metric = "RSV-associated ALRI hospital admission")

df_mort_att <- df_sum_all_DeCoDe %>%
  select(ISOCountry, ends_with('500')) %>%
  pivot_longer(cols = starts_with("m"), names_to = c("AGEGR", "type"), names_pattern = "m(.*)_([NR])_500", values_to = "value") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(AGEGR = recode(AGEGR, "0006" = "0-<6m", "0612" = "6-<12m", "1260" = "12-<60m", "0060" = "0-<60m", "0012" = "0-<12m"),
         R = R * 100, metric = "RSV-attributable mortality")

df_mort_ass <- df_sum_all_NP %>%
  select(ISOCountry, ends_with('500')) %>%
  pivot_longer(cols = starts_with("m"), names_to = c("AGEGR", "type"), names_pattern = "m(.*)_([NR])_500", values_to = "value") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(AGEGR = recode(AGEGR, "0006" = "0-<6m", "0612" = "6-<12m", "1260" = "12-<60m", "0060" = "0-<60m", "0012" = "0-<12m"),
         R = R * 100, metric = "RSV-associated mortality")

df_master <- bind_rows(df_inc, df_hos, df_mort_att, df_mort_ass) %>%
  mutate(ISOCountry = toupper(trimws(ISOCountry))) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry, CountryName,Income2019,WHORegion) %>% distinct(), by = "ISOCountry") %>%
  left_join(world_sf %>% select(iso_a3,iso_a2),by=c('ISOCountry'='iso_a2'))

country_choices <- df_master %>% 
  filter(!is.na(CountryName)) %>% 
  distinct(ISOCountry, CountryName) %>% 
  mutate(label = paste0(CountryName, " (", ISOCountry, ")")) %>%
  { setNames(.$ISOCountry, .$label) }

lmic_countries <- df_lmic_imputed %>% pull(ISOCountry) %>% unique()