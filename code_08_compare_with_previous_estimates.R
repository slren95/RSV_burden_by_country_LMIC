rm(list=ls())
library(tidyverse)
library(tabulapdf)
library(janitor)
library(ggVennDiagram)
library(ggsci)
library(ggrepel)

df_lmic_imputed<-import('rda/df_lmic_imputed.rds',trust=T)

# Lancet 2022 ----
## 提取 PDF 文件中的表格 
file_path <- "data/Lancet_appendix.pdf"

tables <- extract_tables(
  file = file_path,
  pages = 33:35
)

df_combined <- do.call(rbind, tables) %>% clean_names()


# 数据转换 
df_combined$country

country_map <- c(
  "Afghanistan" = "AFG",
  "Algeria" = "DZA",
  "Angola" = "AGO",
  "Antigua and Barbuda" = "ATG",
  "Argentina" = "ARG",
  "Armenia" = "ARM",
  "Azerbaijan" = "AZE",
  "Bahamas, The" = "BHS",
  "Bahrain" = "BHR",
  "Bangladesh" = "BGD",
  "Barbados" = "BRB",
  "Belize" = "BLZ",
  "Benin" = "BEN",
  "Bhutan" = "BTN",
  "Bolivia" = "BOL",
  "Botswana" = "BWA",
  "Brazil" = "BRA",
  "Brunei Darussalam" = "BRN",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cabo Verde" = "CPV",
  "Cambodia" = "KHM",
  "Cameroon" = "CMR",
  "Central African Republic" = "CAF",
  "Chad" = "TCD",
  "Chile" = "CHL",
  "China" = "CHN",
  "Colombia" = "COL",
  "Comoros" = "COM",
  "Congo, Dem. Rep." = "COD",
  "Congo, Rep." = "COG",
  "Costa Rica" = "CRI",
  "Côte d'Ivoire" = "CIV",
  "Cuba" = "CUB",
  "Djibouti" = "DJI",
  "Dominican Republic" = "DOM",
  "Ecuador" = "ECU",
  "Egypt, Arab Rep." = "EGY",
  "El Salvador" = "SLV",
  "Equatorial Guinea" = "GNQ",
  "Eritrea" = "ERI",
  "Eswatini" = "SWZ",
  "Ethiopia" = "ETH",
  "Fiji" = "FJI",
  "Gabon" = "GAB",
  "Gambia, The" = "GMB",
  "Georgia" = "GEO",
  "Ghana" = "GHA",
  "Grenada" = "GRD",
  "Guatemala" = "GTM",
  "Guinea" = "GIN",
  "Guinea-Bissau" = "GNB",
  "Guyana" = "GUY",
  "Haiti" = "HTI",
  "Honduras" = "HND",
  "India" = "IND",
  "Indonesia" = "IDN",
  "Iran, Islamic Rep." = "IRN",
  "Iraq" = "IRQ",
  "Jamaica" = "JAM",
  "Jordan" = "JOR",
  "Kazakhstan" = "KAZ",
  "Kenya" = "KEN",
  "Kiribati" = "KIR",
  "Korea, Dem. Rep." = "PRK",
  "Korea, Rep." = "KOR",
  "Kuwait" = "KWT",
  "Kyrgyz Republic" = "KGZ",
  "Lao PDR" = "LAO",
  "Lebanon" = "LBN",
  "Lesotho" = "LSO",
  "Liberia" = "LBR",
  "Libya" = "LBY",
  "Madagascar" = "MDG",
  "Malawi" = "MWI",
  "Malaysia" = "MYS",
  "Maldives" = "MDV",
  "Mali" = "MLI",
  "Mauritania" = "MRT",
  "Mauritius" = "MUS",
  "Mexico" = "MEX",
  "Micronesia, Fed. Sts." = "FSM",
  "Mongolia" = "MNG",
  "Morocco" = "MAR",
  "Mozambique" = "MOZ",
  "Myanmar" = "MMR",
  "Namibia" = "NAM",
  "Nepal" = "NPL",
  "Nicaragua" = "NIC",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "Oman" = "OMN",
  "Pakistan" = "PAK",
  "Panama" = "PAN",
  "Papua New Guinea" = "PNG",
  "Paraguay" = "PRY",
  "Peru" = "PER",
  "Philippines" = "PHL",
  "Qatar" = "QAT",
  "Rwanda" = "RWA",
  "Samoa" = "WSM",
  "São Tomé and Principe" = "STP",
  "Saudi Arabia" = "SAU",
  "Senegal" = "SEN",
  "Seychelles" = "SYC",
  "Sierra Leone" = "SLE",
  "Singapore" = "SGP",
  "Solomon Islands" = "SLB",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "South Sudan" = "SSD",
  "Sri Lanka" = "LKA",
  "St. Lucia" = "LCA",
  "St. Vincent and the Grenadines" = "VCT",
  "Sudan" = "SDN",
  "Suriname" = "SUR",
  "Syrian Arab Republic" = "SYR",
  "Tajikistan" = "TJK",
  "Tanzania" = "TZA",
  "Thailand" = "THA",
  "Timor-Leste" = "TLS",
  "Togo" = "TGO",
  "Tonga" = "TON",
  "Trinidad and Tobago" = "TTO",
  "Tunisia" = "TUN",
  "Turkey" = "TUR",
  "Turkmenistan" = "TKM",
  "Uganda" = "UGA",
  "United Arab Emirates" = "ARE",
  "Uruguay" = "URY",
  "Uzbekistan" = "UZB",
  "Vanuatu" = "VUT",
  "Venezuela, RB" = "VEN",
  "Vietnam" = "VNM",
  "Yemen, Rep." = "YEM",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)

df_combined2 <- df_combined %>%
  mutate(ISOCountry=country_map[country]) %>%
  # 提取 R_est, R_lci, R_uci
  mutate(
    IR_est = str_extract(incidence_rate_per_1_000_children_per_year, "^[0-9.]+") %>% as.numeric(),
    IR_lci = str_extract(incidence_rate_per_1_000_children_per_year, "(?<=\\()[0-9.]+") %>% as.numeric(),
    IR_uci = str_extract(incidence_rate_per_1_000_children_per_year, "(?<=–)[0-9.]+(?=\\))") %>% as.numeric()
  ) %>%
  # 提取 N_est, N_lci, N_uci
  mutate(
    N_est = str_extract(number_of_episodes, "^[0-9 ]+") %>% str_remove_all(" ") %>% as.numeric(),
    N_lci = str_extract(number_of_episodes, "(?<=\\()[0-9 ]+") %>% str_remove_all(" ") %>% as.numeric(),
    N_uci = str_extract(number_of_episodes, "(?<=–)[0-9 ]+(?=\\))") %>% str_remove_all(" ") %>% as.numeric()
  )

names(country_map)[!names(country_map) %in% df_combined$country]

myvenn <- function(x, y, name1 = "set1", name2 = "set2") {
  cat("\n===== 集合比较 =====\n")
  cat(sprintf("%s 数量: %d\n", name1, length(x)))
  cat(sprintf("%s 数量: %d\n", name2, length(y)))
  cat(sprintf("共同数量: %d\n", length(intersect(x, y))))
  cat(sprintf("%s 独有数量: %d\n", name1, length(setdiff(x, y))))
  cat(sprintf("%s 独有数量: %d\n", name2, length(setdiff(y, x))))
  
  cat("\n===== 共同元素 =====\n")
  print(intersect(x, y))
  
  cat(sprintf("\n===== %s 独有 =====\n", name1))
  print(setdiff(x, y))
  
  cat(sprintf("\n===== %s 独有 =====\n", name2))
  print(setdiff(y, x))
}

myvenn(df_combined2$ISOCountry,df_lmic_imputed$ISOCountry)

# Estimated by country ----


RF.res.impute2<-import('rda/RF.res.impute2.rds',trust=T) # community incidence

RF.res.impute2_sum<-RF.res.impute2 %>%
  reframe(across(c(IR,N),list(est=~quantile(.x,0.5),
                              lci=~quantile(.x,0.025),
                              uci=~quantile(.x,0.975)),.names = "{.col}.{.fn}"),.by = c(ISOCountry,Income2019,AGEGR)) 

df_compare<-RF.res.impute2_sum %>%
  filter(AGEGR=='0-<60m') %>%
  select(ISOCountry,Income2019,matches('(IR|N)\\.')) %>%
  left_join(df_combined2 %>% select(ISOCountry,matches('^(IR|N)_'))) %>%
  filter(!is.na(IR_est))

df_compare %>%
  filter(IR_est==max(IR_est) | IR_est==min(IR_est))

df_compare <-df_compare  %>%
  mutate(is_outlier=(IR_est==max(IR_est) | IR_est==min(IR_est)))

cor(df_compare$IR.est,df_compare$IR_est)

ggplot(df_compare, aes(x = IR.est, y = IR_est)) +
  geom_point(aes(color=Income2019),size = 2.5, alpha = 0.8)+
  coord_fixed(ratio = 1,
              xlim = c(0, 85),  
              ylim = c(0, 85))+
  geom_smooth(method = "lm", formula = y ~ x + 0, 
              se = TRUE, color = "#E69F00", fill = "#F0E442", 
              alpha = 0.2, size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "black", size = 0.4, alpha = 0.5) +
  labs(x = 'RSV-associated ALRI incidence rate\n(/1000 person-years)',
       y = 'RSV-associated ALRI incidence rate (Lancet 2022)\n(/1000 person-years)',
       color = 'Income') +
  scale_color_lancet(labels=c('L'='Low','LM'='Lower-middle','UM'='Upper-middle'),name='Income level')+
  scale_x_continuous(expand = expansion())+
  scale_y_continuous(expand = expansion())+
  geom_text_repel(
    data = subset(df_compare, is_outlier == TRUE), # 只给异常值加标签
    aes(label = ISOCountry),                      # 假设列名是 ISO_country
    size = 3.5,
    vjust = -0.5, 
    hjust = 0.5,
    box.padding = 0.5,     # 标签距离点的距离
    point.padding = 0.3,   # 避开点的灵敏度
    segment.color = "grey50", # 连接线颜色
    show.legend = FALSE    # 不在图例中显示 'a'
  ) +
  theme_bw()

ggsave("plot/Compare_lancet.tiff", 
       width = 10, height = 6, units = "in", dpi = 600,compression = "lzw", bg = "white")

