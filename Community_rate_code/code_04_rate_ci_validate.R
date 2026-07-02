library(tidyverse)
library(rio)
library(epiR)

all.raw<-import('rda/all.raw.rds')


calculate_incidence_rate <- function(N, Deno) {
  result <- epi.conf(
    dat = as.matrix(cbind(N, Deno)),
    ctype = "inc.rate",
    method = "exact",
    conf.level = 0.95
  )
  # 返回发生率（est）、置信区间下限（lci）、上限（uci）并乘以1000
  c(est = N / Deno, lci = result$lower, uci = result$upper) * 1000
}

smart_epi_calc <- function(N, Deno, CI_str) {
  if (!is.na(CI_str)) {
    # 如果有CI字符串，用正则提取并返回
    nums <- as.numeric(unlist(strsplit(CI_str, "[^0-9.]+")))
    nums <- nums[!is.na(nums)]
    return(c(est = nums[1], lci = nums[2], uci = nums[3]))
  } else {
    # 如果没有，执行原有的 epi.conf 计算
    return(calculate_incidence_rate(N, Deno))
  }
}

# 应用
df_valid_com <- import('./docs/com_rate_average.minimal_validate.xlsx') %>%
  filter(!is.na(AGEGR2),!is.na(ALRI_Deno)) %>%
  mutate(
    results = pmap(list(ALRI_N, ALRI_Deno, CI), smart_epi_calc)
  ) %>%
  unnest_wider(results)



df_valid_hos<-import('./docs/hos_rate_average.minimal_validate.xlsx') %>%
  filter(!is.na(AGEGR2),!is.na(HosALRI_Deno)) %>%
  mutate(
    results = pmap(list(HosALRI_N, HosALRI_Deno, CI), smart_epi_calc)
  ) %>%
  unnest_wider(results)

# all.raw ----
df_valid_com2<-left_join(df_valid_com, all.raw %>% select(-c(Country,ISOCountry,Income)), by = c("SID", "PubYear"))
df_valid_hos2<-left_join(df_valid_hos, all.raw %>% select(-c(Country,ISOCountry,Income)), by = c("SID", "PubYear"))

export(df_valid_com2,'../rda/df_valid_com2.rds')
export(df_valid_hos2,'../rda/df_valid_hos2.rds')
 