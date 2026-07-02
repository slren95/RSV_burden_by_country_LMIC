# 1 Add deno----
hos_rate.data <- left_join(r22.raw, all.raw, by = c("SID", "PubYear"))
hos_rate_average.data <- hos_rate.data[hos_rate.data$DataYear=="Average" & 
                                         hos_rate.data$Inclusion,] %>%
  filter(!if_all(everything(), is.na))

hos_rate_average.data$HosALRI_Deno <- with(hos_rate_average.data, Deno * HosALRI_p/100)  
# 2 Minimal dataset----
hos_rate_average.data$Impute <- 0 # raw data
hos_rate_average.minimal <- hos_rate_average.data[
  hos_rate_average.data$Income != "H",
  c("SID", "Author0", "PubYear","UNICEF", "WHO", "Setting0", "StudyMY", "Income", "Dev", "Indigenous", 
    "ViralTest","QA_all",
    "AGEGR", "HosALRI_Deno","HosALRI_N", "Impute","Country",'ISOCountry')
]
# 3 Imputation----
hos_rate.impute <- genHosImputeTab(hos_rate_average.minimal)
export(hos_rate.impute,'Tables/hos_rate.impute.xlsx')

hos_rate_impute.data <- do.call(
  rbind,
  by(hos_rate_average.minimal[
    with(hos_rate_average.minimal, 
         SID %in% intersect(setdiff(SID[AGEGR != "0-<60m" & !is.na(HosALRI_N)], SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)]), # 其他年龄组有数据，0-<60m无数据的研究
                            SID[AGEGR %in% c("0-<12m", "0-<24m", "0-<36m") & !is.na(HosALRI_N)])  # 0-<12m或0-<24m或0-<36m有数据的研究
         & AGEGR %in% c("0-<12m", "0-<24m", "0-<36m")),],  # 仅保留0-<12m、0-<24m和0-<36m的数据
    hos_rate_average.minimal[
      with(hos_rate_average.minimal, 
           SID %in% intersect(setdiff(SID[AGEGR != "0-<60m" & !is.na(HosALRI_N)], SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)]),
                              SID[AGEGR %in% c("0-<12m", "0-<24m", "0-<36m") & !is.na(HosALRI_N)]) 
           & AGEGR %in% c("0-<12m", "0-<24m", "0-<36m")),]$SID,
    FUN = genHosImputeEach
  )
)
hos_rate_combined <- rbind(hos_rate_impute.data, hos_rate_average.minimal)
# 4 Meta-analysis----
# 4.1 RSV-ALRI by income----
hos_rate_ALRI_Income.meta <- rbind(
  do.call(rbind, 
          by(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<6m",],
             hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<6m", "Income"],
             genMetaRateEach, prefix = "HosALRI", varToKeep = c("AGEGR","Income", "Impute"))),
  do.call(rbind, 
          by(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="6-<12m",],
             hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="6-<12m", "Income"],
             genMetaRateEach, prefix = "HosALRI", varToKeep = c("AGEGR","Income","Impute"))),
  do.call(rbind, 
          by(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<60m",],
             hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<60m", "Income"],
             genMetaRateEach, prefix = "HosALRI", varToKeep = c("AGEGR","Income","Impute"))),
  do.call(rbind, 
          by(hos_rate_combined[hos_rate_combined$AGEGR=="0-<60m",],
             hos_rate_combined[hos_rate_combined$AGEGR=="0-<60m", "Income"],
             genMetaRateEach.Impute, prefix = "HosALRI", varToKeep = c("AGEGR","Income","Impute")))
)
names(hos_rate_ALRI_Income.meta)[2] <- "Group"
hos_rate_ALRI_Income.meta <- hos_rate_ALRI_Income.meta[names(hos_rate_ALRI_Income.meta)!="I2"]
hos_rate_ALRI_Income.meta <- left_join(hos_rate_ALRI_Income.meta, pop_region.raw)
hos_rate_ALRI_Income.meta$N.est <- with(hos_rate_ALRI_Income.meta, round(IR.est * Pop),0)
hos_rate_ALRI_Income.meta$N.lci <- with(hos_rate_ALRI_Income.meta, round(IR.lci * Pop),0)
hos_rate_ALRI_Income.meta$N.uci <- with(hos_rate_ALRI_Income.meta, round(IR.uci * Pop),0)

df_list<-split(hos_rate_ALRI_Income.meta,hos_rate_ALRI_Income.meta[c("AGEGR", "Impute")])
genRateLMIC(df_list[[2]],n.level = 3)
hos_rate_ALRI_Income.meta <- do.call(rbind,
                                     by(hos_rate_ALRI_Income.meta, hos_rate_ALRI_Income.meta[c("AGEGR", "Impute")],
                                        FUN = genRateLMIC, n.level = 3))

# 5 Imputation for LMIC----
# IRR (0-<6m/0-<60m, 6-<12m/0-<60m) within each income
# hos_rate_ALRI_Income.impute <- genComImputeLMIC(
#   rbind(
#     hos_rate_ALRI_Income.meta[hos_rate_ALRI_Income.meta$AGEGR != "0-<60m" & hos_rate_ALRI_Income.meta$Group != "LMIC",], 
#     hos_rate_ALRI_Income.meta[hos_rate_ALRI_Income.meta$AGEGR == "0-<60m" & hos_rate_ALRI_Income.meta$Group != "LMIC" & 
#                                 hos_rate_ALRI_Income.meta$Impute == "1",]
#   )
# )

export(hos_rate_ALRI_Income.meta,'../rda/hos_rate_ALRI_Income.meta.rds')
export(hos_rate_average.minimal,'../rda/hos_rate_average.minimal.rds')
export(hos_rate_combined,'../rda/hos_rate_combined.rds')

hos_rate.impute %>%
  transmute(AGEGR=sprintf('0-<60m vs %s',AGEGR) %>% str_replace_all('m',' months'),
            IRR=sprintf('%.2f (%.2f-%.2f)',IRR.est,IRR.lci,IRR.uci)) %>%
  set_names('Age group pair','IRR for RSV-associated ALRI hospital admission rate') %>%
  export('docs/IRR_6_vs_60_hos.csv')
