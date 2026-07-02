# 1 Add deno----
com_rate.data <- left_join(r21.raw, all.raw, by = c("SID", "PubYear"))
com_rate_average.data <- com_rate.data[com_rate.data$DataYear=="Average" & 
                                         com_rate.data$Inclusion,]
com_rate_average.data$ALRI_Deno <- with(com_rate_average.data, Deno * ALRI_p/100)   # RSV tested ALRI
com_rate_average.data$CWI_Deno <- with(com_rate_average.data, Deno * CWI_p/100)     # RSV tested ALRI with chest wall indrawing
com_rate_average.data$sALRI_Deno <- with(com_rate_average.data, Deno * sALRI_p/100) # RSV tested severe ALRI (not including chest wall indrawing)
# 2 Minimal dataset----
com_rate_average.data$Impute <- 0 # raw data
com_rate_average.minimal <- com_rate_average.data[
  com_rate_average.data$Income != "H",
  c("SID", "Author0", "PubYear","UNICEF", "WHO", "Setting0", "StudyMY", "Income", "Dev", "Indigenous", 
    "ViralTest", "studyLabel", "QA_all","USID",
    "AGEGR", "ALRI_Deno","ALRI_N", "CWI_Deno", "CWI_N","sALRI_Deno", "sALRI_N", "Impute")
]



# 3 Imputation----
com_rate.impute <- genComImputeTab(com_rate_average.minimal)
export(com_rate.impute,'Tables/com_rate.impute.xlsx')

com_rate_impute.data <- do.call(
  rbind,
  by(com_rate_average.minimal[
    with(com_rate_average.minimal, 
         SID %in% intersect(setdiff(SID[AGEGR != "0-<60m" & !is.na(ALRI_N)], SID[AGEGR == "0-<60m" & !is.na(ALRI_N)]), # 其他年龄组有数据，0-<60m无数据的研究
                            SID[AGEGR %in% c("0-<12m", "0-<24m", "0-<36m") & !is.na(ALRI_N)])  # 0-<12m或0-<24m或0-<36m有数据的研究
         & AGEGR %in% c("0-<12m", "0-<24m", "0-<36m")),],  # 仅保留0-<12m、0-<24m和0-<36m的数据
    com_rate_average.minimal[
      with(com_rate_average.minimal, 
           SID %in% intersect(setdiff(SID[AGEGR != "0-<60m" & !is.na(ALRI_N)], SID[AGEGR == "0-<60m" & !is.na(ALRI_N)]),
                              SID[AGEGR %in% c("0-<12m", "0-<24m", "0-<36m") & !is.na(ALRI_N)]) 
           & AGEGR %in% c("0-<12m", "0-<24m", "0-<36m")),]$SID,
    FUN = genComImputeEach
  )
)
com_rate_combined <- rbind(com_rate_impute.data, com_rate_average.minimal)
# 4 Meta-analysis----
# 4.1 RSV-ALRI by income----
com_rate_ALRI_Income.meta <- rbind(
  do.call(rbind, 
          by(com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<6m",],
             com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<6m", "Income"],
             genMetaRateEach, prefix = "ALRI", varToKeep = c("AGEGR","Income", "Impute"))),
  do.call(rbind, 
          by(com_rate_average.minimal[com_rate_average.minimal$AGEGR=="6-<12m",],
             com_rate_average.minimal[com_rate_average.minimal$AGEGR=="6-<12m", "Income"],
             genMetaRateEach, prefix = "ALRI", varToKeep = c("AGEGR","Income","Impute"))),
  do.call(rbind, 
          by(com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<60m",],
             com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<60m", "Income"],
             genMetaRateEach, prefix = "ALRI", varToKeep = c("AGEGR","Income","Impute"))),
  do.call(rbind, 
          by(com_rate_combined[com_rate_combined$AGEGR=="0-<60m",],
             com_rate_combined[com_rate_combined$AGEGR=="0-<60m", "Income"],
             genMetaRateEach.Impute, prefix = "ALRI", varToKeep = c("AGEGR","Income","Impute")))
)
names(com_rate_ALRI_Income.meta)[2] <- "Group"
com_rate_ALRI_Income.meta <- com_rate_ALRI_Income.meta[names(com_rate_ALRI_Income.meta)!="I2"]
com_rate_ALRI_Income.meta <- left_join(com_rate_ALRI_Income.meta, pop_region.raw)
com_rate_ALRI_Income.meta$N.est <- with(com_rate_ALRI_Income.meta, round(IR.est * Pop),0)
com_rate_ALRI_Income.meta$N.lci <- with(com_rate_ALRI_Income.meta, round(IR.lci * Pop),0)
com_rate_ALRI_Income.meta$N.uci <- with(com_rate_ALRI_Income.meta, round(IR.uci * Pop),0)
com_rate_ALRI_Income.meta_before<-com_rate_ALRI_Income.meta
df_list<-split(com_rate_ALRI_Income.meta,com_rate_ALRI_Income.meta[c("AGEGR", "Impute")])
genRateLMIC(df_list[[2]],n.level = 3)
com_rate_ALRI_Income.meta <- do.call(rbind,
                                     by(com_rate_ALRI_Income.meta, com_rate_ALRI_Income.meta[c("AGEGR", "Impute")],
                                        FUN = genRateLMIC, n.level = 3))

# 5 Imputation for LMIC----
# IRR (0-<6m/0-<60m, 6-<12m/0-<60m) within each income
com_rate_ALRI_Income.impute <- genComImputeLMIC(
  rbind(
    com_rate_ALRI_Income.meta[com_rate_ALRI_Income.meta$AGEGR != "0-<60m" & com_rate_ALRI_Income.meta$Group != "LMIC",], 
    com_rate_ALRI_Income.meta[com_rate_ALRI_Income.meta$AGEGR == "0-<60m" & com_rate_ALRI_Income.meta$Group != "LMIC" & 
                                com_rate_ALRI_Income.meta$Impute == "1",]
  )
)

com_rate_ALRI_Income.impute %>%
  transmute(Group=paste0(Group,'IC'),AGEGR=sprintf('%s vs 0-<60m',AGEGR) %>% str_replace_all('m',' months'),
            IRR=sprintf('%.2f (%.2f-%.2f)',IRR.est,IRR.lci,IRR.uci)) %>%
  set_names('Income level','Age group pair','IRR for RSV-associated ALRI incidence rate') %>%
  export('docs/IRR_6_vs_60.csv')


# By income IR ----
export(com_rate_ALRI_Income.meta,'../rda/com_rate_ALRI_Income.meta.rds')
