# 1.计算LMIC每一个收入层级0-5岁的baseline incidence rate（即无任何危险因素下的发病率）
## 部分国家缺失某些RF的prevalence：
## 先单独搜一下这个国家有没有，如果还是没有，用同收入、同WHO region的平均值插补
RF.minimal <- RF.new %>% 
  mutate(pop_U5 = pop_0006 + pop_0612 + pop_1260) %>%
  dplyr::select("Income2019", "ISOCountry", "pop_U5",
                "PREM", "NOTBF", "LBW", "SIB", "M25", "PAS", "SMOK", "HIV") %>%
  rename_with(~paste0(., "_P"), all_of(c("PREM", "NOTBF", "LBW", "SIB", "M25", "PAS", "SMOK", "HIV")))


# 生成每个国家的RR
RF.minimal <- do.call(rbind,
                      by(RF.minimal, RF.minimal$ISOCountry, FUN = genCountryRR))
# 每个收入层级内，每个国家按0-5岁人口比例加权RR=每个国家的RR×该国家0-5岁人口/所有国家0-5岁人口
RF.minimal <- RF.minimal %>%
  group_by(Income2019) %>%
  mutate(
    RR_weighted = pop_U5 / sum(pop_U5) * RR  # 组内按0-5岁人口占比加权
  ) %>%
  ungroup()

# RR from sampling
set.seed(20260323)
RF.minimal_1000 <- 1:1000 %>%
  purrr::imap(~{
    message('Process: ',.y)
    do.call(rbind,
            by(RF.minimal, RF.minimal$ISOCountry, FUN = genCountryRR_new)) %>%
        group_by(Income2019) %>%
        mutate(
          RR_weighted = pop_U5 / sum(pop_U5) * RR  # 组内按0-5岁人口占比加权
        ) %>%
        ungroup()
  })


# Method 1 ----

# 每个收入层级内，0-5岁的基线发病率=0-5岁的meta汇总发病率÷汇总的加权RR
RF.baseInc <- com_rate_ALRI_Income.meta %>%
  filter(AGEGR == "0-<60m", Impute == 1, Group != "LMIC") %>%
  dplyr::select(Group, IR.est, IR.lci, IR.uci) %>%
  left_join(
    RF.minimal %>%
      group_by(Income2019) %>%
      summarise(sum_RR_weighted = sum(RR_weighted, na.rm = TRUE), 
                .groups = "drop"),
    by = c("Group" = "Income2019")
  ) %>%
  mutate(
    IR.est = IR.est / sum_RR_weighted,
    IR.lci = IR.lci / sum_RR_weighted,
    IR.uci = IR.uci / sum_RR_weighted
  ) %>%
  dplyr::select(Group, IR.est, IR.lci, IR.uci)

# 2.计算每个国家5岁以下儿童基于危险因素RR估算的发病率和发病数
# 每个收入层级内,每个国家的发病率IR=RR×基线发病率
RF.res <- RF.minimal[c("Income2019", "ISOCountry", "pop_U5", "RR")] %>%
  left_join(RF.baseInc, by = c("Income2019" = "Group")) %>% 
  mutate(
    IR.est = IR.est * RR,
    IR.lci = IR.lci * RR,
    IR.uci = IR.uci * RR
  ) %>%
  dplyr::select(-RR)
# 每个国家的发病数N
RF.res$N.est <- RF.res$pop_U5 * RF.res$IR.est
RF.res$N.lci <- RF.res$pop_U5 * RF.res$IR.lci
RF.res$N.uci <- RF.res$pop_U5 * RF.res$IR.uci
# 去掉人口数为0的国家
RF.res <- RF.res[RF.res$pop_U5 != 0,]

# 3.基于IRR，估计0-<6m、6-<12m发病率和发病数
RF.res.impute <- RF.res %>% 
  left_join(RF.new[c("ISOCountry", "pop_0006", "pop_0612", "pop_1260")]) %>% 
  mutate(pop_0012=pop_0006+pop_0612) %>%
  pivot_longer(cols = starts_with("pop_"),
               names_to = "AGEGR",
               values_to = "pop") %>%
  mutate(AGEGR = case_when(
    AGEGR == "pop_U5"   ~ "0-<60m",
    AGEGR == "pop_0006" ~ "0-<6m",
    AGEGR == "pop_0612" ~ "6-<12m",
    AGEGR == "pop_1260" ~ "12-<60m",
    AGEGR == "pop_0012" ~ "0-<12m",
    TRUE ~ AGEGR)
  ) %>%
  rowwise() %>%
  mutate(
    IR.est = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, IR.est),
    IR.lci = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, IR.lci),
    IR.uci = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, IR.uci),
    N.est = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, N.est),
    N.lci = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, N.lci),
    N.uci = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, N.uci)
  ) %>%
  ungroup()

RF.res.impute <- RF.res.impute %>%
  left_join(com_rate_ALRI_Income.impute, by = c("Income2019" = "Group", "AGEGR")) %>%
  group_by(ISOCountry) %>%
  mutate(
    IR.est = ifelse(AGEGR %in% c("0-<6m", "6-<12m"), IRR.est*IR.est[AGEGR=="0-<60m"], IR.est),
    IR.lci = ifelse(AGEGR %in% c("0-<6m", "6-<12m"), IRR.lci*IR.lci[AGEGR=="0-<60m"], IR.lci),
    IR.uci = ifelse(AGEGR %in% c("0-<6m", "6-<12m"), IRR.uci*IR.uci[AGEGR=="0-<60m"], IR.uci)
  )
# 每个国家的发病数N
RF.res.impute$N.est <- RF.res.impute$pop * RF.res.impute$IR.est
RF.res.impute$N.lci <- RF.res.impute$pop * RF.res.impute$IR.lci
RF.res.impute$N.uci <- RF.res.impute$pop * RF.res.impute$IR.uci

# 4.作差估计每个国家12-<60m的发病数和发病率
RF.res.impute <- RF.res.impute %>%
  group_by(ISOCountry) %>%
  mutate(
    N.est = ifelse(AGEGR == "12-<60m", N.est[AGEGR=="0-<60m"] - N.est[AGEGR=="0-<6m"] - N.est[AGEGR=="6-<12m"], N.est),
    N.lci = ifelse(AGEGR == "12-<60m", N.lci[AGEGR=="0-<60m"] - N.lci[AGEGR=="0-<6m"] - N.lci[AGEGR=="6-<12m"], N.lci),
    N.uci = ifelse(AGEGR == "12-<60m", N.uci[AGEGR=="0-<60m"] - N.uci[AGEGR=="0-<6m"] - N.uci[AGEGR=="6-<12m"], N.uci),
    IR.est = ifelse(AGEGR == "12-<60m", N.est[AGEGR=="12-<60m"] / pop[AGEGR=="12-<60m"], IR.est),
    IR.lci = ifelse(AGEGR == "12-<60m", N.lci[AGEGR=="12-<60m"] / pop[AGEGR=="12-<60m"], IR.lci),
    IR.uci = ifelse(AGEGR == "12-<60m", N.uci[AGEGR=="12-<60m"] / pop[AGEGR=="12-<60m"], IR.uci),
    N.est = ifelse(AGEGR == "0-<12m", N.est[AGEGR=="0-<6m"] + N.est[AGEGR=="6-<12m"], N.est),
    N.lci = ifelse(AGEGR == "0-<12m", N.lci[AGEGR=="0-<6m"] + N.lci[AGEGR=="6-<12m"], N.lci),
    N.uci = ifelse(AGEGR == "0-<12m", N.uci[AGEGR=="0-<6m"] + N.uci[AGEGR=="6-<12m"], N.uci),
    IR.est = ifelse(AGEGR == "0-<12m", N.est[AGEGR=="0-<12m"] / pop[AGEGR=="0-<12m"], IR.est),
    IR.lci = ifelse(AGEGR == "0-<12m", N.lci[AGEGR=="0-<12m"] / pop[AGEGR=="0-<12m"], IR.lci),
    IR.uci = ifelse(AGEGR == "0-<12m", N.uci[AGEGR=="0-<12m"] / pop[AGEGR=="0-<12m"], IR.uci),
  ) %>%
  ungroup()

# # 5.将结果整理成Table
# RF.tab <- left_join(RF.new[c("ISOCountry", "CountryName")], RF.res.impute)
# RF.tab <- RF.tab[,c("Income2019", "CountryName", "ISOCountry", "AGEGR", 
#                     "IR.est", "IR.lci", "IR.uci","N.est", "N.lci", "N.uci")]
# # Incidence rate, per 1000 children per year 
# RF.tab$IR <- with(RF.tab, paste(format(round(IR.est,1), nsmall=1,trim = TRUE), # 保留1位小数（整数也会补.0），format对齐字符串宽度
#                                 " (",
#                                 format(round(IR.lci,1), nsmall=1,trim = TRUE),
#                                 "-",
#                                 format(round(IR.uci,1), nsmall=1,trim = TRUE),
#                                 ")", sep = ""))
# # Number of episodes
# RF.tab$N <- with(
#   RF.tab,
#   paste0(
#     formatC(round(N.est, 0), format = "f", digits = 0, big.mark = ","),
#     " (",
#     formatC(round(N.lci, 0), format = "f", digits = 0, big.mark = ","),
#     "-",
#     formatC(round(N.uci, 0), format = "f", digits = 0, big.mark = ","),
#     ")"
#   )
# )
# RF.tab <- RF.tab[c("Income2019", "CountryName", "AGEGR", "IR", "N")]
# write.csv(RF.tab, file = "Tables/com_inc_RSV-ALRI_ByCountry.csv")

export(RF.res.impute,'../rda/RF.res.impute.rds')

# Method 2----
RF.minimal %>%
  group_by(Income2019) %>%
  summarise(sum_RR_weighted = sum(RR_weighted, na.rm = TRUE), 
            .groups = "drop")

df_sum_RR_weighted<-RF.minimal_1000 %>%
  purrr::imap_dfr(~{
    .x %>%
      group_by(Income2019) %>%
      summarise(sum_RR_weighted = sum(RR_weighted, na.rm = TRUE), 
                .groups = "drop") %>%
      mutate(index=.y)
  })

set.seed(20160317)
RF.baseInc_1000<-com_rate_ALRI_Income.meta %>%
  filter(AGEGR == "0-<60m", Impute == 1, Group != "LMIC") %>%
  dplyr::select(AGEGR,Group,est,se,starts_with('IR.')) %>%
  mutate(data=pmap(list(est,se,Group),~{
    tibble(index=1:1000,IR=exp(rnorm(1000,..1,..2))*1000,Income2019=..3) %>%
      left_join(
        df_sum_RR_weighted,
        by = c("Income2019","index")
      ) %>%
      mutate(IR_base=IR/sum_RR_weighted)
  })) %>%
  mutate(q500=map_dbl(data,~quantile(.x$IR,0.5)),
         q025=map_dbl(data,~quantile(.x$IR,0.025)),
         q975=map_dbl(data,~quantile(.x$IR,0.975)))

RF.baseInc2<-RF.baseInc_1000 %>%
  dplyr::select(AGEGR,Group,data) %>%
  unnest(data) %>%
  select(Group,index,IR_base)


RF.res2<-RF.minimal_1000 %>%
  imap_dfr(~{
    .x[c("Income2019", "ISOCountry", "pop_U5", "RR")] %>%
      mutate(index=.y)
  }) %>%
  left_join(RF.baseInc2, by = c("Income2019" = "Group","index"="index")) %>%
  mutate(IR=IR_base*RR,N=pop_U5*IR)


RF.res.impute2 <- RF.res2 %>% 
  left_join(RF.new[c("ISOCountry", "pop_0006", "pop_0612", "pop_1260")]) %>% 
  mutate(pop_0012=pop_0006+pop_0612) %>%
  pivot_longer(cols = starts_with("pop_"),
               names_to = "AGEGR",
               values_to = "pop") %>%
  mutate(AGEGR = case_when(
    AGEGR == "pop_U5"   ~ "0-<60m",
    AGEGR == "pop_0006" ~ "0-<6m",
    AGEGR == "pop_0612" ~ "6-<12m",
    AGEGR == "pop_1260" ~ "12-<60m",
    AGEGR == "pop_0012" ~ "0-<12m",
    TRUE ~ AGEGR)
  ) %>%
  rowwise() %>%
  mutate(
    IR = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, IR),
    N = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "12-<60m","0-<12m"), NA, N)
  ) %>%
  ungroup()

RF.res.impute2 <- RF.res.impute2 %>%
  left_join(com_rate_ALRI_Income.impute, by = c("Income2019" = "Group", "AGEGR")) %>%
  group_by(ISOCountry,index) %>%
  mutate(
    IR = ifelse(AGEGR %in% c("0-<6m", "6-<12m"), IRR.est*IR[AGEGR=="0-<60m"], IR),
    N=pop*IR
  )

RF.res.impute2 <- RF.res.impute2 %>%
  group_by(ISOCountry,index) %>%
  mutate(
    N = ifelse(AGEGR == "12-<60m", N[AGEGR=="0-<60m"] - N[AGEGR=="0-<6m"] - N[AGEGR=="6-<12m"], N),
    IR = ifelse(AGEGR == "12-<60m", N[AGEGR=="12-<60m"] / pop[AGEGR=="12-<60m"], IR),
    N = ifelse(AGEGR == "0-<12m", N[AGEGR=="0-<6m"] + N[AGEGR=="6-<12m"], N),
    IR = ifelse(AGEGR == "0-<12m", N[AGEGR=="0-<12m"] / pop[AGEGR=="0-<12m"], IR)
  ) %>%
  ungroup()

export(RF.res.impute2,'../rda/RF.res.impute2.rds')

RF.res.impute2.sum <- RF.res.impute2 %>%
  reframe(
    across(
      c(IR, N), 
      list(
        lower  = ~quantile(.x, 0.025),
        median = ~quantile(.x, 0.5),
        upper  = ~quantile(.x, 0.975)
      ),
      .names = "{.col}.{.fn}"  # 这里的 {.col} 是原列名，{.fn} 是列表里的名字
    ),
    .by = c(ISOCountry, Income2019, AGEGR)
  )

RF.res.impute2 %>%
  group_by(Income2019,index,AGEGR) %>%
  mutate(share=N/sum(N)) %>%
  ungroup() %>%
  filter(index==1,AGEGR=='0-<60m')

RF.res %>% 
  group_by(Income2019) %>%
  mutate(share=N.est/sum(N.est)) %>%
  ungroup()
