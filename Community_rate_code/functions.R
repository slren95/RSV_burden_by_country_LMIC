# Generate Incidence Rate Ratio to impute data for 0-<60m
genComImputeTab <- function(df) {
  fit1 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(ALRI_N)],
                                             SID[AGEGR == "0-<12m" & !is.na(ALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<12m")), c("SID", "AGEGR", "ALRI_Deno", "ALRI_N")
                   ] %>% pivot_wider(names_from =AGEGR, values_from = c(ALRI_Deno, ALRI_N)),
                   x1i = `ALRI_N_0-<60m`, x2i=`ALRI_N_0-<12m`, 
                   t1i = `ALRI_Deno_0-<60m`, t2i = `ALRI_Deno_0-<12m`)
  fit2 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(ALRI_N)],
                                             SID[AGEGR == "0-<24m" & !is.na(ALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<24m")), c("SID", "AGEGR", "ALRI_Deno", "ALRI_N")
                   ] %>% pivot_wider(names_from =AGEGR, values_from = c(ALRI_Deno, ALRI_N)),
                   x1i = `ALRI_N_0-<60m`, x2i=`ALRI_N_0-<24m`, 
                   t1i = `ALRI_Deno_0-<60m`, t2i = `ALRI_Deno_0-<24m`)
  fit3 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(ALRI_N)],
                                             SID[AGEGR == "0-<36m" & !is.na(ALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<36m")), c("SID", "AGEGR", "ALRI_Deno", "ALRI_N")
                   ] %>% pivot_wider(names_from =AGEGR, values_from = c(ALRI_Deno, ALRI_N)),
                   x1i = `ALRI_N_0-<60m`, x2i=`ALRI_N_0-<36m`, 
                   t1i = `ALRI_Deno_0-<60m`, t2i = `ALRI_Deno_0-<36m`)
  res.df <- data.frame(
    AGEGR = c("0-<12m", "0-<24m", "0-<36m"),
    est = unlist(list(fit1$b, fit2$b, fit3$b)),
    se = unlist(list(fit1$se, fit2$se, fit3$se)),
    deno.p = c(5, 5/2, 5/3)
  )
  res.df$IRR.est <- exp(res.df$est)
  res.df$IRR.lci <- exp(res.df$est - 1.96*res.df$se)
  res.df$IRR.uci <- exp(res.df$est + 1.96*res.df$se)
  return(res.df)
}

# Use IRR to impute data for 0-<60m in each study
# generate 100 samples based on the log-normal distribution of the IRR
genComImputeEach <- function(df.each) {
  suppressMessages({
    ref <- left_join(df.each[nrow(df.each),], com_rate.impute)
    impute.res <- left_join(ref[rep(1,N.imputation),
                                c("SID", "UNICEF", "WHO", "Setting0", "StudyMY", "Income", "Dev", "Indigenous", 
                                  "ViralTest", "studyLabel", "QA_all","USID")],
                            df.each[0,])
  })
  impute.res$AGEGR <- "0-<60m"
  impute.res$Impute <- 1:N.imputation
  impute.res$ALRI_Deno <- ref$ALRI_Deno * ref$deno.p
  set.seed(6920)
  impute.res$ALRI_N <- round(exp(rnorm(N.imputation, mean = ref$est, sd = ref$se)) *  # IRR
                               ref$ALRI_N/ref$ALRI_Deno *  # IR of ref
                               impute.res$ALRI_Deno,0)
  return(impute.res)
}
genHosImputeEach <- function(df.each) {
  suppressMessages({
    ref <- left_join(df.each[nrow(df.each),], hos_rate.impute)
    impute.res <- left_join(ref[rep(1,N.imputation),
                                c("SID", "UNICEF", "WHO", "Setting0", "StudyMY", "Income", "Dev", "Indigenous", 
                                  "ViralTest", "QA_all")],
                            df.each[0,])
  })
  impute.res$AGEGR <- "0-<60m"
  impute.res$Impute <- 1:N.imputation
  impute.res$HosALRI_Deno <- ref$HosALRI_Deno * ref$deno.p
  set.seed(6920)
  impute.res$HosALRI_N <- round(exp(rnorm(N.imputation, mean = ref$est, sd = ref$se)) *  # IRR
                               ref$HosALRI_N/ref$HosALRI_Deno *  # IR of ref
                               impute.res$HosALRI_Deno,0)
  return(impute.res)
}

# Generate log incidence rate and standard error
genINC <- function(case, deno) {
  est <- log(case/deno)
  se <- sqrt(1/case- 1/deno)
  return(c(est = est, se = se))
}
genMetaRateEach <- function(df, prefix, varToKeep = NULL, rate.adjust = 1000) {
  # 如果输入df为空，直接返回全NA结果
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      I2 = NA,
      IR.est = NA,
      IR.lci = NA,
      IR.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  # 去掉数值缺失的行
  df <- df[!(is.na(df[[paste(prefix, "_N", sep = "")]])|is.na(df[[paste(prefix, "_Deno", sep = "")]])),]
  # 对分母和分子四舍五入
  df[paste(prefix, "_Deno", sep = "")] <- round(df[paste(prefix, "_Deno", sep = "")],0)
  df[paste(prefix, "_N", sep = "")] <- round(df[paste(prefix, "_N", sep = "")],0)
  # 如果清洗后无数据行，返回全NA
  if(nrow(df)==0){
    res.df <- data.frame(est = NA, se = NA, n.all = 0, n.impute = NA,
                         I2 = NA, IR.est = NA, IR.lci = NA, IR.uci = NA)
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  # 只有一条数据时，直接用genINC计算
  if(nrow(df) ==1) {
    temp <- genINC(df[[paste(prefix, "_N", sep = "")]], 
                   df[[paste(prefix, "_Deno", sep = "")]])
    res.df <- data.frame(
      est = temp[1],
      se = temp[2],
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0),
      I2 = NA
    )
  }else{
    # 多条数据时，用rma.glmm做随机效应meta分析
    fit <- rma.glmm(measure = "IRLN", data = df,   # 指定指标为对数发病率
                    xi = get(paste(prefix, "_N", sep = "")),
                    ti = get(paste(prefix, "_Deno", sep = "")))
    res.df <- data.frame(
      est = as.numeric(fit$b), # 对数发病率估计值
      se = fit$se, # 标准误
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
    res.df$I2 <- formatC(fit$I2, digits=1, format="f") # 异质性指标
  }
  # 计算IR+95%CI（单位转换为每1000人）
  res.df$IR.est <- exp(res.df$est) * rate.adjust
  res.df$IR.lci <- exp(res.df$est - 1.96*res.df$se)* rate.adjust
  res.df$IR.uci <- exp(res.df$est + 1.96*res.df$se)* rate.adjust
  # 保留必要的原始变量列
  res.df <- cbind(df[1,varToKeep], res.df)
  return(res.df)
}

genMetaRateEach.Impute <- function(df, prefix,varToKeep = NULL, rate.adjust = 1000) {
  # 如果df为空，直接返回NULL
  if(is.null(df)){
    return(NULL)
  }else{
    # 初始化空数据框
    res.df <- data.frame(est = NA, se = NA, n.all = NA, n.impute = NA,
                         I2 = NA, IR.est = NA, IR.lci = NA, IR.uci = NA)
    res.df <- res.df[0,]
    # 对每次插补后的df分别计算meta估计est+se+IR+95%CI
    for(i in 1:N.imputation){
      df.each <- df[df$Impute %in% c(0,i),]
      res.df <- rbind(res.df,
                      genMetaRateEach(df.each, prefix = prefix, varToKeep = "Impute"))
    }
  }
  # 利用Rubin’s rules合并100个插补数据集得到的meta估计est+se
  res.rubins <- unlist(mi.meld(q = res.df["est"], se = res.df["se"]))
  res.rubins <- data.frame(
    est = res.rubins[1],
    se = res.rubins[2],
    n.all = res.df$n.all[1],       # 该分组总共几条数据
    n.impute = res.df$n.impute[1], # 有几条是插补得到的数据（非原始数据）
    I2 = median(as.numeric(res.df[["I2"]]))
  )
  # 加上分组依据
  res.rubins <- cbind(df[1,varToKeep], res.rubins)
  # 计算IR+95%CI（单位转换为每1000人）
  res.rubins$IR.est <- exp(res.rubins$est) * rate.adjust
  res.rubins$IR.lci <- exp(res.rubins$est - 1.96*res.rubins$se)* rate.adjust
  res.rubins$IR.uci <- exp(res.rubins$est + 1.96*res.rubins$se)* rate.adjust
  return(res.rubins)
}
genMC <- function(df, id, input.mean, input.se, transFUN, n, output.name = "value") {
  set.seed(6920)
  res <-by(df[c(id, input.mean, input.se)],
           df[id],
           function(x, n) 
             return(data.frame(
               id = x[[id]],
               index = 1:n,
               value = transFUN(rnorm(n = n,mean = x[[input.mean]], sd = x[[input.se]]))
             )),
           n = n)
  res <- do.call(rbind, res)
  row.names(res) <- NULL
  names(res) <- c(id, "index", output.name)
  return(res)
}
# 生成所有LMIC的汇总rate
genRateLMIC <- function(df, n.level, genMC = FALSE){
  df <- df[df$Group != "LMIC",]
  if(sum(!is.na(df$est))<n.level) {
    return(df)
  }else{
    rate <- genMC(df =df, id = "Group", input.mean = "est", input.se = "se", n = N.MC,transFUN = exp)
    rate <- left_join(rate, unique(df[c("Group", "Pop")]))
    rate <- rate %>% group_by(index) %>%
      dplyr::summarise(N = round(sum(value * Pop),3))
    new.df <- df[1,]
    new.df$AGEGR <- df$AGEGR[1]
    new.df$Group <- "LMIC"
    new.df$est <- NA
    new.df$se <- NA
    new.df$n.all <- sum(df$n.all)
    new.df$n.impute <- sum(df$n.impute)
    new.df$N.est <- round(median(rate$N),3)
    new.df$N.lci <- round(quantile(rate$N, 0.025),3)
    new.df$N.uci <- round(quantile(rate$N, 0.975),3)
    new.df$Pop <- sum(df$Pop)
    new.df$IR.est <- with(new.df, N.est / Pop*1000)
    new.df$IR.lci <- with(new.df, N.lci / Pop*1000)
    new.df$IR.uci <- with(new.df, N.uci / Pop*1000)
    if(genMC) {
      rate$rate <- rate$N / sum(df$Pop)*1000
      return(rate)
    }
    return(rbind(df, new.df))
  }
}
genComImputeLMIC <- function(df, genMC = FALSE) {
  # 生成1000组样本
  rate.MC <- df %>%
    group_by(AGEGR) %>%
    group_modify(~ {
      tmp <- genMC(
        df = .x,
        id = "Group",
        input.mean = "est",
        input.se = "se",
        n = N.MC,
        transFUN = exp
      )
      tmp$IR.value <- tmp$value * 1000
      tmp
    }) %>%
    ungroup()
  # 宽表，计算1000个IRR
  ratio.MC <- rate.MC %>%
    pivot_wider(id_cols = c(Group, index), names_from = AGEGR, values_from = IR.value) %>%
    mutate(
      IRR_0_6m = `0-<6m` / `0-<60m`,
      IRR_6_12m = `6-<12m` / `0-<60m`
    )
  # 长表，分组计算IRR点估计值和95%CI
  res.df <- ratio.MC %>%
    dplyr::select(Group, IRR_0_6m, IRR_6_12m) %>%
    pivot_longer(cols = starts_with("IRR_"), names_to = "AGEGR", values_to = "IRR") %>%
    mutate(AGEGR = recode(AGEGR, IRR_0_6m = "0-<6m", IRR_6_12m = "6-<12m")) %>%
    group_by(Group, AGEGR) %>%
    summarise(
      IRR.est = round(median(IRR), 3),
      IRR.lci = round(quantile(IRR, 0.025), 3),
      IRR.uci = round(quantile(IRR, 0.975), 3),
      .groups = "drop"
    )
  return(res.df)
}
genCountryRR <- function(each.df, reduction = 1.25) {
  # 生成所有可能的危险因素组合
  expand.df <- cbind(
    # risk factor：yes, no (2^6=64 combinations)
    expand.grid(PREM = c(T,F),
                NOTBF = c(T,F),
                LBW = c(T,F),
                SIB = c(T,F),
                M25 = c(T,F),
                PAS = c(T,F),
                SMOK = c(T,F),
                HIV = c(T,F)),
    # prevalence, 1-prevalence
    expand.grid(PREM_pop = c(each.df$PREM_P[1], 1-each.df$PREM_P[1]),
                NOTBF_pop = c(each.df$NOTBF_P[1], 1-each.df$NOTBF_P[1]),
                LBW_pop = c(each.df$LBW_P[1], 1-each.df$LBW_P[1]),
                SIB_pop = c(each.df$SIB_P[1], 1-each.df$SIB_P[1]),
                M25_pop = c(each.df$M25_P[1], 1-each.df$M25_P[1]),
                PAS_pop = c(each.df$PAS_P[1], 1-each.df$PAS_P[1]),
                SMOK_pop = c(each.df$SMOK_P[1], 1-each.df$SMOK_P[1]),
                HIV_pop = c(each.df$HIV_P[1], 1-each.df$HIV_P[1])),
    # OR
    expand.grid(PREM_OR = c(RF_OR[1], 1),
                NOTBF_OR = c(RF_OR[2], 1),
                LBW_OR = c(RF_OR[3], 1),
                SIB_OR = c(RF_OR[4], 1),
                M25_OR = c(RF_OR[5], 1),
                PAS_OR = c(RF_OR[6], 1),
                SMOK_OR = c(RF_OR[7], 1),
                HIV_OR = c(RF_OR[8], 1))
  )
  # 计算每种组合的危险因素个数n
  expand.df$N.RF <- with(expand.df, PREM + NOTBF + LBW + SIB + M25 + PAS + SMOK + HIV)
  # Combined OR = 所有危险因素的OR乘积 ÷ 1.25^(n-1)
  expand.df$OR <- with(expand.df, PREM_OR*NOTBF_OR*LBW_OR*SIB_OR*M25_OR*PAS_OR*SMOK_OR*HIV_OR)/(reduction^(expand.df$N.RF-1))
  # Combined prevalence
  expand.df$pop <- with(expand.df, PREM_pop*NOTBF_pop*LBW_pop*SIB_pop*M25_pop*PAS_pop*SMOK_pop*HIV_pop)
  # 计算每种组合的RR
  expand.df$RR <- expand.df$OR * expand.df$pop
  # 汇总所有组合的RR，作为该国家整体的RR
  each.df$RR <- sum(expand.df$RR)
  return(each.df)
}

# added function shalom ----
genHosImputeTab <- function(df) {
  fit1 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)],
                                             SID[AGEGR == "0-<12m" & !is.na(HosALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<12m")), c("SID", "AGEGR", "HosALRI_Deno", "HosALRI_N")
                   ] %>% pivot_wider(names_from =AGEGR, values_from = c(HosALRI_Deno, HosALRI_N)),
                   x1i = `HosALRI_N_0-<60m`, x2i=`HosALRI_N_0-<12m`, 
                   t1i = `HosALRI_Deno_0-<60m`, t2i = `HosALRI_Deno_0-<12m`)
  fit2 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)],
                                             SID[AGEGR == "0-<24m" & !is.na(HosALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<24m")), c("SID", "AGEGR", "HosALRI_Deno", "HosALRI_N")
                   ] %>% pivot_wider(names_from =AGEGR, values_from = c(HosALRI_Deno, HosALRI_N)),
                   x1i = `HosALRI_N_0-<60m`, x2i=`HosALRI_N_0-<24m`, 
                   t1i = `HosALRI_Deno_0-<60m`, t2i = `HosALRI_Deno_0-<24m`)
  fit3 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)],
                                             SID[AGEGR == "0-<36m" & !is.na(HosALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<36m")), c("SID", "AGEGR", "HosALRI_Deno", "HosALRI_N")
                   ] %>% pivot_wider(names_from =AGEGR, values_from = c(HosALRI_Deno, HosALRI_N)),
                   x1i = `HosALRI_N_0-<60m`, x2i=`HosALRI_N_0-<36m`, 
                   t1i = `HosALRI_Deno_0-<60m`, t2i = `HosALRI_Deno_0-<36m`)
  res.df <- data.frame(
    AGEGR = c("0-<12m", "0-<24m", "0-<36m"),
    est = unlist(list(fit1$b, fit2$b, fit3$b)),
    se = unlist(list(fit1$se, fit2$se, fit3$se)),
    deno.p = c(5, 5/2, 5/3)
  )
  res.df$IRR.est <- exp(res.df$est)
  res.df$IRR.lci <- exp(res.df$est - 1.96*res.df$se)
  res.df$IRR.uci <- exp(res.df$est + 1.96*res.df$se)
  return(res.df)
}


genCountryRR_new <- function(each.df, reduction = 1.25) {
  # 生成所有可能的危险因素组合
  expand.df <- cbind(
    # risk factor：yes, no (2^6=64 combinations)
    expand.grid(PREM = c(T,F),
                NOTBF = c(T,F),
                LBW = c(T,F),
                SIB = c(T,F),
                M25 = c(T,F),
                PAS = c(T,F),
                SMOK = c(T,F),
                HIV = c(T,F)),
    # prevalence, 1-prevalence
    expand.grid(PREM_pop = c(each.df$PREM_P[1], 1-each.df$PREM_P[1]),
                NOTBF_pop = c(each.df$NOTBF_P[1], 1-each.df$NOTBF_P[1]),
                LBW_pop = c(each.df$LBW_P[1], 1-each.df$LBW_P[1]),
                SIB_pop = c(each.df$SIB_P[1], 1-each.df$SIB_P[1]),
                M25_pop = c(each.df$M25_P[1], 1-each.df$M25_P[1]),
                PAS_pop = c(each.df$PAS_P[1], 1-each.df$PAS_P[1]),
                SMOK_pop = c(each.df$SMOK_P[1], 1-each.df$SMOK_P[1]),
                HIV_pop = c(each.df$HIV_P[1], 1-each.df$HIV_P[1])),
    # OR from sampling
    expand.grid(PREM_OR = c(exp(rnorm(1,RF_OR_new[1,'est'],RF_OR_new[1,'se'])), 1),
                NOTBF_OR = c(exp(rnorm(1,RF_OR_new[2,'est'],RF_OR_new[2,'se'])), 1),
                LBW_OR = c(exp(rnorm(1,RF_OR_new[3,'est'],RF_OR_new[3,'se'])), 1),
                SIB_OR = c(exp(rnorm(1,RF_OR_new[4,'est'],RF_OR_new[4,'se'])), 1),
                M25_OR = c(exp(rnorm(1,RF_OR_new[5,'est'],RF_OR_new[5,'se'])), 1),
                PAS_OR = c(exp(rnorm(1,RF_OR_new[6,'est'],RF_OR_new[6,'se'])), 1),
                SMOK_OR = c(exp(rnorm(1,RF_OR_new[7,'est'],RF_OR_new[7,'se'])), 1),
                HIV_OR = c(exp(rnorm(1,RF_OR_new[8,'est'],RF_OR_new[8,'se'])), 1))
  )
  # 计算每种组合的危险因素个数n
  expand.df$N.RF <- with(expand.df, PREM + NOTBF + LBW + SIB + M25 + PAS + SMOK + HIV)
  # Combined OR = 所有危险因素的OR乘积 ÷ 1.25^(n-1)
  expand.df$OR <- with(expand.df, PREM_OR*NOTBF_OR*LBW_OR*SIB_OR*M25_OR*PAS_OR*SMOK_OR*HIV_OR)/(reduction^(expand.df$N.RF-1))
  # Combined prevalence
  expand.df$pop <- with(expand.df, PREM_pop*NOTBF_pop*LBW_pop*SIB_pop*M25_pop*PAS_pop*SMOK_pop*HIV_pop)
  # 计算每种组合的RR
  expand.df$RR <- expand.df$OR * expand.df$pop
  # 汇总所有组合的RR，作为该国家整体的RR
  each.df$RR <- sum(expand.df$RR)
  return(each.df)
}