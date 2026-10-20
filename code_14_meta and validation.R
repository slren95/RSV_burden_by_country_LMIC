rm(list=ls())
library(tidyverse)
library(rio)
library(ggsci)
library(metafor)
library(patchwork)
library(ggh4x)
library(janitor)
library(flextable)
library(psych)
library(survinger)
library(cowplot)
library(magrittr)

source('functions.R')


# Functions ----

recover_ir <- function(est, lci=NULL, uci=NULL, xi=NULL, ti=NULL){
  # 都有
  if(!is.na(xi) && !is.na(ti)){
    return(list(xi=xi, ti=ti, source="original"))
  }
  # 已知 xi
  if(!is.na(xi)){
    ti <- xi / est
    return(list(xi=xi, ti=ti, source="from_xi"))
  }
  # 已知 ti
  if(!is.na(ti)){
    xi <- round(est * ti)
    return(list(xi=xi, ti=ti, source="from_ti"))
  }
  # 用CI恢复
  if(!is.na(lci) && !is.na(uci)){
    se <- (log(uci) - log(lci)) / (2*1.96)
    xi <- round(1 / se^2)
    ti <- xi / est
    return(list(xi=xi, ti=ti, source="from_ci"))
  }
  stop("Insufficient information")
}

# LMIC country ----

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

# Validation Study ----
## new collected
df_valid.4<-import('rda/df_valid.4.rds',trust=T) %>% rownames_to_column(var = 'rowname3') %>%
  rowwise() %>%
  mutate(
    rec = list(
      recover_ir(est = valid_est/1000,
                 lci = valid_lci/1000,
                 uci = valid_uci/1000,
                 xi  = Numerator,
                 ti  = Denominator
      )
    ),
    xi = rec$xi,ti = round(rec$ti),xiti_source = rec$source,.after=Denominator
  ) %>%
  ungroup() %>%
  select(-rec)


df_valid_com2<-import('rda/df_valid_com2.rds',trust=T) %>% rownames_to_column(var = 'rowname1')
df_valid_hos2<-import('rda/df_valid_hos2.rds',trust=T) %>% filter(ISOCountry!='USA') %>% rownames_to_column(var = 'rowname2')

## PRISMA Count ----
df_valid_com2 %>% distinct(SID)

df_valid_hos2 %>% distinct(SID)

unique(c(df_valid_com2 %>% pull(SID),df_valid_hos2 %>% pull(SID)))

df_valid.4 %>% count(metric)

#myvenn(names(df_valid.4),names(df_valid_com2))

df_valid_all<-bind_rows(
  df_valid_com2 %>% transmute(from='internal',rowname1,SID,ISOCountry,Country,Location=Location0.x,MultiCenter,StudyPeriod,Author0=Author0.x,PubYear=as.integer(PubYear),Title=Title.x,
                              AGEGR,metric='inc',Numerator=ALRI_N,Denominator=ALRI_Deno,valid_est=est,valid_lci=lci,valid_uci=uci),
  df_valid_hos2 %>% transmute(from='internal',rowname2,SID,ISOCountry,Country,Location=Location0.x,MultiCenter,StudyPeriod,Author0=Author0.x,PubYear=as.integer(PubYear),Title=Title.x,
                              AGEGR,metric='hos',Numerator=HosALRI_N,Denominator=HosALRI_Deno,valid_est=est,valid_lci=lci,valid_uci=uci),
  df_valid.4 %>% transmute(from='external',rowname3,SID,ISOCountry,Country=CountryName,Location=Region,MultiCenter,StudyPeriod,Author0,PubYear,Title,
                              AGEGR,metric,Numerator=xi,Denominator=ti,valid_est,valid_lci,valid_uci)
) %>%
  filter(Numerator>0,Denominator>0) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,Income2019))

df_valid_all %>%
  filter(from=='internal',!is.na(PubYear)) %>%
  distinct(SID,.keep_all = T) %>% pull(SID)

c(
  "L200", "L275", "T015", "T080", "T143", "T162", "T192",
  "L088", "L157", "L165", "L248", "T001", "T052", "T063",
  "T091", "T145", "T163"
)


# 1️⃣Main validation analysis ----

df_valid.main<-df_valid_all %>%
  relocate(metric,AGEGR,Country) %>%
  arrange(metric,AGEGR,Country) %>%
  group_split(metric,AGEGR,Country) %>%
  map_dfr(~{
    df<-.
    if(df$metric[1] %in% c('mort.att','mort.ass')){
      return(df %>% mutate(valid_type='single'))
    }
    if(any(df$MultiCenter==1)){
      return(df %>% filter(MultiCenter==1) %>% mutate(valid_type='single') %>% add_count(MultiCenter,name='single_multi_n') %>%
               slice_max(Numerator))
    }
    if(all(df$MultiCenter==0) & nrow(df)>1) {
      if(length(unique(df$Location))>1)
      return(df  %>% mutate(valid_type='meta'))
    }
  }) %>%
  mutate(gid= paste(Country, AGEGR, metric,valid_type,sep = "_"))

# NEW ADDED remove duplicate study
export(df_valid.main,'validation/df_valid.main.xlsx')
df_valid.main_copy<-import('validation/df_valid.main - 副本.xlsx') %>%
  mutate(uid=paste(Country, AGEGR, metric,SID,sep = "_"))

duplicated(df_valid.main_copy$uid)

# replace with no duplicate study
df_valid.main<-df_valid.main_copy %>%
  filter(REMOVE_DUE_TO_DUP==0) %>%
  group_split(metric,AGEGR,Country) %>%
  map_dfr(~{
    df<-.
    if(df$metric[1] %in% c('mort.att','mort.ass')){
      return(df)
    }
    if(any(df$MultiCenter==1)){
      return(df)
    }
    if(all(df$MultiCenter==0) & nrow(df)>1) {
      if(length(unique(df$Location))>1)
        return(df)
    }
  })


#df_valid.main %>% filter(single_multi_n>1) %>% view()

df_main_split<-split(df_valid.main,df_valid.main[,'gid'],drop = T)


## Meta ----

df_meta_main<-df_valid.main %>%
  filter(valid_type == "meta") %>% distinct(gid) %>%
  mutate(es=map(gid,~{
    df<-df_main_split[[.x]]
    # rma.glmm(
    #   measure = "IRLN",
    #   xi = Numerator,
    #   ti = Denominator,
    #   data = df
    # )
    es <- escalc(
      measure = "IRLN",
      xi = Numerator,
      ti = Denominator,
      data = df
    ) %>%
      mutate(lci=yi-1.96*sqrt(vi),uci=yi+1.96*sqrt(vi))
    es
  }),
  fits=map(es,~{
    rma(yi, vi, data = ., method = "REML")
  }),
  df_weight=map(fits,~{
    data.frame(weights=weights(.x))
  }),
  df_plot=map2(gid,es,~{
    df_main_split[[.x]] %>%
      select(SID,Country,Location,from,StudyPeriod,Author0,PubYear,from,ISOCountry,AGEGR) %>%
      left_join(.y %>% select(SID,yi,vi,lci,uci))
  }),
  df_meta=map(fits,~{
    predict(.x)
  })
  )

### Component of forest plot ----
forest(df_meta_main$fits[[1]])

df_meta_main$fits[[1]]
escalc(
  measure = "IRLN",
  xi = Numerator,
  ti = Denominator,
  data =data.frame(Numerator=c(2,2),Denominator=c(66,65))
) %>%
  mutate(lci=yi-1.96*sqrt(vi),uci=yi+1.96*sqrt(vi))

forest(df_meta_main$fits[[1]],transf = function(x) exp(x) * 1000)

predict(df_meta_main$fits[[1]])

weights(df_meta_main$fits[[1]])

df_meta_plot_main<-pmap_dfr(df_meta_main,function(gid, es, fits, df_weight, df_plot, df_meta){
  df_plot %>%
    mutate(gid=gid,
           weight=df_weight[[1]]) %>%
    bind_rows(tibble(gid=gid,yi=df_meta$pred,lci=df_meta$ci.lb,uci=df_meta$ci.ub)) %>%
    mutate(row=max(row_number())-row_number()+1)
}) %>%
  separate(gid,sep='_',into=c('Country', 'AGEGR', 'metric','valid_type'),remove = F) %>%
  mutate(across(c(yi,lci,uci),~{
    exp(.x)*if_else(metric %in% c('mort.att','motrt.ass'),10000,1000)
  }),
  study_label = case_when(
    is.na(SID) ~ "Meta estimates (main)",
    T~sprintf('%s, %s%s',Author0,ifelse(is.na(PubYear),'RSV GEN',PubYear),ifelse(from=='internal','*',''))
  ),
  effect_label = sprintf("%.1f [%.1f, %.1f]", yi, lci, uci),
  weight_label = ifelse(is.na(weight), "", sprintf("%.1f%%", weight)),
  is_pooled = is.na(SID)) %>%
  group_by(gid) %>%
  mutate(
    max = max(pmax(yi, lci, uci), na.rm = TRUE)
    # diamond_x = ifelse(is_pooled, list(c(lci, yi, uci, yi)), list(NA)),
    # diamond_y = ifelse(is_pooled, list(c(row, row + 0.2, row, row - 0.2)), list(NA))
  ) %>%
  ungroup() %>%
  fill(ISOCountry,.direction = 'down')

names(df_meta_plot_main)

head(df_meta_plot_main)

## inc ----
plots_inc<-df_meta_plot_main %>%
  filter(metric=='inc') %>%
  count(AGEGR,Country) %>%
  pmap(function(AGEGR,Country,n){
    gid_val <- sprintf('%s_%s_%s_meta', Country, AGEGR, 'inc')
    
    print(gid_val)
    df_plot<-df_meta_plot_main %>%
      #filter(gid == sprintf('%s_%s_%s_meta',Country,AGEGR,'inc'))
      filter(gid == gid_val) %>%
      mutate(Country_AGEGR=sprintf('%s, %s',Country,AGEGR))
    
    x_range <- range(df_plot$yi, na.rm = TRUE)
    y_max <- max(df_plot$row, na.rm = TRUE)
    
    ggplot(df_plot, aes(x = yi, y = row)) +
      geom_point(data = ~subset(., !is_pooled),
                 aes(size = sqrt(weight)), shape = 15, color = "black") +
      geom_errorbarh(data = ~subset(., !is_pooled),
                     aes(xmin = lci, xmax = uci), height = 0.15, color = "black", size = 0.5) +
      geom_point(data = ~subset(., is_pooled),
                 shape = 18, size = 3, color = "red") +
      geom_errorbarh(data = ~subset(., is_pooled),
                     aes(xmin = lci, xmax = uci), height = 0.15, color = "red", size = 0.5) +
      scale_x_continuous(name = "RSV-asscoiated ALRI incidence rate (per 1000 person-years)") +
      scale_y_continuous(
        breaks = df_plot$row,
        labels = df_plot$study_label,
        name = ""
      ) +
      scale_size_continuous(range = c(1, 4), guide = "none") +
      theme_bw() +
      theme(
        panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
      )+
      facet_wrap(~Country_AGEGR)
  })

plots_inc[c(2:7,1)] %>%
  wrap_plots(nrow = 3, guides = "collect",axes = 'collect_x') &
  theme_bw() 

ggsave('plot/Meta_main_inc.tiff', width = 12,height = 6,dpi = 300)


## hos ----
plots_hos<-df_meta_plot_main %>%
  filter(metric=='hos') %>%
  count(AGEGR,Country) %>%
  pmap(function(AGEGR,Country,n){
    gid_val <- sprintf('%s_%s_%s_meta', Country, AGEGR, 'hos')
    
    print(gid_val)
    df_plot<-df_meta_plot_main %>%
      #filter(gid == sprintf('%s_%s_%s_meta',Country,AGEGR,'inc'))
      filter(gid == gid_val) %>%
      mutate(Country_AGEGR=sprintf('%s, %s',Country,AGEGR))
    
    x_range <- range(df_plot$yi, na.rm = TRUE)
    y_max <- max(df_plot$row, na.rm = TRUE)
    
    ggplot(df_plot, aes(x = yi, y = row)) +
      geom_point(data = ~subset(., !is_pooled),
                 aes(size = sqrt(weight)), shape = 15, color = "black") +
      geom_errorbarh(data = ~subset(., !is_pooled),
                     aes(xmin = lci, xmax = uci), height = 0.15, color = "black", size = 0.5) +
      geom_point(data = ~subset(., is_pooled),
                 shape = 18, size = 3, color = "red") +
      geom_errorbarh(data = ~subset(., is_pooled),
                     aes(xmin = lci, xmax = uci), height = 0.15, color = "red", size = 0.5) +
      scale_x_continuous(name = "RSV-asscoiated ALRI hospital ammission rate (per 1000 person-years)") +
      scale_y_continuous(
        breaks = df_plot$row,
        labels = df_plot$study_label,
        name = ""
      ) +
      scale_size_continuous(range = c(1, 4), guide = "none") +
      theme_bw() +
      theme(
        panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
      )+
      facet_wrap(~Country_AGEGR)
  })

plots_hos[1:4] %>%
  wrap_plots(nrow = 2, guides = "collect",axes = 'collect_x') &
  theme_bw() 

ggsave('plot/Meta_main_hos.tiff', width = 12,height = 6,dpi = 300)


# 2️⃣Sensitive validatiuon analysis ----

df_valid.sens_old<-df_valid_all %>%
  filter(from!='internal') %>%
  relocate(metric,AGEGR,Country) %>%
  arrange(metric,AGEGR,Country) %>%
  group_split(metric,AGEGR,Country) %>%
  map_dfr(~{
    df<-.
    if(df$metric[1] %in% c('mort.att','mort.ass')){
      return(df %>% mutate(valid_type='single'))
    }
    if(any(df$MultiCenter==1)){
      return(df %>% filter(MultiCenter==1) %>% mutate(valid_type='single') %>% add_count(MultiCenter,name='single_multi_n') %>%
               slice_max(Numerator))
    }
    if(all(df$MultiCenter==0) & nrow(df)>1) {
      if(length(unique(df$Location))>1)
        return(df  %>% mutate(valid_type='meta'))
    }
  }) %>%
  mutate(gid= paste(Country, AGEGR, metric,valid_type,sep = "_"),
         uid=paste(Country, AGEGR, metric,SID,sep = "_"))

# remove duplicated study ----
df_valid.sens<-df_valid.sens_old %>%
  left_join(df_valid.main_copy %>% select(uid,REMOVE_DUE_TO_DUP)) %>%
  filter(REMOVE_DUE_TO_DUP==0) %>%
  group_split(metric,AGEGR,Country) %>%
  map_dfr(~{
    df<-.
    if(df$metric[1] %in% c('mort.att','mort.ass')){
      return(df)
    }
    if(any(df$MultiCenter==1)){
      return(df)
    }
    if(all(df$MultiCenter==0) & nrow(df)>1) {
      if(length(unique(df$Location))>1)
        return(df)
    }
  })

df_sens_split<-split(df_valid.sens,df_valid.sens[,'gid'],drop = T)

df_meta_sens<-df_valid.sens %>%
  filter(valid_type == "meta") %>% distinct(gid) %>%
  mutate(es=map(gid,~{
    df<-df_sens_split[[.x]]
    es <- escalc(
      measure = "IRLN",
      xi = Numerator,
      ti = Denominator,
      data = df
    ) %>%
      mutate(lci=yi-1.96*sqrt(vi),uci=yi+1.96*sqrt(vi))
    es
  }),
  fits=map(es,~{
    rma(yi, vi, data = ., method = "REML")
  }),
  df_weight=map(fits,~{
    data.frame(weights=weights(.x))
  }),
  df_plot=map2(gid,es,~{
    df_sens_split[[.x]] %>%
      select(SID,Country,Location,from,StudyPeriod,Author0,PubYear,ISOCountry,AGEGR) %>%
      left_join(.y %>% select(SID,yi,vi,lci,uci))
  }),
  df_meta=map(fits,~{
    predict(.x)
  })
  )

df_meta_plot_sens<-pmap_dfr(df_meta_sens,function(gid, es, fits, df_weight, df_plot, df_meta){
  df_plot %>%
    mutate(gid=gid,
           weight=df_weight[[1]]) %>%
    bind_rows(tibble(gid=gid,yi=df_meta$pred,lci=df_meta$ci.lb,uci=df_meta$ci.ub)) %>%
    mutate(row=max(row_number())-row_number()+1)
}) %>%
  separate(gid,sep='_',into=c('Country', 'AGEGR', 'metric','valid_type'),remove = F) %>%
  mutate(across(c(yi,lci,uci),~{
    exp(.x)*if_else(metric %in% c('mort.att','motrt.ass'),10000,1000)
  }),
  study_label = case_when(
    is.na(SID) ~ "Meta estimates (sens)",
    T~sprintf('%s, %s',Author0,ifelse(is.na(PubYear),'RSV GEN',PubYear))
  ),
  effect_label = sprintf("%.1f [%.1f, %.1f]", yi, lci, uci),
  weight_label = ifelse(is.na(weight), "", sprintf("%.1f%%", weight)),
  is_pooled = is.na(SID)) %>%
  group_by(gid) %>%
  mutate(
    max = max(pmax(yi, lci, uci), na.rm = TRUE)
    # diamond_x = ifelse(is_pooled, list(c(lci, yi, uci, yi)), list(NA)),
    # diamond_y = ifelse(is_pooled, list(c(row, row + 0.2, row, row - 0.2)), list(NA))
  ) %>%
  ungroup() %>%
  fill(ISOCountry,.direction = 'down')


df_meta_plot_sens$gid %in% df_meta_plot_main$gid 

df_meta_plot_main %>%
  filter(!from=='internal') %>%
  group_by(gid) %>%
  count(gid) %>%
  filter(n>1)

unique(df_meta_plot_sens$gid)

df_meta_plot_main %>% filter(gid %in% unique(df_meta_plot_sens$gid)) %>%
  filter(!is.na(SID),from=='external') %>% view()

df_meta_plot_sens %>% filter(!is.na(SID))

## sens plot ----
plot_sens<-df_meta_plot_sens %>%
  count(AGEGR,Country,metric) %>%
  pmap(function(AGEGR,Country,metric,n){
    
    gid_val <- sprintf('%s_%s_%s_meta', Country, AGEGR, metric)
    
    print(gid_val)
    df_plot<-df_meta_plot_sens %>%
      #filter(gid == sprintf('%s_%s_%s_meta',Country,AGEGR,'inc'))
      filter(gid == gid_val) %>%
      mutate(Country_AGEGR=sprintf('%s, %s',Country,AGEGR))
    
    name<-ifelse(metric=='inc','RSV-asscoiated ALRI incidence rate (per 1000 person-years)',
                 'RSV-asscoiated ALRI hospital admission rate (per 1000 person-years)')
    x_range <- range(df_plot$yi, na.rm = TRUE)
    y_max <- max(df_plot$row, na.rm = TRUE)
    
    ggplot(df_plot, aes(x = yi, y = row)) +
      geom_point(data = ~subset(., !is_pooled),
                 aes(size = sqrt(weight)), shape = 15, color = "black") +
      geom_errorbarh(data = ~subset(., !is_pooled),
                     aes(xmin = lci, xmax = uci), height = 0.15, color = "black", size = 0.5) +
      geom_point(data = ~subset(., is_pooled),
                 shape = 18, size = 3, color = "red") +
      geom_errorbarh(data = ~subset(., is_pooled),
                     aes(xmin = lci, xmax = uci), height = 0.15, color = "red", size = 0.5) +
      scale_x_continuous(name = name) +
      scale_y_continuous(
        breaks = df_plot$row,
        labels = df_plot$study_label,
        name = ""
      ) +
      scale_size_continuous(range = c(1, 4), guide = "none") +
      theme_bw() +
      theme(
        panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
      )+
      facet_wrap(~Country_AGEGR)
  })

# plot_sens[[1]] <- plot_sens[[1]] + labs(tag = "A")
# plot_sens[[3]] <- plot_sens[[3]] + labs(tag = "B")
# 
# left_panel <- wrap_plots(
#   plot_sens[c(1,2,4)],
#   ncol = 1,
#   guides = "collect",
#   axes = "collect_x"
# )
# 
# right_panel <- plot_sens[[3]]
# 
# (left_panel + right_panel) +
#   plot_layout(widths = c(3,1)) &
#   theme(
#     plot.tag = element_text(size = 14),
#     plot.tag.position = c(0.02, 0.98)
#   )
# ggsave('plot/Meta_sens.tiff', width = 8,height = 8,dpi = 300)

plot_sens[[1]]
ggsave('plot/Meta_sens.tiff', width = 8,height = 4,dpi = 300)

# 3️⃣Model estimate ----

df_model_inc<-import('rda/df_model_inc.rds',trust=T)
df_model_hos<-import('rda/df_model_hos.rds',trust=T)
df_model_mort.att<-import('rda/df_model_mort.att.rds',trust=T)
df_model_mort.ass<-import('rda/df_model_mort.ass.rds',trust=T)

df_model<-df_valid.main %>%
  distinct(ISOCountry,AGEGR,Income2019) %>%
  left_join(df_model_inc %>% select(ISOCountry,AGEGR,starts_with('inc_'))) %>%
  left_join(df_model_hos %>% select(ISOCountry,AGEGR,starts_with('hos_'))) %>%
  left_join(df_model_mort.att %>% select(ISOCountry,AGEGR,starts_with('mort.att_'))) %>%
  left_join(df_model_mort.ass %>% select(ISOCountry,AGEGR,starts_with('mort.ass_')))

df_model_long<-df_model %>%
  pivot_longer(cols=-c(ISOCountry,AGEGR,Income2019), names_to = c("metric", "stat"),
               names_pattern = "(.*)_(025|500|975)",
               values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value,
    names_prefix = "model_"
  ) %>%
  rename(model_est=model_500,model_lci=model_025,model_uci=model_975)

# Validation line range Plot ----
# 编写一个简易的 Interval Score 计算函数
winkler_score <- function(actual, lower, upper, alpha = 0.05) {
  width <- upper - lower
  penalty_lower <- (2 / alpha) * (lower - actual) * (actual < lower)
  penalty_upper <- (2 / alpha) * (actual - upper) * (actual > upper)
  return(width + penalty_lower + penalty_upper)
}

# 示例数据：实际外部验证集住院率 5%, 模型预测区间 [3%, 6%]
winkler_score(actual = 0.05, lower = 0.03, upper = 0.06, alpha = 0.05)

calc_overlap_coefficient <- function(L1, U1, L2, U2,log=F) {
  if(log==T){
    L1<-log(L1)
    U1<-log(U1)
    L2<-log(L2)
    U2<-log(U2)
  }
  # 1. 计算两者的区间宽度
  len_A <- U1 - L1
  len_B <- U2 - L2
  
  # 2. 计算交集的边界
  L_overlap <- max(L1, L2)
  U_overlap <- min(U1, U2)
  
  # 3. 计算分子（交集长度）
  intersection <- max(0, U_overlap - L_overlap)
  
  # 4. 计算分母（较短区间的长度）
  denominator <- min(len_A, len_B)
  
  #return(intersection / denominator)
  return(intersection / len_B)
}

# 示例：你的预测区间 [0.04, 0.08]，别人验证集区间 [0.05, 0.12]
calc_overlap_coefficient(0.04, 0.08, 0.05, 0.12,log = F) 
# 输出结果：0.75 (说明重叠了较窄区间的 75%)

calc_jaccard_index <- function(L1, U1, L2, U2) {
  # 1. 计算两者的区间宽度
  len_A <- U1 - L1
  len_B <- U2 - L2
  
  # 2. 计算交集的边界
  L_overlap <- max(L1, L2)
  U_overlap <- min(U1, U2)
  
  # 3. 计算分子（交集长度）
  intersection <- max(0, U_overlap - L_overlap)
  
  # 4. 计算分母（并集长度 = A + B - 交集）
  union_len <- len_A + len_B - intersection
  
  # 5. 返回 Jaccard 指数
  if (union_len == 0) return(0) # 防止极端情况下分母为0
  return(intersection / union_len)
}

# 示例 1：精准且一致
calc_jaccard_index(0.04, 0.08, 0.05, 0.09) 
# 输出：0.60  (交集 0.03 / 并集 0.05)

# 示例 2：故意拉宽区间（你提到的漏洞场景）
# 假设验证集是 [0.05, 0.09]，你故意预测了一个超级宽的 [0.01, 0.50]
calc_jaccard_index(0.01, 0.50, 0.05, 0.09)
# 输出：0.081 (瞬间暴跌，真面目暴露)

## main ----
df_final_main <- imap_dfr(df_main_split, ~{
  if(str_detect(.y, '_single')) return(.x)
  df_meta_plot_main %>% filter(gid == .y,is.na(SID))
}) %>%
  mutate(compare_est=coalesce(valid_est,yi),
         compare_lci=coalesce(valid_lci,lci),
         compare_uci=coalesce(valid_uci,uci)) %>%
  select(-Income2019) %>%
  left_join(df_model_long,by=c('metric','AGEGR','ISOCountry')) %>%
  mutate(overlap = pmax(compare_lci, model_lci, na.rm = TRUE) <= pmin(compare_uci, model_uci, na.rm = TRUE)) %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','0-<12m','12-<60m','0-<60m'))) %>%
  mutate(winkler_model=winkler_score(compare_est,model_lci,model_uci),
         winkler_compare=winkler_score(compare_est,compare_lci,compare_uci),
         winkler_ratio=winkler_model/winkler_compare) %>%
  rowwise() %>%
  mutate(overlap_coef=calc_overlap_coefficient(compare_lci,compare_uci,model_lci,model_uci,log=T),
         jaccard_index=calc_jaccard_index(compare_lci,compare_uci,model_lci,model_uci)) %>%
  ungroup()


df_final_main %>% select(winkler_ratio:jaccard_index) %>%
  pairs.panels(method = "pearson",
              hist.col = "#00AFBB",
              density = TRUE,
              ellipses = TRUE)

df_plot_main <- df_final_main %>% 
  select(SID,ISOCountry, Country, AGEGR, metric,valid_type,starts_with("compare_"), starts_with("model_"),overlap) %>% 
  filter(!is.na(model_est)) %>%
  mutate(valid_type2=if_else(metric %in% c('mort.att','mort.ass'),'Mortality Study',valid_type))

df_plot_main_long <- df_plot_main %>% 
  pivot_longer(c(compare_est:model_uci), names_to = c("source", ".value"), names_sep = "_") %>% 
  mutate(source = recode(source, compare = "Validation", model = "Model")) %>%
  mutate(type=case_when(source=='Model'~'Model-estimated',
                        valid_type=='single'~'Multicenter-study\nreported estimates',
                        valid_type=='meta'~'Cross-study\nmeta-estimates')) %>%
  mutate(type=factor(type,levels=c('Model-estimated','Multicenter-study\nreported estimates','Cross-study\nmeta-estimates')))

## sens ----
df_final_sens <- imap_dfr(df_sens_split, ~{
  if(str_detect(.y, '_single')) return(.x)
  df_meta_plot_sens %>% filter(gid == .y,is.na(SID))
}) %>%
  mutate(compare_est=coalesce(valid_est,yi),
         compare_lci=coalesce(valid_lci,lci),
         compare_uci=coalesce(valid_uci,uci)) %>%
  select(-Income2019) %>%
  left_join(df_model_long,by=c('metric','AGEGR','ISOCountry')) %>%
  mutate(overlap = pmax(compare_lci, model_lci, na.rm = TRUE) <= pmin(compare_uci, model_uci, na.rm = TRUE)) %>%
  mutate(winkler_model=winkler_score(compare_est,model_lci,model_uci),
         winkler_compare=winkler_score(compare_est,compare_lci,compare_uci),
         winkler_ratio=winkler_model/winkler_compare) %>%
  rowwise() %>%
  mutate(overlap_coef=calc_overlap_coefficient(compare_lci,compare_uci,model_lci,model_uci,log=T),
         jaccard_index=calc_jaccard_index(compare_lci,compare_uci,model_lci,model_uci)) %>%
  ungroup()

df_plot_sens <- df_final_sens %>% 
  select(SID,ISOCountry, Country, AGEGR, metric,valid_type,starts_with("compare_"), starts_with("model_"),overlap) %>% 
  filter(!is.na(model_est)) %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','0-<12m','12-<60m','0-<60m'))) %>%
  mutate(valid_type2=if_else(metric %in% c('mort.att','mort.ass'),'Mortality Study',valid_type))
  

df_plot_sens_long <- df_plot_sens %>% 
  pivot_longer(c(compare_est:model_uci), names_to = c("source", ".value"), names_sep = "_") %>% 
  mutate(source = recode(source, compare = "Validation", model = "Model")) %>%
  mutate(type=case_when(source=='Model'~'Model-estimated',
                        valid_type=='single'~'Multicenter-study\nreported estimates',
                        valid_type=='meta'~'Cross-study\nmeta-estimates')) %>%
  mutate(type=factor(type,levels=c('Model-estimated','Multicenter-study\nreported estimates','Cross-study\nmeta-estimates')))

# Overlap coefficient ----


summary(df_final_main$winkler_ratio)
summary(df_final_main$overlap_coef)
summary(df_final_main$jaccard_index)

summary(df_final_sens$winkler_ratio)
summary(df_final_sens$jaccard_index)
summary(df_final_sens$overlap_coef)

df_final_main %>% count(metric)

df_final_main %>%
  group_by(metric) %>%
  summarise(
    n=n(),
    median_overlap = median(overlap_coef, na.rm = TRUE),
    Q1_overlap = quantile(overlap_coef, 0.25, na.rm = TRUE),
    Q3_overlap = quantile(overlap_coef, 0.75, na.rm = TRUE),
    IQR_overlap = IQR(overlap_coef, na.rm = TRUE)
  )

# # A tibble: 4 × 6
# metric       n median_overlap Q1_overlap Q3_overlap IQR_overlap
# <chr>    <int>          <dbl>      <dbl>      <dbl>       <dbl>
#   1 hos         16          0.171     0.0507      0.412       0.362
# 2 inc         15          0.536     0.306       0.888       0.582
# 3 mort.ass     7          0.548     0.293       0.601       0.307
# 4 mort.att     1          0.484     0.484       0.484       0   

df_final_sens %>%
  group_by(metric) %>%
  summarise(
    n=n(),
    median_overlap = median(overlap_coef, na.rm = TRUE),
    Q1_overlap = quantile(overlap_coef, 0.25, na.rm = TRUE),
    Q3_overlap = quantile(overlap_coef, 0.75, na.rm = TRUE),
    IQR_overlap = IQR(overlap_coef, na.rm = TRUE)
  )

# A tibble: 4 × 6
# metric       n median_overlap Q1_overlap Q3_overlap IQR_overlap
# <chr>    <int>          <dbl>      <dbl>      <dbl>       <dbl>
#   1 hos          4          0.211     0.0637      0.515       0.452
# 2 inc          4          0.550     0.487       0.673       0.186
# 3 mort.ass     7          0.548     0.293       0.601       0.307
# 4 mort.att     1          0.484     0.484       0.484       0     


## inc ----
df_plot_main_long %>% 
  filter(metric=='inc') %>%
  mutate(overlap_factor = factor(overlap, levels = c(TRUE, FALSE))) %>%
  ggplot(aes(Country,y = est,colour = type)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),show.legend = TRUE,size=.3
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "none")+
  scale_color_lancet(name = NULL)+
  # 1. 在这里加上 drop = FALSE
  scale_linetype_discrete(
    name = NULL,
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  # 2. 在这里也加上 drop = FALSE，并补齐 values 映射
  scale_shape_manual(
    name = NULL,
    values = c("TRUE" = 19, "FALSE" = 21),
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  labs(x = NULL, y = "RSV-associated ALRI incidence rate\n(/1,000 person-years)",
       title='Main validation analysis') +
  theme_bw() +
  coord_cartesian(ylim=c(0,500))+
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_inc_main.tiff', width = 10,height = 6,dpi = 300)

df_plot_sens_long %>% 
  filter(metric=='inc') %>%
  mutate(overlap_factor = factor(overlap, levels = c(TRUE, FALSE))) %>%
  ggplot(aes(Country,y = est,colour = type)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),show.legend = TRUE,size=.3
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "none")+
  scale_color_lancet(name = NULL)+
  scale_linetype_discrete(
    name = NULL,
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  # 2. 在这里也加上 drop = FALSE，并补齐 values 映射
  scale_shape_manual(
    name = NULL,
    values = c("TRUE" = 19, "FALSE" = 21),
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  labs(x = NULL, y = "RSV-associated ALRI incidence rate\n(/1,000 person-years)",
       title='Sensitivity validation analysis') +
  coord_cartesian(ylim=c(0,400))+
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_inc_sens.tiff', width = 10,height = 6,dpi = 300)

## hos ----
df_plot_main_long %>% 
  filter(metric=='hos') %>%
  ggplot(aes(Country,y = est,colour = type)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),size=.3
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "none")+
  scale_color_lancet(name = NULL)+
  scale_linetype_discrete(
    name = NULL,
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  # 2. 在这里也加上 drop = FALSE，并补齐 values 映射
  scale_shape_manual(
    name = NULL,
    values = c("TRUE" = 19, "FALSE" = 21),
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  labs(x = NULL, y = "RSV-associated ALRI hospital admission rate\n(/1,000 person-years)",
       title='Main validation analysis') +
  coord_cartesian(ylim=c(0,60))+
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_hos_main.tiff', width = 10,height = 6,dpi = 300)

df_plot_sens_long %>% 
  filter(metric=='hos') %>%
  ggplot(aes(Country,y = est,colour = type)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),show.legend = T,size=.3
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "y")+
  scale_color_lancet(name = NULL)+
  scale_linetype_discrete(
    name = NULL,
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  # 2. 在这里也加上 drop = FALSE，并补齐 values 映射
  scale_shape_manual(
    name = NULL,
    values = c("TRUE" = 19, "FALSE" = 21),
    labels = c("Overlapping", "Non-overlapping"),
    drop = FALSE
  ) +
  labs(x = NULL, y = "RSV-associated ALRI hospital admission rate\n(/1,000 person-years)",
       title='Sensitivity validation analysis') +
  coord_cartesian(ylim=c(0,50))+
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave('plot/Valid_hos_sens.tiff', width = 10,height = 6,dpi = 300)

## mort----
p_att<-df_plot_sens_long %>% 
  filter(metric=='mort.att') %>%
  ggplot(aes(Country,y = est,colour = type)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),size=.3
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "y")+
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported'))+
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping")) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"))+
  labs(x = "Study", y = "RSV-attributable all-cause mortality rate\n(/100,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_ass<-df_plot_sens_long %>% 
  filter(metric=='mort.ass') %>%
  ggplot(aes(Country,y = est,colour = type)) +
  geom_pointrange(
    aes(ymin = lci, ymax= uci,linetype = factor(overlap,levels=c(T,F)),shape=factor(overlap,levels=c(T,F))),
    position = position_dodge(.55),size=.3
  ) +
  facet_grid2(. ~ AGEGR, scales = "free", space = "free_x", independent = "none")+
  scale_color_lancet(name = NULL,labels=c('Model-estimated','Study-reported'))+
  scale_linetype_discrete(name = NULL,labels = c("Overlapping", "Non-overlapping")) +
  scale_shape_manual(name = NULL,values=c(19,21),labels = c("Overlapping", "Non-overlapping"))+
  labs(x = "Study", y = "RSV-associated all-cause mortality rate\n(/100,000 person-years)") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_ass + p_att +
  plot_layout(widths = c(6, 2),axes = "collect_x")+
  plot_annotation(tag_levels = "A")

ggsave('plot/Valid_main_mort.tiff', width = 10,height = 6,dpi = 300)

# Diagonal plot ----

df_plot_sens %>%
  summary()

df_plot_sens %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = metric), alpha = 0.5, linewidth = 0.5) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = metric), alpha = 0.5, linewidth = 0.5) +
  geom_point(aes(color = metric, shape = Country), size = 3, alpha = 0.8) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(0.08, 500), ylim = c(0.08, 500),ratio = 1) +
  labs(x = "Published Estimates (95% CI)", y = "Model Estimates (95% CI)", color = "Metric", shape = "Metric")+
  facet_wrap(.~AGEGR)

### mort ----
df_plot_main %>%
  filter(str_detect(metric, "mort")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_point(aes(color = Country, shape = metric), size = 3, alpha = 0.8) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(0.08, 500), ylim = c(0.08, 500),ratio = 1) +
  scale_shape_discrete(labels = c(
    "mort.ass" = "RSV-associated mortality rate",
    "mort.att" = "RSV-attributable mortality rate",
    "inc" = "RSV-associated ALRI incidence",
    "hos" = "RSV-associated ALRI hospital admission rate"
  ),
  name=NULL) +
  scale_color_discrete(name=NULL)+
  labs(x = "Study reported mortality rate (/100,000 person-years)", y = "Model-estimated mortality rate\n(/100,000 person-years)", color = "Metric", shape = "Country") +
  facet_wrap(~AGEGR)

ggsave('plot/Diagonal_mort.tiff', width = 10, height = 5, dpi = 300,compression = "lzw")

### inc ----
lapply(split(df_plot_main, df_plot_main$metric), summary)


lapply(split(df_plot_sens, df_plot_sens$metric), summary)

df_plot_main %>%
  filter(str_detect(metric, "inc")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_point(aes(color = Country, shape = valid_type), size = 3, alpha = 0.8,show.legend = T) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(6, 450), ylim = c(6, 450),ratio = 1) +
  scale_shape_discrete(name=NULL,label=c('meta'='Cross-study meta-estimates','single'='Multicenter-study reported estimates'))+
  scale_color_discrete(name=NULL)+
  labs(x = "Reference RSV-associated ALRI incidence rate from meta-analyses\nor multicenter studies (/1,000 person-years)",
       y = "Model-estimated RSV-associated ALRI incidence rate\n(/1,000 person-years)", color = "Metric", shape = "Country") +
  facet_wrap(~AGEGR)+
  guides(shape = guide_legend(position = "top"))

ggsave('plot/Diagonal_main_inc.tiff', width = 10, height = 5, dpi = 300,compression = "lzw")

df_plot_sens %>%
  filter(str_detect(metric, "inc")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_point(aes(color = Country, shape = valid_type), size = 3, alpha = 0.8,show.legend = T) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(20, 400), ylim = c(20, 400),ratio = 1) +
  scale_shape_discrete(name=NULL,label=c('meta'='Cross-study meta-estimates','single'='Multicenter-study reported estimates'))+
  scale_color_discrete(name=NULL)+
  labs(x = "Reference RSV-associated ALRI incidence rate from meta-analyses\nor multicenter studies (/1,000 person-years)",
       y = "Model-estimated RSV-associated ALRI incidence rate\n(/1,000 person-years)", color = "Metric", shape = "Country") +
  facet_wrap(~AGEGR)+
  guides(shape = guide_legend(position = "top"))

ggsave('plot/Diagonal_sens_inc.tiff', width = 10, height = 5, dpi = 300,compression = "lzw")

### hos ----

df_plot_main %>%
  filter(str_detect(metric, "mort.att")) %>%
  summary()

df_plot_sens %>%
  filter(str_detect(metric, "hos")) %>%
  summary()

df_plot_main %>%
  filter(str_detect(metric, "hos")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_point(aes(color = Country, shape = valid_type), size = 3, alpha = 0.8,show.legend = T) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(0.05, 170), ylim = c(0.05, 170),ratio = 1) +
  scale_shape_discrete(name=NULL,label=c('meta'='Cross-study meta-estimates','single'='Multicenter-study reported estimates'))+
  scale_color_discrete(name=NULL)+
  labs(x = "Reference RSV-associated ALRI hospital admission rate from meta-analyses\nor multicenter studies (/1,000 person-years)",
       y = "Model-estimated RSV-associated ALRI\nhospital admission rate (/1,000 person-years)", color = "Metric", shape = "Country") +
  facet_wrap(~AGEGR)+
  guides(shape = guide_legend(position = "top"))

ggsave('plot/Diagonal_main_hos.tiff', width = 10, height = 5, dpi = 300,compression = "lzw")

df_plot_sens %>%
  filter(str_detect(metric, "hos")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5) +
  geom_point(aes(color = Country, shape = valid_type), size = 3, alpha = 0.8,show.legend = T) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(0.08, 170), ylim = c(0.08, 170),ratio = 1) +
  scale_shape_manual(name=NULL,values = c("meta" = 16, "single" = 17),
                     label=c('meta'='Cross-study meta-estimates','single'='Multicenter-study reported estimates'),
                     breaks = c("single", "meta"))+
  scale_color_discrete(name=NULL)+
  labs(x = "Reference RSV-associated ALRI hospital admission rate from meta-analyses\nor multicenter studies (/1,000 person-years)",
       y = "Model-estimated RSV-associated ALRI\nhospital admission rate (/1,000 person-years)", color = "Metric", shape = "Country") +
  facet_wrap(~AGEGR)+
  guides(shape = guide_legend(position = "top"),
         color = guide_legend(override.aes = list(shape=17)))

ggsave('plot/Diagonal_sens_hos.tiff', width = 10, height = 5, dpi = 300,compression = "lzw")

# Figure 4 diagnoal plot ----
## main ----

df_plot_main %>% count(Country)
country_colors <- c(
  "Argentina"    = "#00468B",  # 经典深蓝 (Lancet Blue)
  "India"        = "#ED0000",  # 哑光红 (Lancet Red)
  "Kenya"        = "#42B540",  # 柔和绿 (Lancet Green)
  "Peru"         = "#0099B4",  # 青蓝/湖蓝 (Lancet Cyan)
  "Philippines"  = "#925E9F",  # 典雅紫 (Lancet Purple)
  "South Africa" = "#FDAF91",  # 暖珊瑚杏 (Lancet Peach)
  "Thailand"     = "#AD002A"   # 深红/酒红 (Lancet Dark Red)
)
df_plot_main %>% count(valid_type2)

type_shapes<-c("meta" = 15, "single" = 17, "Mortality Study" = 16)
type_labels <- c("Meta-estimates", 
                 "Multicenter estimates", 
                 "Mortality estimates")

df_plot_sens %>% filter(metric %in% c('inc','hos')) %>% count(Country)
country_colors_sens<-country_colors[c(2,6)]
type_shapes_sens<-type_shapes[1:2]
type_labels_sens<-type_labels[1:2]

p1<-df_plot_main %>%
  filter(str_detect(metric, "inc")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_point(aes(color = Country, shape = valid_type2), size = 2, alpha = 0.8,show.legend = T) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(6, 450), ylim = c(6, 450),ratio = 1) +
  scale_shape_manual(name = NULL, limits = c("meta", "single", "Mortality Study"), 
                     values = type_shapes, 
                     labels = type_labels, 
                     drop = FALSE)+
  scale_color_manual(values = country_colors,name=NULL,drop=F,limits = names(country_colors))+
  labs(x = "Validation estimates",
       y = "Modeled estimates", color = "Metric", shape = "Country",
       title = 'RSV-associated ALRI incidence rate',tag = 'A') +
  facet_wrap(~AGEGR,nrow = 1)

p2<-df_plot_main %>%
  filter(str_detect(metric, "hos")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_point(aes(color = Country, shape = valid_type2), size = 2, alpha = 0.8,show.legend = T) +
  scale_x_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + scale_y_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + theme_bw() +
  coord_cartesian(xlim = c(0.05, 170), ylim = c(0.05, 170),ratio = 1) +
  scale_shape_manual(name = NULL, limits = c("meta", "single", "Mortality Study"), 
                     values = type_shapes, 
                     labels = type_labels, 
                     drop = FALSE)+
  scale_color_manual(values = country_colors,name=NULL,drop=F,limits = names(country_colors))+
  labs(x = "Validation estimates",
       y = "Modeled estimates", color = "Metric", shape = "Country",
       title = 'RSV-associated ALRI hospital admission rate',tag = 'B') +
  facet_wrap(~AGEGR,nrow=1)

p3<-df_plot_main %>%
  filter(str_detect(metric, "mort.att")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_point(aes(color = Country,shape = valid_type2), size = 2, alpha = 0.8,show.legend = T) +
  scale_x_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + scale_y_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + theme_bw() +
  coord_cartesian(xlim = c(0.08, 500), ylim = c(0.08, 500),ratio = 1) +
  scale_color_manual(values = country_colors,name=NULL,drop=F,limits = names(country_colors))+
  scale_shape_manual(name = NULL, limits = c("meta", "single", "Mortality Study"), 
                     values = type_shapes, 
                     labels = type_labels, 
                     drop = FALSE)+
  labs(x = "Validation estimates", y = "Modeled estimates",
       title = 'RSV-attributable\nall-cause mortality rate',tag = 'C') +
  facet_wrap(~AGEGR)

p4<-df_plot_main %>%
  filter(str_detect(metric, "mort.ass")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_point(aes(color = Country,shape = valid_type2), size = 2, alpha = 0.8,show.legend = T) +
  scale_x_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + scale_y_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + theme_bw() +
  coord_cartesian(xlim = c(0.08, 500), ylim = c(0.08, 500),ratio = 1) +
  scale_color_manual(values = country_colors,name=NULL,drop=F,limits = names(country_colors))+
  scale_shape_manual(name = NULL, limits = c("meta", "single", "Mortality Study"), 
                     values = type_shapes, 
                     labels = type_labels,
                     drop = FALSE)+
  labs(x = "Validation estimates", y = "Modeled estimates",
       title = 'RSV-associated all-cause mortality rate',tag = 'D') +
  facet_wrap(~AGEGR,nrow=1)

# 在 p3 和 p4 的代码末尾各加上：force_panelsizes()
u<-3.235
p1_fixed <- p1 + force_panelsizes(rows = unit(3.5, "cm"), cols = unit(3.5, "cm"))
p2_fixed <- p2 + force_panelsizes(rows = unit(3.5, "cm"), cols = unit(3.5, "cm"))
p3_fixed <- p3 + force_panelsizes(rows = unit(u, "cm"), cols = unit(u, "cm"))
p4_fixed <- p4 + force_panelsizes(rows = unit(u, "cm"), cols = unit(u, "cm"))

# 此时宽度不需要手动按 1:4 猜比例了，直接并排即可！
p3_p4_row <- p3_fixed + p4_fixed

(p1_fixed / p2_fixed / p3_p4_row)+
  plot_layout(
    guides = "collect"
  ) &
  theme_survinger(base_family = "Arial") &
  theme(
    text=element_text(family = 'Arial'),
    legend.position = "right",
    plot.tag = element_text(size=12,face='bold'),
    strip.text = element_text(size=8,face='bold',vjust = .5,margin = margin(b = 3)),
    strip.background = element_blank(),
    plot.title = element_text(size=9),
    axis.title = element_text(size=9),
    axis.text = element_text(size=8),
    plot.margin = margin()
  )

ggsave("plot/Figure_4.tiff", width = 10, height = 7, dpi = 300,compression = "lzw")

## sens ----
p1_sens<-df_plot_sens %>%
  filter(str_detect(metric, "inc")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = T) +
  geom_point(aes(color = Country, shape = valid_type2), size = 2, alpha = 0.8,show.legend = T) +
  scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(xlim = c(6, 450), ylim = c(6, 450),ratio = 1) +
  scale_shape_manual(name = NULL, limits = c("meta", "single"), 
                     values = type_shapes_sens, 
                     labels = type_labels_sens, 
                     drop = FALSE)+
  scale_color_manual(values = country_colors_sens,name=NULL,drop=F,limits = names(country_colors_sens))+
  labs(x = "Validation estimates",
       y = "Modeled estimates", color = "Metric", shape = "Country",
       title = 'RSV-associated ALRI incidence rate',tag = 'A') +
  facet_wrap(~AGEGR,nrow = 1)+
  theme(legend.position = 'right')

p2_sens<-df_plot_main %>%
  filter(str_detect(metric, "hos")) %>%
  ggplot(aes(x = compare_est, y = model_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", color = "gray50", linewidth = 1) +
  geom_linerange(aes(xmin = compare_lci, xmax = compare_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = F) +
  geom_linerange(aes(ymin = model_lci, ymax = model_uci, color = Country), alpha = 0.5, linewidth = 0.5,show.legend = F) +
  geom_point(aes(color = Country, shape = valid_type2), size = 2, alpha = 0.8,show.legend = F) +
  scale_x_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + scale_y_log10(breaks=c(0.1,1,10,100),labels=c(0.1,1,10,100)) + theme_bw() +
  coord_cartesian(xlim = c(0.05, 170), ylim = c(0.05, 170),ratio = 1) +
  scale_shape_manual(name = NULL, limits = c("meta", "single"), 
                     values = type_shapes_sens, 
                     labels = type_labels_sens, 
                     drop = FALSE)+
  scale_color_manual(values = country_colors_sens,name=NULL,drop=F,limits = names(country_colors_sens))+
  labs(x = "Validation estimates",
       y = "Modeled estimates", color = "Metric", shape = "Country",
       title = 'RSV-associated ALRI hospital admission rate',tag = 'B') +
  facet_wrap(~AGEGR,nrow=1)+
  theme(legend.position = 'null')

design_layout <- "
AAA##
BBBBB
"

# 2. 直接拼接，并应用你的主题
final_plot_sens <- (p1_sens + p2_sens) +
  plot_layout(
    design = design_layout
  ) &
  theme(
    text = element_text(family = 'Arial'),
    plot.tag = element_text(size = 12, face = 'bold'),
    strip.text = element_text(size = 8, face = 'bold', vjust = .5, margin = margin(b = 3)),
    strip.background = element_blank(),
    plot.title = element_text(size = 9,face='bold'),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

final_plot_sens

ggsave("plot/Figure_4_sens.tiff", width = 9, height = 5, dpi = 300,compression = "lzw")

# Table overlap ----

## main ----
metric_map <- c(
  inc      = "RSV-associated ALRI incidence rate",
  hos      = "RSV-associated ALRI hospital admission rate",
  mort.att = "RSV-attributable all-cause mortality rate",
  mort.ass = "RSV-associated all-cause mortality rate"
)

res_main <- df_plot_main %>%
  tabyl(AGEGR, overlap, metric) %>%
  adorn_totals("row",name = 'overall') %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(1) %>%
  adorn_ns("front")

tab_main <- bind_rows(lapply(names(metric_map), function(nm){
  
  x <- res_main[[nm]] %>%
    rename(Yes = `TRUE`, No = `FALSE`) %>%
    select(AGEGR, Yes, No)
  
  x <- x %>%
    filter(!(str_remove_all(Yes,' ') %in% c("0(0.0%)", "0(-)") &
               str_remove_all(No,' ')  %in% c("0(0.0%)", "0(-)")))
  
  if(nrow(x)==2){x<-head(x,1)}
  
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

group_idx <- which(tab_main$Age %in% metric_map)

flextable(tab_main) %>%
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
  merge_at(i = 17, j = 1:3, part = "body") %>%
  bold(i = group_idx, part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:3, align = "right", part = "body") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx(path = "docs/Valid_main_overlap.docx")

## sens ----
res_sens <- df_plot_sens %>%
  tabyl(AGEGR, overlap, metric) %>%
  adorn_totals("row",name = 'overall') %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(1) %>%
  adorn_ns("front")

tab_sens <- bind_rows(lapply(names(metric_map), function(nm){
  
  x <- res_sens[[nm]] %>%
    rename(Yes = `TRUE`, No = `FALSE`) %>%
    select(AGEGR, Yes, No)
  
  # 删除全0 / (-) 行
  x <- x %>%
    filter(!(str_remove_all(Yes,' ') %in% c("0(0.0%)", "0(-)") &
               str_remove_all(No,' ')  %in% c("0(0.0%)", "0(-)")))
  
  if(nrow(x)==2){x<-head(x,1)}
  
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

group_idx <- which(tab_sens$Age %in% metric_map)

flextable(tab_sens) %>%
  add_header_row(
    values = c("Age", "Uncertainty interval overlap"),
    colwidths = c(1, 2)
  ) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header", j = 1) %>%
  align(align = "center", part = "header") %>%
  merge_at(i = 1, j = 1:3, part = "body") %>%
  merge_at(i = 8, j = 1:3, part = "body") %>%
  merge_at(i = 14, j = 1:3, part = "body") %>%
  merge_at(i = 16, j = 1:3, part = "body") %>%
  bold(i = group_idx, part = "body") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:3, align = "right", part = "body") %>%
  theme_booktabs() %>%
  autofit() %>%
  save_as_docx(path = "docs/Valid_sens_overlap.docx")

# Study Included----

## mortality ----
df_final_main %>%
  filter(metric %in% c('mort.att','mort.ass')) %>%
  arrange(metric) %>% 
  split(.,.$metric) %>%
  imap_dfr(~{
    df<-.x
    df %>%
      transmute(Age=AGEGR,Country,`Income level`=Income2019,References=sprintf('%s et al. %s',Author0,PubYear),Location,`Study Period`=StudyPeriod,
                `Estimates from individual studies`=sprintf('%.1f\n[%.1f-%.1f]',valid_est,valid_lci,valid_uci)
                ) %>%
      bind_rows(tibble(Age=metric_map[.y]),.)
  }) %>%
  export_rsv_table_doc('docs/Study_final_mortality.docx')


## inc hos main ----
c('inc','hos') %>%
  imap_dfr(~{
    df_final_main %>%
      filter(metric %in% .x) %>%
      arrange(AGEGR,desc(valid_type)) %>% 
      split(.,.$gid) %>%
      imap_dfr(~{
        df<-.x
        if(str_detect(.y,'_single')){
          df %>%
            transmute(Type='Multicenter-study reported',SID,References=sprintf('%s et al. %s',Author0,ifelse(!is.na(PubYear),PubYear,'RSV GEN')),Country,Location,`Study Period`=StudyPeriod,`Income level`=Income2019,Age=AGEGR,
                      valid=sprintf('%.1f\n(%.1f-%.1f)',compare_est,compare_lci,compare_uci),
                      model=sprintf('%.1f\n(%.1f-%.1f)',model_est,model_lci,model_uci),
                      `Uncertainty interval overlap`=ifelse(overlap,'Yes','No'))
        } else {
          df %>%
            transmute(Type='Meta-pooled',SID='-',References='',Country,Location='-',`Study Period`='-',`Income level`=Income2019,Age=AGEGR,
                      valid=sprintf('%.1f\n(%.1f-%.1f)',compare_est,compare_lci,compare_uci),
                      model=sprintf('%.1f\n(%.1f-%.1f)',model_est,model_lci,model_uci),
                      `Uncertainty interval overlap`=ifelse(overlap,'Yes','No'))
        }
      }) %>%
      arrange(Age,desc(Type)) %>%
      bind_rows(tibble(Age=metric_map[.y]),.)
  }) %>% 
  relocate(Age,Country,`Income level`,valid,model,`Uncertainty interval overlap`,Type,SID,Location,`Study Period`) %>%
  export('docs/Study_valid_main_inc_hos.xlsx')

## inc hos sens ----
c('inc','hos') %>%
  imap_dfr(~{
    df_final_sens %>%
      filter(metric %in% .x) %>%
      arrange(AGEGR,desc(valid_type)) %>% 
      split(.,.$gid) %>%
      imap_dfr(~{
        df<-.x
        if(str_detect(.y,'_single')){
          df %>%
            transmute(Type='Multicenter-study reported',SID,References=sprintf('%s et al. %s',Author0,ifelse(!is.na(PubYear),PubYear,'Unpub')),Country,Location,`Study Period`=StudyPeriod,`Income level`=Income2019,Age=AGEGR,
                      valid=sprintf('%.1f\n(%.1f-%.1f)',compare_est,compare_lci,compare_uci),
                      model=sprintf('%.1f\n(%.1f-%.1f)',model_est,model_lci,model_uci),
                      `Uncertainty interval overlap`=ifelse(overlap,'Yes','No'))
        } else {
          df %>%
            transmute(Type='Meta-pooled',SID='-',References='',Country,Location='-',`Study Period`='-',`Income level`=Income2019,Age=AGEGR,
                      valid=sprintf('%.1f\n(%.1f-%.1f)',compare_est,compare_lci,compare_uci),
                      model=sprintf('%.1f\n(%.1f-%.1f)',model_est,model_lci,model_uci),
                      `Uncertainty interval overlap`=ifelse(overlap,'Yes','No'))
        }
      }) %>%
      arrange(Age,desc(Type)) %>%
      bind_rows(tibble(Age=metric_map[.y]),.)
  }) %>% 
  relocate(Age,Country,`Income level`,valid,model,`Uncertainty interval overlap`,Type,SID,Location,`Study Period`) %>%
  export('docs/Study_valid_sens_inc_hos.xlsx')

# Meta Study ----
c('inc','hos') %>%
  imap_dfr(~{
    df_meta_plot_main %>%
      left_join(df_lmic_imputed %>% select(ISOCountry,Income2019)) %>%
      filter(metric==.x) %>%
      split(.,.$gid) %>%
      map_dfr(~{
        df<-.x
        df %>%
          filter(!is.na(SID)) %>%
          transmute(Age=AGEGR,Country,`Income level`=Income2019,SID,References=study_label,Location,`Study Period`=StudyPeriod,
                    Rate=effect_label,Meta=df %>% filter(is.na(SID)) %>% pull(effect_label)) %>%
          mutate(across(c(Age,Country,`Income level`,Meta),~{
            ifelse(row_number()==1,.x,NA_character_)
          }))
      })  %>%
      bind_rows(tibble(Age=metric_map[.y]),.)
  }) %>% 
  export('docs/Study_meta_main.xlsx')

c('inc') %>%
  imap_dfr(~{
    df_meta_plot_sens %>%
      left_join(df_lmic_imputed %>% select(ISOCountry,Income2019)) %>%
      filter(metric==.x) %>%
      split(.,.$gid) %>%
      map_dfr(~{
        df<-.x
        df %>%
          filter(!is.na(SID)) %>%
          transmute(Age=AGEGR,Country,`Income level`=Income2019,SID,References=study_label,Location,`Study Period`=StudyPeriod,
                    Rate=effect_label,Meta=df %>% filter(is.na(SID)) %>% pull(effect_label)) %>%
          mutate(across(c(Age,Country,`Income level`,Meta),~{
            ifelse(row_number()==1,.x,NA_character_)
          }))
      })  %>%
      bind_rows(tibble(Age=metric_map[.y]),.)
  }) %>% 
  export('docs/Study_meta_sens.xlsx')

# Validation Study final ----
df_meta<-left_join(
  df_meta_plot_main %>%
    left_join(df_lmic_imputed %>% select(ISOCountry,Income2019)) %>%
    split(.,.$gid) %>%
    map_dfr(~{
      df<-.x
      df %>%
        filter(!is.na(SID)) %>%
        transmute(gid,SID,Type='Meta-pooled',Age2=AGEGR,Age=AGEGR,Country,`Income level`=Income2019,References=study_label,Location,`Study Period`=StudyPeriod,
                  Rate_ind=effect_label,Rate_main=df %>% filter(is.na(SID)) %>% pull(effect_label)) %>%
        mutate(across(c(Age,Type,Country,`Income level`,Rate_main),~{
          ifelse(row_number()==1,.x,NA_character_)
        }))
    }),
  df_meta_plot_sens %>%
    left_join(df_lmic_imputed %>% select(ISOCountry,Income2019)) %>%
    split(.,.$gid) %>%
    map_dfr(~{
      df<-.x
      df %>%
        filter(!is.na(SID)) %>%
        transmute(gid,SID,Type='Meta-pooled',Age2=AGEGR,Age=AGEGR,Country,`Income level`=Income2019,References=study_label,Location,`Study Period`=StudyPeriod,
                  Rate_ind=effect_label,Rate_sens=df %>% filter(is.na(SID)) %>% pull(effect_label)) %>%
        mutate(across(c(Age,Type,Country,`Income level`,Rate_sens),~{
          ifelse(row_number()==1,.x,NA_character_)
        }))
    }) %>% select(gid,SID,Rate_sens),by = c('gid','SID')
) %>% 
  mutate(Rate_sens=ifelse(!is.na(Rate_main) & is.na(Rate_sens),'—',Rate_sens),
         across(starts_with('Rate_'),~str_replace(.x,' \\[','\n[')))

tem_main<-c('inc','hos') %>%
  imap_dfr(~{
    df_final_main %>%
      filter(metric %in% .x) %>%
      mutate(Age2=ordered(AGEGR,levels=c('0-<6m','6-<12m','0-<12m','12-<60m','0-<60m'))) %>%
      arrange(Age2,desc(valid_type),Country) %>% 
      mutate(gid=factor(gid,levels=unique(.$gid))) %>%
      split(.,.$gid) %>%
      imap_dfr(~{
        df<-.x
        if(str_detect(.y,'_single')){
          df %>%
            transmute(gid,Age2,Type='Multicenter-\nstudy reported',SID,References=sprintf('%s et al. %s%s',Author0,ifelse(!is.na(PubYear),PubYear,'RSV GEN'),ifelse(from=='internal','*','')),Country,Location,`Study Period`=StudyPeriod,`Income level`=Income2019,Age=AGEGR,
                      Rate_ind=sprintf('%.1f\n[%.1f-%.1f]',compare_est,compare_lci,compare_uci),Rate_main=Rate_ind)
        } else {
          df %>%
            transmute(gid) %>%
            left_join(df_meta %>% select(-Rate_sens),by=c('gid'))
        }
      }) %>%
      bind_rows(tibble(Age=metric_map[.y]),.)
  }) %>% 
  relocate(gid,Age2,SID,Age,Country,`Income level`,References,Location,`Study Period`,Type,Rate_ind,Rate_main) %>% 
  mutate(gid_sid=paste(gid,SID,sep='_'),.before=1)

tem_sens<-c('inc','hos') %>%
  imap_dfr(~{
    df_final_sens %>%
      mutate(Age2=factor(AGEGR,levels=c('0-<6m','6-<12m','0-<12m','12-<60m','0-<60m'))) %>%
      filter(metric %in% .x) %>%
      arrange(Age2,desc(valid_type),Country) %>% 
      mutate(gid=factor(gid,levels=unique(.$gid))) %>%
      split(.,.$gid) %>%
      imap_dfr(~{
        df<-.x
        if(str_detect(.y,'_single')){
          df %>%
            transmute(gid,Age2,Type='Multicenter-\nstudy reported',SID,References=sprintf('%s et al. %s',Author0,ifelse(!is.na(PubYear),PubYear,'RSV GEN')),Country,Location,`Study Period`=StudyPeriod,`Income level`=Income2019,Age=AGEGR,
                      Rate_ind=sprintf('%.1f\n[%.1f-%.1f]',compare_est,compare_lci,compare_uci),Rate_sens=Rate_ind)
        } else {
          df %>%
            transmute(gid) %>%
            left_join(df_meta %>% select(-Rate_main),by=c('gid'))
        }
      }) %>%
      arrange(Age2)
  }) %>% 
  relocate(gid,Type,Age2,SID,Age,Country,`Income level`,References,Location,`Study Period`,Rate_ind,Rate_sens) %>% 
  mutate(gid_sid=paste(gid,SID,sep='_'),not_na_row_sens=T,.before=1)

df<-left_join(tem_main,tem_sens %>% select(gid_sid,Rate_sens,not_na_row_sens),by='gid_sid')  %>%
  mutate(Rate_sens=if_else(is.na(not_na_row_sens),'—',Rate_sens)) %T>% 
  export('docs/Study_final_hos_inc_raw.xlsx') %>%
  select(-c(1:4),-not_na_row_sens) %>%
  mutate(across(everything(), ~ replace_na(.x, "")),
         `Study Period`=str_replace(`Study Period`,'-','-\n')) %>%
  export_rsv_table_doc('docs/Study_final_hos_inc_lite.docx')


## References ----

df_meta_plot_main %>% filter(from=='internal',!is.na(PubYear)) %>% distinct(SID,.keep_all = T) %>%
  pull(SID)

df_valid_all %>% filter(SID %in% c("T052","T080","T015")) %>%
  select(SID,Title,PubYear,Author0) %>%
  distinct()

c(
  "L200", "L275", "T015", "T080", "T143", "T162", "T192",
  "L088", "L157", "L165", "L248", "T001", "T052", "T063",
  "T091", "T145", "T163"
)


save.image('rda/code_14_meta and validation.RData')
  
