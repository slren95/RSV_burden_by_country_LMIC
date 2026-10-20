rm(list=ls())
library(rio)
library(tidyverse)
library(ggpubr)
library(scales)
library(patchwork)

## All Results for Community Mortality Rate ----
load("workspaceToBegin.RData")
rm(list = setdiff(ls(), c("mor_all_DeCoDe.predict","mor_all_NP.predict","pop_mor.raw")))


mor_all_DeCoDe.predict2<-import('mor_all_DeCoDe.predict.rds',trust=T)
df_lmic_imputed<-import('rda/df_lmic_imputed.rds',trust=T)
all.equal(mor_all_DeCoDe.predict2,mor_all_DeCoDe.predict)

mor_all_DeCoDe.predict %>% filter(index==1) %>% count(Income2019)

country_mort<-mor_all_DeCoDe.predict %>% filter(Income2019!='H',index==1) %>% select(ISOCountry,Country.Name,Income2019,Pop) # 133

setdiff(df_lmic_imputed$ISOCountry,country_mort$ISOCountry)
setdiff(country_mort$ISOCountry,df_lmic_imputed$ISOCountry)

df_lmic_imputed %>% filter(ISOCountry %in% c('GTM','ASM','XKX')) 

mor_all_DeCoDe.predict %>% filter(index==1) %>% filter(ISOCountry %in% c('GTM','ASM','XKX','NIU')) 

# Fix GTM Guatemala as UM country 

mor_all_DeCoDe.predict.new <- mor_all_DeCoDe.predict %>%
  mutate(Income2019 = if_else(ISOCountry == 'GTM', 'UM', Income2019))

mor_all_DeCoDe.predict.new %>% filter(index==1) %>% count(Income2019)

df_lmic_imputed %>% filter(ISOCountry %in% c('ASM','XKX')) 

# Population Check ----
mor_all_DeCoDe.predict.new %>% filter(index==1) %>% 
  summarise(across(Y0:Y4,~sum(.x,na.rm=T)),.by=Income2019) %>%
  mutate(pop_0006=Y0/2,pop_0612=Y0/2,pop_1260=Y1+Y2+Y3+Y4,pop_U5=Y0+Y1+Y2+Y3+Y4) %>%
  filter(Income2019!='H')

df_lmic_imputed %>%
  ungroup() %>%
  summarise(across(matches('^(Y|pop_)'),sum),.by=Income2019) %>%
  mutate(pop_U5=Y0+Y1+Y2+Y3+Y4)

# Global mean ----
df_global_pop<-pop_mor.raw %>%
  summarise(across(starts_with('Y'),~sum(.x,na.rm=T))) %>%
  transmute(pop_0006=Y0/2,pop_0612=Y0/2,pop_1260=Y1+Y2+Y3+Y4,
            pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612)
  
df_global_mort_att<-mor_all_DeCoDe.predict.new %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm=T)),.by=index) %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    )
  ) %>%
  cross_join(df_global_pop) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

df_global_mort_ass<-mor_all_NP.predict %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm=T)),.by=index) %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    )
  ) %>%
  cross_join(df_global_pop) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

df_global_mean_mort<-bind_rows(list(mort_att=df_global_mort_att,mort_ass=df_global_mort_ass),.id = 'metric') %>%
  select(-starts_with('pop')) %>%
  pivot_longer(cols=-metric,names_pattern = "(.*)_(N|R)_(.*)",names_to = c("AGEGR",'type','.value')) %>%
  mutate(AGEGR = recode(AGEGR,
                        'm0006' = '0-<6m',
                        'm0612' = '6-<12m',
                        'm1260' = '12-<60m',
                        'm0060' = '0-<60m',
                        'm0012' = '0-<12m'
  )) %>%
  rename(est = `500`, lci = `025`, uci = `975`) %>%
  pivot_wider(
    id_cols = c(AGEGR),      # 行标识列
    names_from = c(metric,type),              # 将 AGEGR 变成列名
    values_from = c(est, lci, uci), # 要展开的值列
    names_glue = "{metric}_{type}_{.value}"
  )

rio::export(df_global_mean_mort,'rda/df_global_mean_mort.rds')

# 1️⃣133 LMIC country ----
df_mort_all_DeCoDe.lmic<-mor_all_DeCoDe.predict.new %>%
  filter(ISOCountry %in% df_lmic_imputed$ISOCountry)

df_mort_all_NP.lmic<-mor_all_NP.predict %>%
  mutate(Income2019 = if_else(ISOCountry == 'GTM', 'UM', Income2019)) %>%
  filter(ISOCountry %in% df_lmic_imputed$ISOCountry)

rio::export(df_mort_all_DeCoDe.lmic,'rda/df_mort_all_DeCoDe.lmic.rds')
rio::export(df_mort_all_NP.lmic,'rda/df_mort_all_NP.lmic.rds')

# Population LMIC ----
pop_region.raw_new<-import('rda/pop_region.raw_new.rds',trust=T)

pop_region.byincome<-bind_rows(
  pop_region.raw_new,
  pop_region.raw_new %>% mutate(Income2019='Global') %>%
    summarise(pop=sum(pop),.by = c(Income2019,Group,AGEGR))
) %>%
  pivot_wider(id_cols = Income2019,names_from = Group,values_from = pop) %>%
  rename(pop_0060=pop_U5)

# 2️⃣Summarise and calculate mortality ----

df_sum_all_DeCoDe<-df_mort_all_DeCoDe.lmic %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N,
            ISOCountry,Income2019) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(ISOCountry, Income2019)
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,
         m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

df_sum_all_NP<-df_mort_all_NP.lmic %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N,
            ISOCountry,Income2019) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(ISOCountry, Income2019)
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,
         m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
         ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

rio::export(df_sum_all_DeCoDe,'rda/df_sum_all_DeCoDe.rds')
rio::export(df_sum_all_NP,'rda/df_sum_all_NP.rds')

# 3️⃣By Income level ----
df_sum_all_DeCoDe.byincome<-bind_rows(
  df_mort_all_DeCoDe.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(Income2019,index)),
  df_mort_all_DeCoDe.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(index)) %>%
    mutate(Income2019='Global')
) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric) & !all_of("index"),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(Income2019)
  ) %>%
  left_join(pop_region.byincome,by = 'Income2019') %>%
  mutate(pop_0012=pop_0006+pop_0612) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  ) %>%
  mutate(Income2019=recode(Income2019,'Global'='LMIC'))

df_sum_all_NP.byincome<-bind_rows(
  df_mort_all_NP.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(Income2019,index)),
  df_mort_all_NP.lmic %>%
    transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,
              m0012_N=m0006_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    summarise(across(where(is.numeric),sum),.by = c(index)) %>%
    mutate(Income2019='Global')
) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric) & !all_of("index"),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(Income2019)
  ) %>%
  left_join(pop_region.byincome,by = 'Income2019') %>%
  mutate(pop_0012=pop_0006+pop_0612) %>%
  mutate(m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  ) %>%
  mutate(Income2019=recode(Income2019,'Global'='LMIC'))

rio::export(df_sum_all_DeCoDe.byincome,'rda/df_sum_all_DeCoDe.byincome.rds')
rio::export(df_sum_all_NP.byincome,'rda/df_sum_all_NP.byincome.rds')

# Attributable and Associated ALRI deaths ----

mor_ALRI_DeCoDe.predict<-import('mor_ALRI_DeCoDe.predict.rds',trust=T)
mor_ALRI_NP.predict<-import('mor_ALRI_NP.predict.rds',trust=T)

df_mort_ALRI_DeCoDe.lmic<-mor_ALRI_DeCoDe.predict %>%
  mutate(Income2019 = if_else(ISOCountry == 'GTM', 'UM', Income2019)) %>%
  filter(ISOCountry %in% df_lmic_imputed$ISOCountry)

df_mort_ALRI_NP.lmic<-mor_ALRI_NP.predict %>%
  mutate(Income2019 = if_else(ISOCountry == 'GTM', 'UM', Income2019)) %>%
  filter(ISOCountry %in% df_lmic_imputed$ISOCountry)


df_sum_ALRI_DeCoDe<-df_mort_ALRI_DeCoDe.lmic %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N,
            ISOCountry,Income2019) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(ISOCountry, Income2019)
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,
         m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

df_sum_ALRI_NP<-df_mort_ALRI_NP.lmic %>%
  transmute(m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,m0060_N=m0006_N+m0612_N+m1260_N,m0012_N=m0006_N+m0612_N,
            ISOCountry,Income2019) %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      where(is.numeric),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(ISOCountry, Income2019)
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,
         m0006_R=m0006_N/pop_0006,
         m0612_R=m0612_N/pop_0612,
         m1260_R=m1260_N/pop_1260,
         m0060_R=m0060_N/pop_0060,
         m0012_R=m0012_N/pop_0012
  ) %>%
  pivot_wider(
    names_from = q,                        
    values_from = contains(c("_N", "_R")), 
    names_glue = "{.value}_{q}"           
  )

rio::export(df_sum_ALRI_DeCoDe,'rda/df_sum_ALRI_DeCoDe.rds')
rio::export(df_sum_ALRI_NP,'rda/df_sum_ALRI_NP.rds')

# Ratio of ALRI dealth to all-cause deaths ----
df_ratio<-left_join(df_mort_ALRI_DeCoDe.lmic %>%
  transmute(m0012_N=m0001_N+m0106_N+m0612_N,
            ISOCountry,Income2019,index) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
  mutate(pop_0012=pop_0006+pop_0612,
         m0012_R=m0012_N/pop_0012
  ) %>%
  transmute(index,ISOCountry,CountryName,Income2019,mort_rate_ALRI_DeCoDe=m0012_R),
  df_mort_all_DeCoDe.lmic %>%
    transmute(m0012_N=m0001_N+m0106_N+m0612_N,
              ISOCountry,Income2019,index) %>%
    left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,starts_with('pop_'))) %>%
    mutate(pop_0012=pop_0006+pop_0612,
           m0012_R=m0012_N/pop_0012
    ) %>%
    transmute(index,ISOCountry,mort_rate_all_DeCoDe=m0012_R),by = c('ISOCountry','index')) %>%
  mutate(ratio=mort_rate_ALRI_DeCoDe/mort_rate_all_DeCoDe) %>%
  mutate(Income2019 = recode(Income2019,
                             'L'  = 'Low income',
                             'LM' = 'Lower-middle income',
                             'UM' = 'Upper-middle income',
                             'H'  = 'High income'
  ))
  
df_ratio %>%
  group_by(Income2019) %>%
  group_map(~summary(.x$ratio))

df_ratio %>%
  ggplot(aes(ratio,color=Income2019))+
  geom_density()

# 先按 Income2019 分组计算中位数
median_df <- df_ratio %>%
  group_by(Income2019) %>%
  summarise(median_ratio = median(ratio, na.rm = TRUE),
            median_pct=percent(median_ratio, accuracy = 0.1))

# 绘图 + 添加中位数垂直线
df_ratio %>%
  ggplot(aes(ratio, color = Income2019)) +
  geom_density(linewidth = 1,aes()) +
  geom_vline(data = median_df, aes(xintercept = median_ratio, color = Income2019), 
             linetype = "dashed", linewidth = 0.5) +
  geom_label(data = median_df, aes(x = median_ratio, y = 2, label = median_pct, color = Income2019), 
            angle = 00, hjust = c(0,2,9), size = 3.5, show.legend = FALSE) +
  xlim(0, 2) +
  scale_color_discrete(labels = c("LIC" = "Low income", "LMIC" = "Lower middle income", 
                                  "UMIC" = "Upper middle income")) +
  labs(x = "Ratio", y = "Density", color = "Income level") +
  theme_bw()

ggdensity(df_ratio, x = "ratio",xlab = 'ratio',caption='dashed line: median',
          add = "median", rug = TRUE,ylab = 'Density',
          color = "Income2019", fill = "Income2019")+
  scale_x_continuous(breaks = seq(0,1,by=.1),labels = percent_format())+
  labs(fill=NULL,color=NULL)+
  theme(axis.title.x = element_text(lineheight = .5))+
  xlim(c(0,2))

df_ratio %>%
  ggplot(aes(x = Income2019, y = ratio, fill = Income2019)) +
  geom_boxplot(width = 0.6, outlier.size = 1) +
  geom_label(data = median_df, aes(x = Income2019, y = median_ratio, label = median_pct), 
             size = 3.5, vjust = 0.5, fill = "white", label.size = 0.2, show.legend = FALSE) +
  scale_fill_discrete(labels = c("L" = "Low income", "LM" = "Lower middle income", 
                                 "UM" = "Upper middle income")) +
  scale_x_discrete(labels = c("L" = "Low income", "LM" = "Lower middle income", 
                              "UM" = "Upper middle income")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = percent_format()) +
  labs(x = "Income level", y = "Proportion of RSV-attributable ALRI deaths in RSV-attributable \n all-cause deaths (0–<12 months)", fill = "Income level") +
  theme_bw()+
  coord_cartesian(ylim=c(0, 1))

ggsave("plot/Proportion_ALRI_to_ALL_by_income.tiff", width = 12, height = 6, dpi = 300,compression = "lzw")


df_ratio %>%
  group_by(Income2019) %>%
  group_map(~{
    ggplot(.x,aes(x = ratio, y = CountryName, fill = Income2019)) +
      geom_boxplot(width = 0.6, outlier.size = 1) +
      facet_grid(~ Income2019, scales = "free_y", space = "free_y") +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = percent_format()) +
      labs(x = "Proportion of RSV-attributable ALRI deaths in RSV-attributable \n all-cause deaths (0–<12 months)", 
           y = "Country", 
           fill = "Income level") +
      theme_bw()
  },.keep = TRUE) %>%
  wrap_plots(ncol = 3)


ggplot(df_ratio,aes(x = ratio, y = CountryName, fill = Income2019)) +
  # 密集虚线 (保持原样)
  geom_vline(
    data = median_df,
    aes(xintercept = median_ratio, color = Income2019),
    linetype = "22", 
    linewidth = 0.7
  ) +
  geom_boxplot(width = 0.6, outlier.size = 1) +
  # 升级点 1：将数值标记在绘图区底部，但位于坐标轴之上
  # 我们需要为 Y 轴添加一个空的占位符，以确保标签有空间
  geom_text(
    data = median_df,
    aes(
      x = median_ratio, 
      y = 0.5,  # 强制放置在 Y 轴底部空白处
      label = percent(median_ratio, accuracy = 0.1), 
      color = Income2019
    ),
    vjust = 1.5,        # 文字向上微调，避免贴线
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ Income2019, ncol = 3, scales = "free_y") +
  # 升级点 2：为每个分面的离散 Y 轴手动扩展底部空间
  scale_y_discrete(
    # 强制将底部边界扩展到 0，给 geom_text(y=0.5) 留出空间
    expand = expansion(mult = c(0.05, 0.03)) # mult = c(底部扩展, 顶部扩展)
  ) +
  scale_x_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.2), 
    labels = percent_format()
  ) +
  labs(
    x = "\nProportion of RSV-attributable ALRI deaths in RSV-attributable all-cause deaths (0–<12 months)", 
    y = "Country", 
    fill = "Income level",
    color = "Income level",
    caption = "Dashed lines represent the median value within each income group."
  ) +
  theme_bw()

ggsave("plot/Proportion_ALRI_to_ALL_by_country.tiff", width = 12, height = 8, dpi = 300,compression = "lzw")

ggplot(df_ratio %>% filter(Income2019=='Low income'),aes(x = ratio, y = CountryName, fill = Income2019)) +
  # 密集虚线 (保持原样)
  geom_vline(
    data = median_df %>% filter(Income2019=='Low income'),
    aes(xintercept = median_ratio, color = Income2019),
    linetype = "22", 
    linewidth = 0.7
  ) +
  geom_boxplot(width = 0.6, outlier.size = 1) +
  # 升级点 1：将数值标记在绘图区底部，但位于坐标轴之上
  # 我们需要为 Y 轴添加一个空的占位符，以确保标签有空间
  geom_text(
    data = median_df %>% filter(Income2019=='Low income'),
    aes(
      x = median_ratio, 
      y = 0.5,  # 强制放置在 Y 轴底部空白处
      label = percent(median_ratio, accuracy = 0.1), 
      color = Income2019
    ),
    vjust = 1.5,        # 文字向上微调，避免贴线
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  # 升级点 2：为每个分面的离散 Y 轴手动扩展底部空间
  scale_y_discrete(
    # 强制将底部边界扩展到 0，给 geom_text(y=0.5) 留出空间
    expand = expansion(mult = c(0.05, 0.03)) # mult = c(底部扩展, 顶部扩展)
  ) +
  facet_wrap(~ Income2019, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.2), 
    labels = percent_format()
  ) +
  labs(
    x = "\nProportion of RSV-attributable ALRI deaths in RSV-attributable all-cause deaths (0–<12 months)", 
    y = "Country", 
    fill = "Income level",
    color = "Income level",
    caption = "Dashed lines represent the median value"
  ) +
  theme_bw()

ggsave("plot/Proportion_ALRI_to_ALL_by_country_LIC.tiff", width = 12, height = 8, dpi = 300,compression = "lzw")

# 025 975 ----
df_ratio_by_country<-df_ratio %>% 
  summarise(est=median(ratio),lci=quantile(ratio,0.025),uci=quantile(ratio,0.975),.by = c(CountryName,Income2019))

ggplot(df_ratio_by_country, aes(x = est, y = CountryName, color = Income2019)) +
  # 1. 密集虚线 (全局中位数)
  geom_vline(
    data = median_df,
    aes(xintercept = median_ratio, color = Income2019),
    linetype = "22", 
    linewidth = 0.7
  ) +
  # 2. 横向 Line-range
  geom_pointrange(
    aes(xmin = lci, xmax = uci),
    size = 0.4,       # 中心点大小
    fatten = 2.5,     # 点放大的倍数
    linewidth = 0.6   # 线条粗细
  ) +
  # 3. 升级点：将数值标在底部 (y = 0.5)
  geom_text(
    data = median_df,
    aes(
      x = median_ratio, 
      y = 0.5,        # 锚定在离散 Y 轴下方第 0.5 的位置（最底部）
      label = median_pct, 
      color = Income2019
    ),
    vjust = 0,        # 文字底部对齐，向上微调
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  # 4. 按收入等级分面
  facet_wrap(~ Income2019, ncol = 3, scales = "free_y") +
  # 5. 关键控制：底部多留 12% 的衬垫空间给 geom_text，顶部保持正常
  scale_y_discrete(
    expand = expansion(mult = c(0.12, 0.05)) # mult = c(底部扩展倍数, 顶部扩展倍数)
  ) +
  # 6. X 轴百分比格式
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2), 
    labels = percent_format()
  ) +
  # 7. 标签与 Caption 说明
  labs(
    x = "\nProportion of RSV-attributable ALRI deaths in RSV-attributable all-cause deaths (0–<12 months)", 
    y = "Country", 
    color = "Income level",
    caption = "Points represent country-level median values; horizontal lines show 2.5%–97.5% intervals. Dashed lines indicate median by income level."
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 9, face = "italic", color = "grey30", hjust = 1, margin = margin(t = 12)),
    panel.grid.minor = element_blank()
  )+
  coord_cartesian(xlim = c(0, 1), clip = "off")
