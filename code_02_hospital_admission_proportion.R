rm(list=ls())
library(tidyverse)
library(ggsci)
library(janitor)
library(openxlsx)
library(showtext)
library(ggvenn)
library(rio)
library(ggcorrplot)
library(mgcv)
library(sjPlot)
library(officer)
library(patchwork)
library(psych)


load("workspaceToBegin.RData")
rm(list = setdiff(ls(), c("com_rate_combined","hos_rate_combined","all.raw")))

# Merge Incidence and Hospital admission Data ----

df1.raw<-com_rate_combined %>%
  filter(Impute %in% c(0),!is.na(ALRI_Deno))
  

df2.raw<-hos_rate_combined  %>%
  filter(Impute %in% c(0),!is.na(HosALRI_Deno))


df1<-df1.raw %>% left_join(all.raw %>% dplyr::select(SID,Country,ISOCountry,Location0)) %>%
  filter(ALRI_N>5) %>%
  rownames_to_column('df1_row') %>%
  relocate(ISOCountry,AGEGR,Income)
  
df2<-df2.raw %>% left_join(all.raw %>% dplyr::select(SID,Country,ISOCountry,Location0)) %>%
  filter(HosALRI_N>5) %>%
  rownames_to_column('df2_row') %>%
  relocate(ISOCountry,AGEGR,Income)

df3<-df1 %>%
  left_join(df2,by=c('AGEGR','ISOCountry','Income')) %>% 
  filter(!is.na(SID.y)) %>%
  relocate(ISOCountry,AGEGR,Income,df1_row,df2_row,StudyMY.x,StudyMY.y,ALRI_N,ALRI_Deno,HosALRI_N,HosALRI_Deno) %>%
  filter(abs(as.numeric(StudyMY.x)-as.numeric(StudyMY.y))<=5) %>%
  add_count(ISOCountry,AGEGR,df1_row,name = 'n1') %>%
  add_count(ISOCountry,AGEGR,df2_row,name = 'n2') %>% 
  relocate(ISOCountry,AGEGR,Income,df1_row,df2_row,n1,n2) %>%
  arrange(-n1,-n2,ISOCountry,AGEGR,Income) %>%
  rownames_to_column('df3_row')

df3.n1eq1_n2eq1<-df3 %>%
  filter(n1==1,n2==1)

df3.n1eq1_n2lt1<-df3 %>%
  filter(n1==1,n2>1)

df3.n1lt1_n2eq1<-df3 %>%
  filter(n1>1,n2==1)

df3_other<- df3 %>%
  filter(n1>1,n2>1)

## Merge ----
df3.n1eq1_n2eq1_new<-df3.n1eq1_n2eq1 %>%
  select(ISOCountry,AGEGR,Income,StudyMY.x,StudyMY.y,ALRI_N,ALRI_Deno,HosALRI_N,HosALRI_Deno,df1_row,df2_row,df3_row)

df3.n1eq1_n2lt1_new<-df3.n1eq1_n2lt1 %>%
  reframe(ISOCountry,AGEGR,Income,StudyMY.x=paste0(StudyMY.x,collapse = ','),StudyMY.y,
            ALRI_N=sum(ALRI_N),ALRI_Deno=sum(ALRI_Deno),HosALRI_N,HosALRI_Deno,
            df1_row=paste0(df1_row,collapse = ','),df3_row=paste0(df3_row,collapse = ','),
            .by=c(ISOCountry,AGEGR,Income,df2_row)) %>%
  distinct()

df3.n1lt1_n2eq1_new<-df3.n1lt1_n2eq1 %>%
  reframe(ISOCountry,AGEGR,Income,StudyMY.x,StudyMY.y=paste0(StudyMY.y,collapse = ','),
          ALRI_N=ALRI_N,ALRI_Deno=ALRI_Deno,HosALRI_N=sum(HosALRI_N),HosALRI_Deno=sum(HosALRI_Deno),
          df2_row=paste0(df1_row,collapse = ','),df3_row=paste0(df3_row,collapse = ','),
          .by=c(ISOCountry,AGEGR,Income,df1_row)) %>%
  distinct()

## df3 other --
df3_other %>% count(ISOCountry,AGEGR,Income)

df3_other2<-df3_other %>%
  filter(AGEGR %in% c('0-<6m','6-<12m','12-<24m'))

df1_ZAF<-df1 %>% filter(df1_row %in% df3_other2$df1_row) %>% arrange(AGEGR) %>%
  reframe(ISOCountry,AGEGR,Income,StudyMY.x=paste0(StudyMY,collapse = ','),
          ALRI_N=sum(ALRI_N),ALRI_Deno=sum(ALRI_Deno),df1_row=paste0(df1_row,collapse = ','),
          .by=c(ISOCountry,AGEGR,Income)) %>%
  distinct()

df2_ZAF<-df2 %>% filter(df2_row %in% df3_other2$df2_row) %>% arrange(AGEGR) %>% 
  reframe(ISOCountry,AGEGR,Income,StudyMY.y=paste0(StudyMY,collapse = ','),
          HosALRI_N=sum(HosALRI_N),HosALRI_Deno=sum(HosALRI_Deno),df2_row=paste0(df2_row,collapse = ','),
          .by=c(ISOCountry,AGEGR,Income)) %>%
  distinct()

df_ZAF<-df1_ZAF %>%
  left_join(df2_ZAF,by=c('AGEGR','ISOCountry','Income'))

df_all.1<-bind_rows(
  df3.n1eq1_n2eq1_new %>% mutate(across(starts_with('StudyMY.'),as.character)),
  df3.n1eq1_n2lt1_new %>% mutate(across(starts_with('StudyMY.'),as.character)),
  df3.n1lt1_n2eq1_new %>% mutate(across(starts_with('StudyMY.'),as.character)),
  df_ZAF
) %>%
  mutate(IR=ALRI_N/ALRI_Deno,
         HR=HosALRI_N/HosALRI_Deno,
         pro=HR/IR) %>%
  filter(pro>0,pro<1) %>%
  arrange(pro) %>%
  mutate(AGEGR2=case_when(
    AGEGR %in% c("0-27d","0-<3m","28d-<3m","0-<6m","3-<6m") ~ "0-<6m",
    AGEGR %in% c("6-<9m","6-<12m","9-<12m") ~ "6-<12m",
    AGEGR %in% c("12-<24m","12-<60m","24-<36m","36-<60m") ~ "12-<60m",
    AGEGR %in% c("0-<12m","0-<24m","0-<36m","0-<60m") ~ AGEGR,
    TRUE ~ NA_character_
  ),.after=AGEGR)

# df
df_all.1 %>%
  count(ISOCountry,AGEGR,Income,sort = T)

df_all.1 %>% count(AGEGR2,AGEGR)


# Add independent variable ----
load('rda/code_01_prevalance_of_risk_factor.RData')

df_pro_country<-distinct(df_all.1,ISOCountry)
df_pro_country.1<-df_pro_country %>%
  left_join(df_u5mr.2 %>% transmute(ISOCountry=ISO.Code,U5MR2019)) %>%
  left_join(df_nmr.2 %>% transmute(ISOCountry=ISO.Code,NMR2019)) %>%
  left_join(df_dhs.2 %>% transmute(ISOCountry,VIS)) %>%
  left_join(df_beds.3 %>% transmute(ISOCountry,BEDS))

df_pro_country.1[which(df_pro_country.1$ISOCountry=='NGA'),'BEDS']=0.5

# new added
df_pro_country.1_new<-df_pro_country %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,U5MR2019:INPA_type))

df_all.2<-df_all.1 %>%
  left_join(df_pro_country.1) %>%
  arrange(VIS,ISOCountry,AGEGR)

df_all.2 %>% rio::export('docs/df_all.2.xlsx')

df_all.2_new<-import('docs/df_all.2_new.xlsx')

df_all.3<-df_all.2_new %>%
  filter(is.na(Excluded)) %>%
  #left_join(df_pro_country.1) %>%
  select(-c(U5MR_2019,NMR_2019,VIS,BEDS)) %>%
  left_join(df_pro_country.1_new) %>% 
  dplyr::select(ISOCountry,Income,AGEGR2,StudyMY.x,StudyMY.y,ALRI_N,ALRI_Deno,pro,HosALRI_N,HosALRI_Deno,U5MR2019:INPA_type) %>%
  relocate(HIV_type,.after=INPA_type)

df_all.3 %>% count(AGEGR2)
df_all.3 %>% count(ISOCountry)

## 1️⃣Export df_all.3 ----
rio::export(df_all.3,'rda/df_all.3.rds')

names(df_all.3)

cor_mat <- cor(df_all.3 %>% select(U5MR2019:INPA), use = "complete.obs")

ggcorrplot(cor_mat, 
           method = "circle", 
           type = "lower",
           lab = TRUE, lab_size = 3,
           colors = c("blue", "white", "red"),
           title = "Correlation matrix heatmap")

df_all.3 %>% select(U5MR2019:INPA) %>%
  pairs.panels(method = "pearson",
            hist.col = "#00AFBB",
            density = TRUE,
            ellipses = TRUE)


# Beta Regression ----
ggplot(df_all.3,aes(BEDS,U5MR2019))+
  geom_point(aes(color=ISOCountry))+
  geom_smooth()


p1<-ggplot(df_all.3,aes(U5MR2019,pro))+
  geom_point(aes(color=ISOCountry))+
  geom_smooth()

p2<-ggplot(df_all.3,aes(BEDS,pro))+
  geom_point(aes(color=ISOCountry))+
  geom_smooth()

p3<-ggplot(df_all.3,aes(BEDS,pro))+
  geom_point(aes(color=ISOCountry))+
  geom_smooth(method = 'lm')

p4<-ggplot(df_all.3,aes(VIS,pro))+
  geom_point(aes(color=ISOCountry))+
  geom_smooth(method = 'lm')

p1+p2+p3+p4+plot_layout(nrow = 2,guides='collect')

df_all.3 %>% count(ISOCountry)

ggplot(df_all.3,aes(U5MR2019,pro,color=AGEGR2,shape=AGEGR2))+
  geom_point(size=3)

# GAM Beta 回归，所有连续变量加平滑项
# 分类变量 AGEGR2 仍作为因子处理
gam_model <- gam(
  pro ~ AGEGR2 + s(U5MR2019,k=3) + BEDS,
  family = betar(),   # Beta 回归
  data = df_all.3
)

gam_model <- gam(
  pro ~ AGEGR2 + s(U5MR2019,k=3),
  family = betar(),   # Beta 回归
  data = df_all.3
)

summary(gam_model)

plot(gam_model, pages=1, rug=TRUE, se=TRUE)

par(mfrow = c(2, 2))
gam.check(gam_model)


df_pred_u5mr <- expand.grid(
  AGEGR2 = c('0-<6m','6-<12m','12-<60m'),
  BEDS   = quantile(df_all.3$BEDS),
  U5MR2019 = seq(min(df_all.3$U5MR2019, na.rm = TRUE),
                  max(df_all.3$U5MR2019, na.rm = TRUE),
                  length.out = 100)
) %>%
  mutate(
    fit = predict(gam_model, newdata = ., type = "response"),
    #fit = predict(models[[1]], newdata = ., type = "response")
  )

ggplot(df_pred_u5mr,aes(U5MR2019,fit,color=AGEGR2))+
  geom_line(aes(group=AGEGR2))+
  facet_wrap(vars(BEDS),labeller =  labeller(BEDS = function(x) paste0("BEDS: ", x)))

ggplot(df_pred_u5mr,aes(U5MR2019,fit,color=BEDS))+
  geom_line(aes(group=BEDS))+
  facet_wrap(vars(AGEGR2))

ggplot(df_all.3,aes(U5MR2019,BEDS))+
  geom_point()+
  geom_smooth()

ggplot(df_pred_u5mr,aes(U5MR2019,fit,color=AGEGR2))+
  geom_line(aes(group=AGEGR2))

# U5MR distribution

ggplot(df_u5mr.2, aes(x = U5MR2019)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 5, fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "U5MR2019",
    y = "Density",
    title = "Histogram + Density of U5MR"
  )

ggplot(df_u5mr.2, aes(x = log(U5MR2019))) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = .1, fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "U5MR2019",
    y = "Density",
    title = "Histogram + Density of log(U5MR)"
  )

summary(df_u5mr.2$U5MR2019)


# Model select ----
models<-c('pro ~ AGEGR2 + s(U5MR2019,k=3)',
  'pro ~ AGEGR2 + s(U5MR2019,k=3)+BEDS',
  'pro ~ AGEGR2 + s(U5MR2019,k=3)+VIS',
  'pro ~ AGEGR2 + s(U5MR2019,k=3)+BEDS+VIS',
  'pro ~ AGEGR2 + BEDS',
  'pro ~ AGEGR2 + VIS'
  ) %>%
  map(~{
    gam(
      as.formula(.x),
      family = betar(),   # Beta 回归
      data = df_all.3
    )
  })
  
tab_model(models, transform = NULL)

models_2<-c('pro ~ AGEGR2 + s(log(U5MR2019),k=3)',
          'pro ~ AGEGR2 + s(log(U5MR2019),k=3)+BEDS',
          'pro ~ AGEGR2 + s(log(U5MR2019),k=3)+VIS',
          'pro ~ AGEGR2 + s(log(U5MR2019),k=3)+BEDS+VIS',
          'pro ~ AGEGR2 + BEDS',
          'pro ~ AGEGR2 + VIS'
) %>%
  map(~{
    gam(
      as.formula(.x),
      family = betar(),   # Beta 回归
      data = df_all.3
    )
  })

tab_model(models_2, transform = NULL)


par(mfrow = c(1, 2))
plot(models[[1]], rug=TRUE, se=TRUE,main='')
plot(models[[2]], rug=TRUE, se=TRUE,main='+BEDS')

par(mfrow = c(2, 2))
gam.check(models[[1]])

tab_model(models[[1]])

summary(models[[1]])

# Pro Distrubution ----

df_all.3 %>%
  ggplot(aes(pro,color=AGEGR2))+
  geom_histogram()+
  geom_density()

hist(df_all.3[df_all.3$AGEGR2=='0-<6m','pro'])

# Clean Hospitalisation raw data ----
df2_lmic<-df2 %>%
  filter(Income!='H') %>%
  mutate(AGEGR2=case_when(
    AGEGR %in% c("0-27d","0-<3m","28d-<3m","0-<6m","3-<6m") ~ "0-<6m",
    AGEGR %in% c("6-<9m","6-<12m","9-<12m") ~ "6-<12m",
    AGEGR %in% c("12-<24m","12-<60m","24-<36m","36-<60m") ~ "12-<60m",
    AGEGR %in% c("0-<12m","0-<24m","0-<36m","0-<60m") ~ AGEGR,
    TRUE ~ NA_character_
  ),.after=AGEGR)

df2_lmic.aggregated<-df2_lmic %>%
  summarise(across(c(HosALRI_N,HosALRI_Deno),sum),.by=c(ISOCountry,AGEGR2,Income)) %>%
  mutate(HR_real=HosALRI_N/HosALRI_Deno*1000)

rio::export(df2_lmic.aggregated,'rda/df2_lmic.aggregated.rds')
rio::export(df2_lmic,'rda/df2_lmic.rds')
