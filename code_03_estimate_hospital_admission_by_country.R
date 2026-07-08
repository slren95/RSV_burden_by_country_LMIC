rm(list=ls())
library(tidyverse)
library(rio)
library(psych)
library(factoextra)

library(fitdistrplus)
library(goftest)
library(ppcor)
library(patchwork)
library(logitnorm)
library(ggsci)
library(plotly)
library(janitor)
library(mice)
library(broom)


source('functions.R')

df_lmic.5<-import('rda/df_lmic.5.rds',trust=T)
df_lmic_imputed<-import('rda/df_lmic_imputed.rds',trust=T)

df_all.2_new<-import('docs/df_all.2_new.xlsx')
df_all.3<-import('rda/df_all.3.rds',trust=T)


# Pro Real Data ----
df_all.3 %>% distinct(ISOCountry,AGEGR2,pro) #

df_all.3 %>% count(AGEGR2)

df_all.3 %>% count(ISOCountry)

# Missing rate ----
skimr::skim(df_lmic.5) %>% mutate(miss=round((1-complete_rate)*100,1))


# Correlation matrix ----
names(df_lmic.5)
vars<-c("U5MR2019","NMR2019","MMRT","BEDS","PHYS","NUMW","OUTP",'INPA')

df_lmic.5 %>% dplyr::select(all_of(vars)) %>% 
  summary()

df_lmic.5 %>% dplyr::select(all_of(vars)) %>% 
  mutate(across(c(everything()),log)) %>%
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )

df_lmic.5 %>% dplyr::select(all_of(vars)) %>% 
  mutate(across(c(everything()),~.x)) %>%
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )

names(df_lmic.5)


# PCA ----
df_pca <- df_lmic_imputed %>%  # df_lmic.5
  dplyr::select(ISOCountry,all_of(vars)) %>%
  drop_na() %>%
  mutate(across(where(is.numeric), ~log(.x))) %>%
  mutate(across(where(is.numeric), ~scale(.x)))

df_pca %>% summary()

df_pca %>%
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )


attr(df_pca$U5MR2019, "scaled:center")
sapply(df_pca[vars], function(x) attr(x, "scaled:center"))
sapply(df_pca[vars], function(x) attr(x, "scaled:scale"))

df_lmic.5 %>%
  dplyr::select(all_of(vars)) %>%
  drop_na() %>%
  mutate(across(everything(), ~log(.x))) %>%
  summarise(across(everything(),list(mean=~mean(.x),sd=~sd(.x))))

df_pca %>% dplyr::select(all_of(vars)) %>% 
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )

pca_res <- prcomp(df_pca[,-1], center = FALSE, scale. = FALSE)

summary(pca_res)
pca_res$rotation

fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 100)) +
  labs(title=NULL)+
  theme(axis.text = element_text(size=10))

ggsave('plot/PCA_scree.jpg',dpi = 200,width = 2000,height = 1200,units = 'px')

fviz_pca_ind(pca_res,
             repel = TRUE,  # 标签不重叠
             col.ind = "steelblue")

fviz_pca_var(pca_res,
             col.var = "contrib", # 按贡献度上色
             gradient.cols = c("lightblue","blue","darkblue"))+
  labs(color='Contribution (%)',title=NULL)+
  guides(color = guide_colorbar(
    title.position = "top",  # 将标题放在色块上方
    title.hjust = 0.5        # 标题水平居中对齐
  ))+
  theme(axis.text = element_text(size=10))

ggsave('plot/PCA_dim.jpg',dpi = 200,width = 2000,height = 1200,units = 'px')


head(pca_res$x)
hist(pca_res$x[,1])
summary(pca_res$x[,1])

# PCA var select ----
selection_grid <- expand_grid(
  U5MR2019 = c(TRUE, FALSE),
  NMR2019 = c(TRUE, FALSE),
  BEDS = c(TRUE, FALSE),
  PHYS = c(TRUE, FALSE),
  NUMW = c(TRUE, FALSE),
  MMRT = c(TRUE, FALSE),
  OUTP = c(TRUE, FALSE),
  INPA = c(TRUE, FALSE)
) %>%
  mutate(vars_selected = apply(., 1, function(x) paste(names(.)[x], collapse = "_"))) %>%
  rowwise() %>%
  mutate(
    n_selected = sum(c_across(-vars_selected))
  ) %>%
  filter(n_selected >= 4) %>%
  ungroup()

df_pca_grid<-selection_grid %>%
  mutate(vars = str_split(vars_selected, "_"),
         df_pca=map(vars,~{
           df_lmic_imputed %>%  # df_lmic.5
             dplyr::select(ISOCountry,all_of(.x)) %>%
             drop_na() %>%
             mutate(across(where(is.numeric), ~log(.x))) %>%
             mutate(across(where(is.numeric), ~scale(.x)))
         }),
         pca_res=map(df_pca,~prcomp(.x[,-1], center = FALSE, scale. = FALSE)),
         variance = map(pca_res, ~{
           tidy(.x, matrix = "pcs") %>%
             filter(PC %in% c(1, 2)) %>%
             select(PC, percent)
         }),
         var_dim1=map_dbl(pca_res, ~{
           tidy(.x, matrix = "pcs") %>%
             filter(PC %in% c(1)) %>%
             select(PC, percent) %>% pull(percent)
         }),
         loadings = map(pca_res, ~{
           tidy(.x, matrix = "loadings") %>%
             filter(PC %in% c(1, 2)) %>%
             select(PC, column, value) %>%
             pivot_wider(names_from = PC, values_from = value, names_prefix = "PC")
         }),
         df_comp=map2(pca_res,df_pca,~{
           pca_res<-.x
           df_pca<-.y
           isos<-unique(df_all.3$ISOCountry)
           match(isos,df_pca$ISOCountry)
           df_comp<-data.frame(ISOCountry=isos,PC1=pca_res$x[match(isos,df_pca$ISOCountry),1]) %>%
             left_join(df_all.3 %>% dplyr::select(ISOCountry,pro,AGEGR2)) %>%
             mutate(AGEGR3=case_when(AGEGR2=='0-<6m'~1,
                                     AGEGR2=='6-<12m'~2,
                                     AGEGR2=='12-<60m'~3))
           df_comp
         }),
         pcor_res=map(df_comp,~{
           df_comp<-.x
           pcor.test(df_comp$pro, df_comp$PC1, df_comp$AGEGR3, method="spearman")
         }),
         coef_speanman=map_dbl(pcor_res,~.x$estimate)) %>%
  arrange(desc(var_dim1)) %>%
  mutate(row_id = row_number())

df_pca_grid %>%
  head(10) %>%
  pwalk(function(row_id, pca_res, ...) {
    # 碎石图
    ggsave(paste0('plot/PCA/PCA_scree_', row_id, '.jpg'),
           plot = fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 100)) +
             labs(title = NULL) +
             theme(axis.text = element_text(size = 10)),
           dpi = 200, width = 2000, height = 1200, units = 'px')
    
    # 变量图
    ggsave(paste0('plot/PCA/PCA_var_', row_id, '.jpg'),
           plot = fviz_pca_var(pca_res,
                               col.var = "contrib",
                               gradient.cols = c("lightblue", "blue", "darkblue"),
                               repel = TRUE) +
             labs(color = 'Contribution (%)', title = NULL) +
             guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
             theme(axis.text = element_text(size = 10)),
           dpi = 200, width = 2000, height = 1200, units = 'px')
  })

df_pca_grid %>%
  head(10) %>%
  pmap(function(row_id, pca_res, ...) {
    name_map <- c(
      "U5MR2019" = "U5MR", 
      "NMR2019"  = "NMR", 
      "MMRT"     = "MMRT", 
      "BEDS"     = "BEDS", 
      "PHYS"     = "PHYS", 
      "NUMW"     = "NUMW", 
      "OUTP"     = "OUTP", 
      "INPA"     = "INPA"
    )
    rownames(pca_res$rotation) <- name_map[rownames(pca_res$rotation)]
    print(rownames(pca_res$rotation))
    fviz_pca_var(pca_res,
                 col.var = "contrib",
                 gradient.cols = c("lightblue", "blue", "darkblue"),
                 repel = TRUE) +
      labs(color = 'Contribution (%)', title = NULL) +
      guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
      theme(axis.text = element_text(size = 8))
  }) %>%
  wrap_plots(ncol=2) %>%
  ggsave('plot/PCA/PCA_all.tiff',plot=., width = 8, height = 10, dpi = 300)



df_pca_grid %>%
  filter(INPA==T | OUTP==T) %>%
  head(10) %>%
  pwalk(function(row_id, pca_res, ...) {
    # 碎石图
    ggsave(paste0('plot/PCA/INPOUTP_scree_', row_id, '.jpg'),
           plot = fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 100)) +
             labs(title = NULL) +
             theme(axis.text = element_text(size = 10)),
           dpi = 200, width = 2000, height = 1200, units = 'px')
    
    # 变量图
    ggsave(paste0('plot/PCA/INPOUTP_var_', row_id, '.jpg'),
           plot = fviz_pca_var(pca_res,
                               col.var = "contrib",
                               gradient.cols = c("lightblue", "blue", "darkblue"),
                               repel = TRUE) +
             labs(color = 'Contribution (%)', title = NULL) +
             guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
             theme(axis.text = element_text(size = 10)),
           dpi = 200, width = 2000, height = 1200, units = 'px')
  })


## Real world Pro Data ----
isos<-unique(df_all.3$ISOCountry)
match(isos,df_pca$ISOCountry)


df_comp<-data.frame(ISOCountry=isos,PC1=pca_res$x[match(isos,df_pca$ISOCountry),1]) %>%
  left_join(df_all.3 %>% dplyr::select(ISOCountry,pro,AGEGR2)) %>%
  mutate(AGEGR3=case_when(AGEGR2=='0-<6m'~1,
                          AGEGR2=='6-<12m'~2,
                          AGEGR2=='12-<60m'~3))

ggplot(df_comp,aes(PC1,pro))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(.~.)

pcor_res <- pcor.test(df_comp$pro, df_comp$PC1, df_comp$AGEGR3, method="spearman")
pcor_res


# Fit the Distribution of Hosp Pro ----

df_all.2<-import('docs/df_all.2.xlsx')

p_pro<-ggplot(df_all.2, aes(x = pro)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1.2) +
  theme_minimal()+
  labs(title='pro')

p_logit<-ggplot(df_all.2, aes(x = logit(pro))) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1.2) +
  theme_minimal()+
  labs(title='logit(pro)')

p_log<-ggplot(df_all.2, aes(x = log(pro))) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1.2) +
  theme_minimal()+
  labs(title='log(pro)')

p_pro+p_logit+p_log

ggsave('plot/Dist_pro.png',dpi = 200,width = 2000,height = 1200,units = 'px')

## Beta ----

fit_beta<-fitdist(df_all.2_new$pro, "beta", method="mle")
AIC(fit_beta)
BIC(fit_beta)
logLik(fit_beta)
plot(fit_beta)
gofstat(fit_beta)

## Lognorm ----

fit_lognorm <- fitdist(df_all.2_new$pro, "lnorm", method = "mle")
AIC(fit_lognorm)
BIC(fit_lognorm)
logLik(fit_lognorm)
plot(fit_lognorm)

## Logitnorm ----

fit_logitnorm <- fitdist(df_all.2_new$pro, "logitnorm", method = "mle",
                         start = list(mu = mean(logit(df_all.2_new$pro)), sigma = sd(logit(df_all.2_new$pro))))

AIC(fit_logitnorm)
BIC(fit_logitnorm)
logLik(fit_logitnorm)
plot(fit_logitnorm)
gofstat(fit_logitnorm)


## Kumaraswamy ----

dcustom <- function(x, a, b, log=FALSE){
  if(any(x <=0 | x >=1)) stop("x must be in (0,1)")
  logdens <- log(a) + log(b) + (a-1)*log(x) + (b-1)*log(1-x^a)
  if(log) return(logdens) else return(exp(logdens))
}
pcustom <- function(x, a, b){
  if(any(x <= 0 | x >= 1)) stop("x must be in (0,1)")
  1 - (1 - x^a)^b
}

# 拟合
fit_kumar <- fitdist(
  df_all.2_new$pro,
  "custom",
  start = list(a=1, b=1),
  method = "mle"
)


list(fit_beta,fit_lognorm,fit_logitnorm,fit_kumar) %>% map_dbl(~logLik(.x))
gofstat(list(fit_beta,fit_lognorm,fit_logitnorm,fit_kumar))

# 1. 将所有拟合对象放入一个命名列表
fit_list <- list(
  "Beta" = fit_beta,
  "Log-normal" = fit_lognorm,
  "Logit-normal" = fit_logitnorm,
  "Kumaraswamy" = fit_kumar
)

# 2. 提取各项指标并整理为 Data Frame
summary_table <- data.frame(
  Model = names(fit_list),
  LogLik = map_dbl(fit_list, ~logLik(.x)),
  AIC = map_dbl(fit_list, ~AIC(.x)),
  BIC = map_dbl(fit_list, ~BIC(.x))
) %>%
  # 按照 AIC 从小到大排序（AIC 越小模型拟合越好）
  arrange(AIC) %>%
  # 计算相对于最优模型的 AIC 差值 (delta AIC)
  mutate(delta_AIC = AIC - min(AIC))

summary_table %>%
  export_flextable_word('docs/Fit_AIC.docx')

# Fit the logitnorm Distribution ----

fit_1<-fitdist(df_all.3 %>% filter(AGEGR2 == '0-<6m') %>% pull(pro), "logitnorm", method = "mle",
               start = list(mu = -1.4, sigma = 1.2))

fit_2<-fitdist(df_all.3 %>% filter(AGEGR2 == '6-<12m') %>% pull(pro), "logitnorm", method = "mle",
               start = list(mu = -1.4, sigma = 1.2))

fit_3<-fitdist(df_all.3 %>% filter(AGEGR2 == '12-<60m') %>% pull(pro), "logitnorm", method = "mle",
               start = list(mu = -1.4, sigma = 1.2))

fit_all<-fitdist(df_all.3 %>% pull(pro), "logitnorm", method = "mle",
               start = list(mu = -1.4, sigma = 1.2))

plot(fit_1)
plot(fit_2)
plot(fit_3)
plot(fit_all)

 
df_params<-list(fit_1,fit_2,fit_3,fit_all) %>% map_dfr(~coef(.x)) %>%
  mutate(AGEGR = c("0-<6m", "6-<12m", "12-<60m","0-<60m"))

age_breaks <- c("0-<6m", "6-<12m", "0-<60m")

df_plot<-df_params %>%
  pmap_dfr(function(AGEGR, mu, sigma){
    x_seq <- seq(0, 1, by = 0.005)
    tibble(
      AGEGR = AGEGR,
      x = x_seq,
      density = dlogitnorm(x_seq, mu=mu, sigma=sigma)
    )
  })

pdf_plot<-ggplot(subset(df_plot,AGEGR!='12-<60m'), aes(x = x, y = density, color = AGEGR)) +
  geom_line(size = 1.2) +
  labs(x = "Hospitalisation proportion", y = "Probability density",color='Age group') +
  theme_classic()+
  scale_color_lancet(alpha = .7, breaks = age_breaks)

pdf_plot
ggsave("plot/PDF_byage.jpg", dpi = 200,width = 1500,height = 1200,units = 'px')

## PDF and  CDF ----
df_plot_cdf <- df_params %>%
  pmap_dfr(function(AGEGR, mu, sigma){
    x_seq <- seq(0, 1, by = 0.005)
    density <- dlogitnorm(x_seq, mu=mu, sigma=sigma)
    cdf <- cumsum(density) * (x_seq[2] - x_seq[1])  # 累积积分
    tibble(
      AGEGR = AGEGR,
      x = x_seq,
      cdf = cdf
    )
  })

# 绘制CDF图
cdf_plot <- ggplot(subset(df_plot_cdf, AGEGR != '12-<60m'), aes(x = x, y = cdf, color = AGEGR)) +
  geom_line(size = 1.2) +
  labs(x = "Hospitalisation proportion", y = "Cumulative probability", color = 'Age group') +
  theme_classic()+
  scale_color_lancet(alpha = .7, breaks = age_breaks)

# 拼接两个图
(pdf_plot + cdf_plot) + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = 'right')

# 保存拼接后的图片
ggsave(
  filename = "plot/PDF_and_CDF_byage.tiff",
  device = "tiff",
  dpi = 300,              # SCI 标准分辨率
  width = 300,            # 宽度 (mm)
  height = 150,            # 高度 (mm)
  units = "mm",           # 单位使用毫米更精确
  compression = "lzw"     # 必须加 LZW 压缩，否则文件会极大且部分期刊不收
)

# 定义函数：提取 CDF 和 Q-Q 图
get_diagnostic_pair_cdf <- function(fit_obj, group_name) {
  
  # 1. 提取累积分布对比图 (CDF)
  # 使用 cdfcomp 提取理论与经验 CDF 的对比
  p_cdf <- cdfcomp(fit_obj, plotstyle = "ggplot", datacol = "grey60", fitcol = "red") + 
    theme_bw() + 
    labs(title = paste(group_name, ": CDF"), 
         x = "Hospitalisation Proportion", 
         y = "Cumulative Probability") +
    theme(plot.title = element_text(size = 10, face = "bold"),
          legend.position = 'NULL')
  
  # 2. 提取 Q-Q 图
  p_qq <- qqcomp(fit_obj, plotstyle = "ggplot") + 
    theme_bw() + 
    scale_color_manual(values = c("logitnorm" = "black")) +
    theme(legend.position = "none")+
    labs(title = paste(group_name, ": Q-Q Plot"), 
         x = "Theoretical Quantiles", 
         y = "Empirical Quantiles") +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  # 返回横向组合的一对图
  return(p_cdf + p_qq)
}

qqcomp(fit_1, plotstyle = "ggplot")+
  theme_bw() + 
  theme(legend.position = 'NULL')

# 生成三组年龄段的诊断图
pair_1   <- get_diagnostic_pair_cdf(fit_1, "0-<6m")
pair_3   <- get_diagnostic_pair_cdf(fit_3, "12-<60m")
pair_all <- get_diagnostic_pair_cdf(fit_all, "0-<60m")

# 纵向堆叠：每行显示一个年龄组的 (CDF + QQ)
(pair_1) / (pair_3) / (pair_all)

# 保存高分辨率图片
ggsave("plot/LogitNormal_CDF_QQ_Diagnostics.png", 
       dpi = 200,width = 1200,height = 1500,units = 'px')

# Calculate PC1 ----
final_df_pca<-df_pca_grid %>% slice_max(var_dim1) %>% pull(df_pca) %>% pluck(1)
final_pca_res<-df_pca_grid %>% slice_max(var_dim1) %>% pull(pca_res) %>% pluck(1)

as.matrix(final_df_pca[,-1]) %*% final_pca_res$rotation[,1] %>% as.vector() ==  as.vector(final_pca_res$x[,1])

# vars
# vars_mean<-sapply(df_pca[vars], function(x) attr(x, "scaled:center"))
# vars_sd<-sapply(df_pca[vars], function(x) attr(x, "scaled:scale"))

# df_lmic_imputed_scaled<-df_lmic_imputed %>%
#   mutate(across(all_of(vars), list(scaled=~(log(.) - vars_mean[cur_column()]) / vars_sd[cur_column()])))
# 
# df_lmic_imputed_scaled %>%
#   dplyr::select(all_of(paste0(vars,'_scaled'))) %>% 
#   as.matrix() %*% pca_res$rotation[,1] %>% as.vector()
# 
# df_lmic_imputed_pc1<-df_lmic_imputed_scaled %>%
#   mutate(PC1=df_lmic_imputed_scaled %>%
#            dplyr::select(all_of(paste0(vars,'_scaled'))) %>% 
#            as.matrix() %*% pca_res$rotation[,1] %>% as.vector()) %>%
#   mutate(percentile=pnorm(PC1,mean=mean(PC1,na.rm=T),sd=sd(PC1,na.rm=T)))

df_pc1<-data.frame(ISOCountry=final_df_pca$ISOCountry,PC1=final_pca_res$x[,1])

df_lmic_imputed_pc1<-df_lmic_imputed %>%
    left_join(df_pc1)

df_lmic_imputed_pc1 %>% dplyr::select(all_of(vars),PC1) %>% 
  mutate(across(c(everything()),~.x)) %>%
  pairs.panels(method = "pearson", # correlation method
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )

## Fit PC1 Distrubution ----

fit_pc1<-fitdist(df_lmic_imputed_pc1$PC1, "norm", method="mle")
coef(fit_pc1)

## Percentile and qlogitnorm ----
    
df_params$mu
df_params$sigma

df_lmic_final<-df_lmic_imputed_pc1 %>%
  mutate(percentile=1-pnorm(PC1,mean=0,sd=1.87),
         qlogitnorm_0060=qlogitnorm(percentile,mu=df_params$mu[4],sigma=df_params$sigma[4]),
         qlogitnorm_0006=qlogitnorm(percentile,mu=df_params$mu[1],sigma=df_params$sigma[1]),
         qlogitnorm_0612=qlogitnorm(percentile,mu=df_params$mu[2],sigma=df_params$sigma[2]),
         )

# Random PC1 1000----
df_pca_10<-df_pca_grid %>% slice_max(var_dim1,n=10) %>%
  mutate(df_pc1=map2(df_pca,pca_res,~{
    df_pca<-.x
    pca_res<-.y
    data.frame(ISOCountry=df_pca$ISOCountry,PC1=pca_res$x[,1])
  }),
  sd_pc1=map_dbl(df_pc1,~{
    coef(fitdist(.x[,2], "norm", method="mle"))[2]
  }),
  df_lmic_final=map2(df_pc1,sd_pc1,~{
    .x %>%
      mutate(percentile=1-pnorm(PC1,mean=0,sd=.y),
             qlogitnorm_0060=qlogitnorm(percentile,mu=df_params$mu[4],sigma=df_params$sigma[4]),
             qlogitnorm_0006=qlogitnorm(percentile,mu=df_params$mu[1],sigma=df_params$sigma[1]),
             qlogitnorm_0612=qlogitnorm(percentile,mu=df_params$mu[2],sigma=df_params$sigma[2]),
      )
  }))

rio::export(df_pca_10,'rda/df_pca_10.rds')


# Method 3 (final used) ----
# Using 1000 Incidence Rate sample
## Import Incidence rate data 2----
RF.res.impute_1000<-import('rda/RF.res.impute2.rds',trust=T) # community incidence

RF.res.impute.minimal_1000<-RF.res.impute_1000 %>% dplyr::select(
  Income2019,ISOCountry,AGEGR,index,pop,N
)

set.seed(1234)
df_limi_final_withNO.INC_1000<-data.frame(index=1:1000,PC_Cand=sample(1:10,1000,replace = T,prob=df_pca_10$var_dim1)) %>%
  left_join(df_pca_10 %>% select(row_id,df_lmic_final),by=c('PC_Cand'='row_id')) %>%
  unnest(df_lmic_final) %>%
  left_join(df_lmic_imputed) %>%
  left_join(RF.res.impute.minimal_1000 %>% pivot_wider(id_cols = c(ISOCountry,index),names_from = c(AGEGR),names_prefix = 'NO.INC_',
                                                       values_from = N)) %>%
  arrange(ISOCountry,index) %>%
  relocate(ISOCountry,index,starts_with('NO.INC_'),starts_with('qlogitnorm')) %>%
  mutate(`pseudo_NO.HOS_0-<60m`=`NO.INC_0-<60m`*qlogitnorm_0060,
         `pseudo_NO.HOS_0-<6m`=`NO.INC_0-<6m`*qlogitnorm_0006,
         `pseudo_NO.HOS_6-<12m`=`NO.INC_6-<12m`*qlogitnorm_0612,.after=qlogitnorm_0612) %>%
  group_by(Income2019,index) %>%
  mutate(across(starts_with('pseudo_NO.HOS'),~.x/sum(.x),.names = 'share_of_{.col}'),.after=index) %>%
  ungroup()

hos_rate_ALRI_Income.meta<-import('rda/hos_rate_ALRI_Income.meta.rds',trust=T)
hos_rate_ALRI_Income.meta.minimal<-hos_rate_ALRI_Income.meta %>%
  dplyr::filter(Group !='LMIC',!(AGEGR=='0-<60m' & Impute==0))

df_hos_sample<-hos_rate_ALRI_Income.meta.minimal %>%
  transmute(AGEGR,Income2019=Group,Pop,est,se) %>%
  mutate(HR=map2(est,se,~exp(rnorm(1000,.x,.y))*1000),
         Hos=map2(HR,Pop,~.x*.y))

df_share_1000<-df_limi_final_withNO.INC_1000 %>%
  dplyr::select(index,ISOCountry,Income2019,starts_with('pop_'),starts_with('share_of')) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,.after = pop_1260)

df_hos_sample_nest_1000 <- df_hos_sample %>%
  unnest_longer(c(Hos,HR)) %>%                 # 展开 Hos 列
  group_by(AGEGR, Income2019, Pop, est, se) %>%  # 按原行分组
  mutate(index = row_number()) %>%       # 给每个样本加 index 1~1000
  ungroup() %>%
  nest(data=-index) %>%
  mutate(data_wide=map(data,~{
    df<-dplyr::select(.x,Income2019,Hos,AGEGR)
    pivot_wider(df,id_cols = Income2019,values_from = Hos,names_from = AGEGR,names_prefix = 'NO.HOS_')
  })) %>%
  mutate(res=imap(data_wide,~{
    cat(.y,'\n')
    df_share_1000 %>% filter(index==.y) %>% left_join(.x) %>%
      mutate(`Hos_0-<60m`=`NO.HOS_0-<60m`*`share_of_pseudo_NO.HOS_0-<60m`,
             `Hos_0-<6m`=`NO.HOS_0-<6m`*`share_of_pseudo_NO.HOS_0-<6m`,
             `Hos_6-<12m`=`NO.HOS_6-<12m`*`share_of_pseudo_NO.HOS_6-<12m`,
             `Hos_12-<60m`=`Hos_0-<60m`-`Hos_0-<6m`-`Hos_6-<12m`,
             `Hos_0-<12m`=`Hos_0-<6m`+`Hos_6-<12m`)
  }))

df_hos_sample_final_1000<-df_hos_sample_nest_1000 %>%
  dplyr::select(res) %>%
  unnest(res) %>%
  mutate(`Rate_0-<60m` = `Hos_0-<60m`/pop_0060,
         `Rate_0-<6m` = `Hos_0-<6m`/pop_0006,
         `Rate_6-<12m` = `Hos_6-<12m`/pop_0612,
         `Rate_12-<60m` = `Hos_12-<60m`/pop_1260,
         `Rate_0-<12m` = `Hos_0-<12m`/pop_0012)

## df_scaling ----

df_scaling<-df_limi_final_withNO.INC_1000 %>%
  summarise(across(starts_with('pseudo_'),sum),.by=c(Income2019,index)) %>%
  pivot_longer(cols=-c(Income2019,index),names_prefix = 'pseudo_NO.HOS_',names_to = 'AGEGR',values_to = 'pseudo_NO.HOS') %>%
  left_join(df_hos_sample %>%
              unnest_longer(c(Hos,HR)) %>%                 # 展开 Hos 列
              group_by(AGEGR, Income2019, Pop, est, se) %>%  # 按原行分组
              mutate(index = row_number()) %>%       # 给每个样本加 index 1~1000
              ungroup() %>% transmute(index,AGEGR,Income2019,real_NO.HOS=Hos)) %>%
  mutate(scaling_factor=real_NO.HOS/pseudo_NO.HOS) %>%
  arrange(AGEGR,Income2019)

head(df_scaling)

ggplot(df_scaling, aes(x = scaling_factor, fill = Income2019)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~AGEGR, scales = "free") +
  labs(x = "Scaling Factor", y = "Density", fill='Income level'
       #title = "Scaling Factor Distribution by Age Group and Income"
       ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave('plot/Hos_scaling_factor.tiff', width = 10, height = 6, dpi = 300,compression = "lzw")

ggplot(df_scaling, aes(x = scaling_factor, fill = AGEGR)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Income2019, scales = "free") +
  labs(x = "Scaling Factor", y = "Density", 
       title = "Scaling Factor Distribution by Age Group and Income") +
  theme_minimal() +
  theme(legend.position = "bottom")


df_hos_by_country2_1000<-df_hos_sample_final_1000 %>%
  summarise(across(matches('^(Rate_|Hos_)'),list(q500=~quantile(.x,.5),
                                                 q025=~quantile(.x,.025),
                                                 q975=~quantile(.x,.975),
                                                 q500pos=~{
                                                   pos_x<-.x[.x>0]
                                                   quantile(pos_x,.5)
                                                 },
                                                 q025pos=~{
                                                   pos_x<-.x[.x>0]
                                                   quantile(pos_x,.025)
                                                 },
                                                 q975pos=~{
                                                   pos_x<-.x[.x>0]
                                                   quantile(pos_x,.975)
                                                 })),.by=c(ISOCountry,Income2019)) %>%
  select(
    -matches("pos$"),      # 先选所有 _pos 列
    matches("12\\-<60m.*pos$")  # 再保留 12-<60m 的 _pos
  )

rio::export(df_hos_by_country2_1000,"rda/df_hos_by_country2_1000.rds")
rio::export(df_hos_sample_final_1000,"rda/df_hos_sample_final_1000.rds")

df_hos_by_country2.long_1000<-df_hos_by_country2_1000 %>%
  dplyr::select(ISOCountry,Income2019,matches('^(Hos|Rate)')) %>%
  pivot_longer(cols =matches('^(Hos|Rate)'),names_pattern = '(.*)_(.*)_(.*)',names_to = c('metric','AGEGR','quantile')) %>%
  mutate(type=recode(quantile,'q500'='est','q025'='lci','q975'='uci'))

df_hos_by_country2.long_1000 %>% 
  pivot_wider(id_cols = c(ISOCountry,Income2019,AGEGR),names_from = c(metric,type),values_from = value) %>%
  mutate(N.est=if_else(AGEGR!='12-<60m',Hos_est,Hos_q500pos),IR.est=if_else(AGEGR!='12-<60m',Rate_est,Rate_q500pos),
         N.lci=if_else(AGEGR!='12-<60m',Hos_lci,Hos_q025pos),IR.lci=if_else(AGEGR!='12-<60m',Rate_lci,Rate_q025pos),
         N.uci=if_else(AGEGR!='12-<60m',Hos_uci,Hos_q975pos),IR.uci=if_else(AGEGR!='12-<60m',Rate_uci,Rate_q975pos)) %>%
  select(ISOCountry,Income2019,AGEGR,starts_with('N.'),starts_with('IR.')) %>%
  mutate(across(starts_with('R.'),~round(.x,1)),
         across(starts_with('N.'),
                function(x) {
                  case_when(
                    x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                    x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                    TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                  )
                })) %>%
  mutate(N=sprintf('%s (%s–%s)', N.est, N.lci, N.uci),
         R=sprintf('%.1f (%.1f–%.1f)',IR.est, IR.lci, IR.uci),
         str=paste0(N,'\n',R)) %>%
  pivot_longer(cols = c(R,N)) %>%
  mutate(name=recode(name,'N'='Number of episodes','R'='Hospital admission rate')) %>%
  pivot_wider(id_cols = c(Income2019,ISOCountry,name),names_from = AGEGR,values_from = value) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName)) %>%
  arrange(Income2019,CountryName) %>%
  relocate(Income2019,Country=CountryName,name,`0-<6m`,`6-<12m`,`0-<12m`,`12-<60m`,`0-<60m`) %>%
  split(.,.$Income2019) %>%
  imap_dfr(~{
    bind_rows(data.frame(Country=switch(.y,'L'='Lower-income','LM'='Lower-middle-income','UM'='Upper-middle-income')),
              .x)
  }) %>%
  mutate(Country=ifelse(name!='Number of episodes' | is.na(name),paste0('',Country),'')) %>%
  select(Country,name,contains('-')) %>% 
  rename_with(~" ",.cols='name') %T>%  
  saveRDS("docs/Table_Hospitalisation_1000.rds") %>%
  export_flextable_word('docs/Table_Hospitalisation_1000.docx',orientation='land')

## by income ----
df_hos_by_country2.byincome<-bind_rows(
  df_hos_sample_final_1000 %>%
    select(index,ISOCountry,Income2019,starts_with('pop_'),starts_with('Hos_')) %>%
    summarise(across(where(is.numeric),sum),.by=c(Income2019,index)),
  df_hos_sample_final_1000 %>%
    select(index,ISOCountry,Income2019,starts_with('pop_'),starts_with('Hos_')) %>%
    summarise(across(where(is.numeric),sum),.by=c(index)) %>% mutate(Income2019='LMIC')
) %>%
  mutate(`Rate_0-<60m` = `Hos_0-<60m`/pop_0060,
         `Rate_0-<12m` = `Hos_0-<12m`/pop_0012,
         `Rate_0-<6m` = `Hos_0-<6m`/pop_0006,
         `Rate_6-<12m` = `Hos_6-<12m`/pop_0612,
         `Rate_12-<60m` = `Hos_12-<60m`/pop_1260) %>%
  summarise(across(matches('^(Rate_|Hos_)'),list(q500=~quantile(.x,.5),
                                                 q025=~quantile(.x,.025),
                                                 q975=~quantile(.x,.975),
                                                 q500pos=~{
                                                   pos_x<-.x[.x>0]
                                                   quantile(pos_x,.5)
                                                 },
                                                 q025pos=~{
                                                   pos_x<-.x[.x>0]
                                                   quantile(pos_x,.025)
                                                 },
                                                 q975pos=~{
                                                   pos_x<-.x[.x>0]
                                                   quantile(pos_x,.975)
                                                 })),.by=c(Income2019)) %>%
  select(
    -matches("pos$"),      # 先选所有 _pos 列
    matches("12\\-<60m.*pos$")  # 再保留 12-<60m 的 _pos
  ) %>%
  select(-matches('(Hos|Rate)_12-<60m_q\\d+$')) %>%
  rename_with(~str_remove(.x,'pos'))

df_hos_by_country2.byincome %>%
  dplyr::select(Income2019,matches('^(Hos|Rate)')) %>%
  pivot_longer(cols =matches('^(Hos|Rate)'),names_pattern = '(.*)_(.*)_(.*)',names_to = c('metric','AGEGR','quantile')) %>%
  mutate(type=recode(quantile,'q500'='est','q025'='lci','q975'='uci')) %>%
  pivot_wider(id_cols = c(Income2019,AGEGR),names_from = c(metric,type),values_from = value) %>%
  rename_with(~ str_replace(.x, "^Hos_", "N.")) %>%
  rename_with(~ str_replace(.x, "^Rate_", "IR.")) %>%
  mutate(across(starts_with('R.'),~round(.x,2)),
         across(starts_with('N.'),
                function(x) {
                  case_when(
                    x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                    x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                    TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                  )
                })) %>%
  mutate(N=sprintf('%s (%s–%s)', N.est, N.lci, N.uci),
         R=sprintf('%.1f (%.1f–%.1f)',IR.est, IR.lci, IR.uci),
         str=paste0(N,'\n',R)) %>%
  pivot_wider(id_cols = c(Income2019),names_from = AGEGR,values_from = str) %>%
  transmute(`Income Level`=paste0(Income2019,if_else(Income2019=='LMIC','','IC')),`0-<6m`, `6-<12m`,`0-<12m`,`12-<60m`, `0-<60m`) %T>%
  rio::export('docs/Table_Hospitalisation.byincome.rds') %>%
  export_flextable_word('docs/Table_Hospitalisation.byincome.docx',orientation='land')

save.image(file='rda/code_03_estimate_hospital_admission_by_country.RData')

