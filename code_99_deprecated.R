# code_03_esitimated

## Method 1 ----
# Use total hospital admission number(est lci and uci) by income level
df_hos_by_country<-df_limi_final_withNO.INC %>%
  dplyr::select(ISOCountry,Income2019,starts_with('pop_'),starts_with('share_of')) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260) %>%
  left_join( hos_rate_ALRI_Income.meta.minimal %>%
               transmute(AGEGR,Income2019=Group,Pop,N.est,N.lci,N.uci) %>%
               pivot_wider(
                 id_cols = c(Income2019),
                 names_from = AGEGR,
                 values_from = c(N.est, N.lci, N.uci),
                 names_glue = "{.value}_{AGEGR}"
               )) %>%
  mutate(`Hos.est_0-<60m`=`N.est_0-<60m`*`share_of_pseudo_NO.HOS_0-<60m`,
         `Hos.lci_0-<60m`=`N.lci_0-<60m`*`share_of_pseudo_NO.HOS_0-<60m`,
         `Hos.uci_0-<60m`=`N.uci_0-<60m`*`share_of_pseudo_NO.HOS_0-<60m`,
         `Hos.est_0-<6m`=`N.est_0-<6m`*`share_of_pseudo_NO.HOS_0-<6m`,
         `Hos.lci_0-<6m`=`N.lci_0-<6m`*`share_of_pseudo_NO.HOS_0-<6m`,
         `Hos.uci_0-<6m`=`N.uci_0-<6m`*`share_of_pseudo_NO.HOS_0-<6m`,
         `Hos.est_6-<12m`=`N.est_6-<12m`*`share_of_pseudo_NO.HOS_6-<12m`,
         `Hos.lci_6-<12m`=`N.lci_6-<12m`*`share_of_pseudo_NO.HOS_6-<12m`,
         `Hos.uci_6-<12m`=`N.uci_6-<12m`*`share_of_pseudo_NO.HOS_6-<12m`,
         `Hos.est_12-<60m`=`Hos.est_0-<60m`-`Hos.est_0-<6m`-`Hos.est_6-<12m`,
         `Hos.lci_12-<60m`=`Hos.lci_0-<60m`-`Hos.lci_0-<6m`-`Hos.lci_6-<12m`,
         `Hos.uci_12-<60m`=`Hos.uci_0-<60m`-`Hos.uci_0-<6m`-`Hos.uci_6-<12m`) %>%
  mutate(`Rate.est_0-<60m` = `Hos.est_0-<60m`/pop_0060,
         `Rate.lci_0-<60m` = `Hos.lci_0-<60m`/pop_0060,
         `Rate.uci_0-<60m` = `Hos.uci_0-<60m`/pop_0060,
         `Rate.est_0-<6m`  = `Hos.est_0-<6m`  / pop_0006,
         `Rate.lci_0-<6m`  = `Hos.lci_0-<6m`  / pop_0006,
         `Rate.uci_0-<6m`  = `Hos.uci_0-<6m`  / pop_0006,
         `Rate.est_6-<12m` = `Hos.est_6-<12m` / pop_0612,
         `Rate.lci_6-<12m` = `Hos.lci_6-<12m` / pop_0612,
         `Rate.uci_6-<12m` = `Hos.uci_6-<12m` / pop_0612,
         `Rate.est_12-<60m` = `Hos.est_12-<60m` / pop_1260,
         `Rate.lci_12-<60m` = `Hos.lci_12-<60m` / pop_1260,
         `Rate.uci_12-<60m` = `Hos.uci_12-<60m` / pop_1260
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName)) %>%
  relocate(ISOCountry,CountryName,Income2019,starts_with('pop'),matches('Hos\\.est'),matches('Hos\\.lci'),matches('Hos\\.uci'),
           matches('Rate\\.est'),matches('Rate\\.lci'),matches('Rate\\.uci'))


df_hos_by_country.long<-df_hos_by_country %>%
  dplyr::select(ISOCountry,Income2019,matches('^(Hos|Rate)')) %>%
  pivot_longer(cols =matches('^(Hos|Rate)'),names_pattern = '(.*)\\.(.*)_(.*)',names_to = c('metric','type','AGEGR')) 

df_hos_by_country %>%
  filter(if_any(starts_with("Hos."), ~ .x < 0)) %>%
  relocate(ISOCountry,CountryName,Income2019,starts_with('pop'),
           matches('Hos.*0-<60m'),matches('Hos.*0-<6m'),matches('Hos.*6-<12m'),matches('Hos.*12-<60m'),
           matches('Rate.*0-<60m'),matches('Rate.*0-<6m'),matches('Rate.*6-<12m'),matches('Rate.*12-<60m'),
  ) %>%
  rio::export('docs/df_hos_by_country_negative_copy.xlsx')

## Method 2 ----
# sampling hospital admission rate by income level
set.seed(1234)
df_hos_sample<-hos_rate_ALRI_Income.meta.minimal %>%
  transmute(AGEGR,Income2019=Group,Pop,est,se) %>%
  mutate(HR=map2(est,se,~exp(rnorm(1000,.x,.y))*1000),
         Hos=map2(HR,Pop,~.x*.y))

df_share<-df_limi_final_withNO.INC %>%
  dplyr::select(ISOCountry,Income2019,starts_with('pop_'),starts_with('share_of')) %>%
  mutate(pop_0060=pop_0006+pop_0612+pop_1260,pop_0012=pop_0006+pop_0612,.after = pop_1260)


df_hos_sample_nest <- df_hos_sample %>%
  unnest_longer(c(Hos,HR)) %>%                 # 展开 Hos 列
  group_by(AGEGR, Income2019, Pop, est, se) %>%  # 按原行分组
  mutate(index = row_number()) %>%       # 给每个样本加 index 1~1000
  ungroup() %>%
  nest(data=-index) %>%
  mutate(data_wide=map(data,~{
    df<-dplyr::select(.x,Income2019,Hos,AGEGR)
    pivot_wider(df,id_cols = Income2019,values_from = Hos,names_from = AGEGR,names_prefix = 'NO.HOS_')
  })) %>%
  mutate(res=map(data_wide,~{
    df_share %>% left_join(.x) %>%
      mutate(`Hos_0-<60m`=`NO.HOS_0-<60m`*`share_of_pseudo_NO.HOS_0-<60m`,
             `Hos_0-<6m`=`NO.HOS_0-<6m`*`share_of_pseudo_NO.HOS_0-<6m`,
             `Hos_6-<12m`=`NO.HOS_6-<12m`*`share_of_pseudo_NO.HOS_6-<12m`,
             `Hos_12-<60m`=`Hos_0-<60m`-`Hos_0-<6m`-`Hos_6-<12m`,
             `Hos_0-<12m`=`Hos_0-<6m`+`Hos_6-<12m`)
  }))

df_hos_sample_final<-df_hos_sample_nest %>%
  dplyr::select(index,res) %>%
  unnest(res) %>%
  mutate(`Rate_0-<60m` = `Hos_0-<60m`/pop_0060,
         `Rate_0-<6m` = `Hos_0-<6m`/pop_0006,
         `Rate_6-<12m` = `Hos_6-<12m`/pop_0612,
         `Rate_12-<60m` = `Hos_12-<60m`/pop_1260,
         `Rate_0-<12m` = `Hos_0-<12m`/pop_0012)



df_hos_by_country2<-df_hos_sample_final %>%
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

### By income level ----
df_hos_by_country2.byincome<-bind_rows(
  df_hos_sample_final %>%
    select(index,ISOCountry,Income2019,starts_with('pop_'),starts_with('Hos_')) %>%
    summarise(across(where(is.numeric),sum),.by=c(Income2019,index)),
  df_hos_sample_final %>%
    select(index,ISOCountry,Income2019,starts_with('pop_'),starts_with('Hos_')) %>%
    summarise(across(where(is.numeric),sum),.by=c(index)) %>% mutate(Income2019='LMIC')
) %>%
  mutate(`Rate_0-<60m` = `Hos_0-<60m`/pop_0060,
         `Rate_0-<6m` = `Hos_0-<6m`/pop_0006,
         `Rate_6-<12m` = `Hos_6-<12m`/pop_0612,
         `Rate_12-<60m` = `Hos_12-<60m`/pop_1260) %>%
  summarise(across(matches('^(Rate_|Hos_)'),list(q500=~quantile(.x,.5),
                                                 q025=~quantile(.x,.025),
                                                 q975=~quantile(.x,.975))),
            .by=c(Income2019))


df_hos_by_country2 %>%
  filter(if_any(starts_with('Hos_12-<60m'),~.x<0)) %>%
  rio::export('docs/df_hos_by_country2_nagative.xlsx')


df_hos_by_country2.long<-df_hos_by_country2 %>%
  dplyr::select(ISOCountry,Income2019,matches('^(Hos|Rate)')) %>%
  pivot_longer(cols =matches('^(Hos|Rate)'),names_pattern = '(.*)_(.*)_(.*)',names_to = c('metric','AGEGR','quantile')) %>%
  mutate(type=recode(quantile,'q500'='est','q025'='lci','q975'='uci'))

df_hos_by_country.compare<-bind_rows(list('Method1'=df_hos_by_country.long,'Method2'=df_hos_by_country2.long),.id = 'method')

df_hos_by_country.compare %>%
  filter(ISOCountry=='CHN',metric=='Rate') %>%
  ggplot(aes(type,value,fill=method))+
  geom_col(position = 'dodge')+
  facet_wrap(vars(AGEGR))

df_hos_by_country.compare %>%
  filter(metric=='Rate',AGEGR=='12-<60m',type=='lci') %>%
  mutate(ISOCountry = fct_reorder(ISOCountry, value)) %>%
  ggplot(aes(ISOCountry,value,fill=method))+
  geom_col(position = 'dodge')+
  coord_flip()

rio::export(df_hos_by_country2,"rda/df_hos_by_country2.rds")

### Export Table ----
df_hos_by_country2.long %>% 
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
  rename_with(~" ",.cols='name')
#export_flextable_word('docs/Table_Hospitalisation.docx',orientation='land')

# df_hos_by_country2.byincome %>%
#   dplyr::select(Income2019,matches('^(Hos|Rate)')) %>%
#   pivot_longer(cols =matches('^(Hos|Rate)'),names_pattern = '(.*)_(.*)_(.*)',names_to = c('metric','AGEGR','quantile')) %>%
#   mutate(type=recode(quantile,'q500'='est','q025'='lci','q975'='uci')) %>%
#   pivot_wider(id_cols = c(Income2019,AGEGR),names_from = c(metric,type),values_from = value) %>%
#   rename_with(~ str_replace(.x, "^Hos_", "N.")) %>%
#   rename_with(~ str_replace(.x, "^Rate_", "IR.")) %>%
#   mutate(across(starts_with('R.'),~round(.x,2)),
#          across(starts_with('N.'),
#                 function(x) {
#                   case_when(
#                     x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
#                     x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
#                     TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
#                   )
#                 })) %>%
#   mutate(N=sprintf('%s (%s–%s)', N.est, N.lci, N.uci),
#          R=sprintf('%.2f (%.2f–%.2f)',IR.est, IR.lci, IR.uci),
#          str=paste0(N,'\n',R)) %>%
#   pivot_wider(id_cols = c(Income2019),names_from = AGEGR,values_from = str) %>%
#   transmute(`Income Level`=paste0(Income2019,if_else(Income2019=='LMIC','','IC')),`0-<6m`, `6-<12m`, `12-<60m`, `Total`=`0-<60m`) %>%
#   export_flextable_word('docs/Table_Hospitalisation.byincome.docx',orientation='land')


# Calculation of Scaling Factor to Match Total Hospitalisations ----
## Import Incidence rate data ----
RF.res.impute<-import('rda/RF.res.impute.rds',trust=T) # community incidence
hos_rate_ALRI_Income.meta<-import('rda/hos_rate_ALRI_Income.meta.rds',trust=T)

RF.res.impute.minimal<-RF.res.impute %>% dplyr::select(
  Income2019,ISOCountry,AGEGR,pop,N.est
)

hos_rate_ALRI_Income.meta.minimal<-hos_rate_ALRI_Income.meta %>%
  dplyr::filter(Group !='LMIC',!(AGEGR=='0-<60m' & Impute==0))

df_limi_final_withNO.INC<-left_join(df_lmic_final,RF.res.impute.minimal %>% pivot_wider(id_cols = ISOCountry,names_from = c(AGEGR),names_prefix = 'NO.INC_',
                                                                                        values_from = N.est)) %>%
  mutate(`pseudo_NO.HOS_0-<60m`=`NO.INC_0-<60m`*qlogitnorm_0060,
         `pseudo_NO.HOS_0-<6m`=`NO.INC_0-<6m`*qlogitnorm_0006,
         `pseudo_NO.HOS_6-<12m`=`NO.INC_6-<12m`*qlogitnorm_0612) %>%
  group_by(Income2019) %>%
  mutate(across(starts_with('pseudo_NO.HOS'),~.x/sum(.x),.names = 'share_of_{.col}')) %>%
  ungroup()

df_limi_final_withNO.INC %>% summarise(across(starts_with('share_of_'),sum),.by=Income2019)

df_scaling<-df_limi_final_withNO.INC %>%
  summarise(across(starts_with('pseudo_'),sum),.by=Income2019) %>%
  pivot_longer(cols=-Income2019,names_prefix = 'pseudo_NO.HOS_',names_to = 'AGEGR',values_to = 'pseudo_NO.HOS') %>%
  left_join(hos_rate_ALRI_Income.meta.minimal %>% transmute(AGEGR,Income2019=Group,real_NO.HOS=N.est)) %>%
  mutate(scaling_factor=real_NO.HOS/pseudo_NO.HOS) %>%
  arrange(AGEGR,Income2019)

# Hospitalisation NO and rate ----



# legend 
# 创建 dummy 数据，包含所有需要展示的 Color 和 Shape 分类
# 1. 创建两个独立长度的数据框（或者直接用扩展组合）
df_country <- data.frame(
  Country = factor(names(country_colors), levels = names(country_colors)),
  x = 1, y = 1,
  xmin = 0.8, xmax = 1.2, # X 轴范围
  ymin = 0.8, ymax = 1.2  # Y 轴范围
)

df_type <- data.frame(
  valid_type2 = factor(names(type_shapes), levels = names(type_shapes)),
  x = 1, y = 1
)

# ==============================================================================
# 3. 构建包含“十字线 + 散点”的独立图例 p_legend
# ==============================================================================
p_for_legend <- ggplot() +
  # --- 1. 竖直误差棒 (Y 轴方向) ---
  geom_linerange(
    data = df_country, 
    aes(x = x, ymin = ymin, ymax = ymax, color = Country), 
    linewidth = 0, alpha = 0
  ) +
  # --- 2. 水平误差棒 (X 轴方向) ---
  geom_errorbarh(
    data = df_country, 
    aes(y = y, xmin = xmin, xmax = xmax, color = Country), 
    linewidth = 0, alpha = 0, height = 0 # height=0 保证线段两头没有横帽，纯直线
  ) +
  # --- 3. 中心散点 ---
  geom_point(
    data = df_country, 
    aes(x = x, y = y, color = Country), 
    size = 0, alpha = .5
  ) +
  
  # --- 4. Study Type 形状图层 ---
  geom_point(
    data = df_type, 
    aes(x = x, y = y, shape = valid_type2), 
    size = 0, alpha = .5
  ) +
  
  # 手动设置标度
  scale_color_manual(
    values = country_colors, 
    name = "Country", 
    drop = FALSE
  ) +
  scale_shape_manual(
    values = type_shapes, 
    labels = type_labels,
    name = "Study Type", 
    drop = FALSE
  ) +
  theme_void() + # 移除所有坐标轴、背景和外框
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"), # 四周边距全部归零
    legend.position = "left",               # 图例靠左贴紧
    legend.box.margin = margin(0, 0, 0, 0, "pt")
  )+
  
  # ==============================================================================
# 关键点：通过 override.aes 激活十字线 + 散点的组合 Key
# ==============================================================================
guides(
  color = guide_legend(
    order = 1, 
    ncol = 1, 
    direction = "vertical",
    override.aes = list(
      linewidth = 0.6, # 十字交叉线的粗细
      size = 3,        # 中心圆点的大小
      alpha = 1        # 保证可见
    )
  ),
  shape = guide_legend(
    order = 2, 
    ncol = 1, 
    direction = "vertical",
    override.aes = list(
      size = 3.5, 
      alpha = 1
    )
  )
)
p_legend<-ggdraw(cowplot::get_legend(p_for_legend))
