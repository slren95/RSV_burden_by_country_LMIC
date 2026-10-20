rm(list=ls())
library(tidyverse)
library(rio)
library(viridis)
library(patchwork)
library(scales)
library(grid)

df_sum_all_DeCoDe<-import('rda/df_sum_all_DeCoDe.rds',trust=T)
df_sum_all_NP<-import('rda/df_sum_all_NP.rds',trust=T)
RF.res.impute2<-import('rda/RF.res.impute2.rds',trust=T)
df_hos_by_country2_1000<-import("rda/df_hos_by_country2_1000.rds",trust=T)

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

## mortality ----
df_plot_mort_att<-df_sum_all_DeCoDe %>%
  select(ISOCountry,Income2019,CountryName,ends_with('500')) %>%
  pivot_longer(
    cols = starts_with("m"),
    names_to = c("AGEGR", "type"),           # 两个捕获组 → 两列
    names_pattern = "m(.*)_([NR])_500",    # 注意 [NR]，匹配 N 或 R
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
  mutate(AGEGR=recode(AGEGR,"0006" = "0-<6m","0612" = "6-<12m","1260" = "12-<60m","0060"="0-<60m","0012"="0-<12m")) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region)) %>%
  mutate(R=100*R)

df_plot_mort_ass<-df_sum_all_NP %>%
  select(ISOCountry,Income2019,CountryName,ends_with('500')) %>%
  pivot_longer(
    cols = starts_with("m"),
    names_to = c("AGEGR", "type"),           # 两个捕获组 → 两列
    names_pattern = "m(.*)_([NR])_500",    # 注意 [NR]，匹配 N 或 R
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
  mutate(AGEGR=recode(AGEGR,"0006" = "0-<6m","0612" = "6-<12m","1260" = "12-<60m","0060"="0-<60m","0012"="0-<12m")) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region))  %>%
  mutate(R=100*R)

## incidence ----
df_plot_inc<-RF.res.impute2 %>%
  reframe(across(c(IR,N),list(q500=~quantile(.x,0.5),
                              q025=~quantile(.x,0.025),
                              q975=~quantile(.x,0.975))),.by = c(ISOCountry,Income2019,AGEGR)) %>%
  transmute(ISOCountry,AGEGR,Income2019,N=N_q500,R=IR_q500) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region,CountryName))


## hospitalisation ----
df_plot_hos<-df_hos_by_country2_1000 %>%
  select(ISOCountry,Income2019,contains('q500'),-c(`Rate_12-<60m_q500`,`Hos_12-<60m_q500`)) %>% 
  pivot_longer(
    cols = contains("_"),
    names_to = c("type", "AGEGR"),           # 两个捕获组 → 两列
    names_pattern = "(.*)_(.*)_q500.*",    # 注意 [NR]，匹配 N 或 R
    values_to = "value"
  ) %>%
  mutate(type = case_when(
    type == "Hos" ~ "N",
    type == "Rate" ~ "R"
  )) %>%
  pivot_wider(id_cols = c(ISOCountry,Income2019,AGEGR),
              names_from = type,
              values_from = value
  ) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,region))

# Global average ----
df_global_mean_mort<-import('rda/df_global_mean_mort.rds',trust=T)

df_global_average <- data.frame(
  AGEGR = c("0-<6m", "6-<12m", "0-<12m", "12-<60m", "0-<60m"),
  
  # Incidence rate (/1000)
  inc_R_est = c(96.3, 82.6, 94.6, NA, 48.8),
  inc_R_lci = c(67.9, 60.8, 70.8, NA, 37.4),
  inc_R_uci = c(142.6, 116.9, 131.6, NA, 65.9),
  
  inc_N_est = c(6554000, 5619000, 12875000, 20510000, 33028000),
  inc_N_lci = c(4620000, 4135000, 9635000, 15744000, 25353000),
  inc_N_uci = c(9702000, 7953000, 17909000, 27670000, 44638000),
  
  # Hospitalization rate (/1000)
  hos_R_est = c(20.2, 10.0, 15.9, 1.5, 5.3),
  hos_R_lci = c(14.9, 7.4, 12.6, 1.1, 4.2),
  hos_R_uci = c(29.1, 14.3, 21.2, 2.2, 6.8),
  
  hos_N_est = c(1376000, 683000, 2170000, 827000, 3567000),
  hos_N_lci = c(1017000, 507000, 1713000, 600000, 2856000),
  hos_N_uci = c(1982000, 973000, 2882000, 1207000, 4634000),
  
  # Attributable death rate (%)
  
  mort_att_N_est = c(45700, 20600, 66300, 35100, 101400),
  mort_att_N_lci = c(38400, 16800, 55200, 29100, 84500),
  mort_att_N_uci = c(55900, 26000, 82000, 43600, 125200),
  
  # All-cause associated death rate (%)
  
  mort_ass_N_est = c(102000, 46400, 148500, 80600, 229000),
  mort_ass_N_lci = c(88800, 38900, 128000, 68000, 196000),
  mort_ass_N_uci = c(118800, 56300, 174900, 96600, 271200)
) %>%
  left_join(df_global_mean_mort %>% select(AGEGR,contains('mort_att_R_'),contains('mort_ass_R_')) %>%
              mutate(across(where(is.numeric),~.x*100)))

df_range<-c('inc','hos','mort_att','mort_ass') %>% 
  map_dfr(~{
    message(.x)
    df_plot_mort_att <- get(paste0('df_plot_', .x))
    df_plot_mort_att %>%
      summarise(across(c(N,R),list(min=min,max=max)),.by = AGEGR) %>%
      mutate(metric=.x)
  }) %>%
  left_join(
    df_global_average %>%
      dplyr::select(AGEGR,contains('est')) %>%
      pivot_longer(cols = contains('est'),names_pattern = "(.*)_(N|R)_est",names_to = c('metric','.value')) %>%
      rename(R_avg=R,N_tot=N)
  ) %>%
  relocate(AGEGR,metric,R_min,R_avg,R_max,N_min,N_tot,N_max) %>%
  mutate(abnormal=case_when(R_avg<R_min~'Rmin_',
                            R_avg>R_max~'Rmax_',
                            T~''))

df_range %>% filter(metric=='hos') %>%
  select(metric,AGEGR,R_min,R_max,R_avg,N_min,N_max)


## map plot ----

world_map <- map_data("world") %>%
  mutate(region=case_when(region=='Taiwan'~'China',T~region))

legend_breaks_labels <- list(
  # ---------------- inc ----------------
  # R values
  inc_0_60m_R = list(breaks = c(35, 40, 48.8, 60, 90), labels = c("Low\n(35)", "40", "▲\n(48.8)", "60", "High\n(90)"), limits = c(35, 90)),
  inc_0_6m_R = list(breaks = c(60, 80, 96.3, 120, 200), labels = c("Low\n(60)", "80", "▲\n(96.3)", "120", "High\n(200)"), limits = c(60, 200)),
  inc_6_12m_R = list(breaks = c(50, 70, 82.6, 100, 140), labels = c("Low\n(50)", "70", "▲\n(82.6)", "100", "High\n(140)"), limits = c(50, 140)),
  inc_12_60m_R = list(breaks = c(25, 40, 50, 65), labels = c("Low\n(25)", "40", "50", "High\n(65)"), limits = c(25, 65)),
  inc_0_12m_R = list(breaks = c(50, 70, 94.6, 120, 160), labels = c("Low\n(50)", "70", "▲\n(94.6)", "120", "High\n(160)"), limits = c(50, 160)),
  
  # N values
  inc_0_60m_N = list(breaks = c(60, 1e3, 1e4, 1e5, 1e6, 6.2e6), labels = c("Low\n(76)", "1K", "10K", "100K", "1M", "High\n(6M)"), limits = c(60, 6.2e6)),
  #inc_0_6m_N = list(breaks = c(18, 1e3, 1e4, 1e5, 1e6, 1.3e6), labels = c("Low\n(18)", "1K", "10K", "100K", "1M", "High\n(1.3M)"), limits = c(18, 1.3e6)),
  inc_0_6m_N = list(breaks = c(14, 1e3, 1e4, 1e5, 1.31e6), labels = c("Low\n(18)", "1K", "10K", "100K", "High\n(1.3M)"), limits = c(14, 1.31e6)),
  #inc_6_12m_N = list(breaks = c(12, 1e3, 1e4, 1e5, 1e6, 1.3e6), labels = c("Low\n(12)", "1K", "10K", "100K", "1M", "High\n(1.3M)"), limits = c(12, 1.3e6)),
  inc_6_12m_N = list(breaks = c(9, 1e3, 1e4, 1e5, 1.3e6), labels = c("Low\n(12)", "1K", "10K", "100K", "High\n(1.3M)"), limits = c(9, 1.3e6)),
  inc_12_60m_N = list(breaks = c(36, 1e3, 1e4, 1e5, 1e6, 3.6e6), labels = c("Low\n(46)", "1K", "10K", "100K", "1M", "High\n(3.6M)"), limits = c(36, 3.6e6)),
  inc_0_12m_N = list(breaks = c(23, 1e3, 1e4, 1e5, 1e6, 2.61e6), labels = c("Low\n(30)", "1K", "10K", "100K", "1M", "High\n(2.6M)"), limits = c(23, 2.61e6)),
  
  # ---------------- hos ----------------
  # R values
  hos_0_60m_R = list(breaks = c(0.5, 1, 5.3, 10, 25), labels = c("Low\n(0.5)", "1", "▲\n(5.3)", "10", "High\n(25)"), limits = c(0.5, 25)),
  hos_0_6m_R = list(breaks = c(5, 10, 20.2, 40, 80, 100), labels = c("Low\n(5)", "10", "▲\n(20.2)", "40","80","High\n(100)"), limits = c(5, 100)),
  hos_6_12m_R = list(breaks = c(1, 5, 10, 20,35), labels = c("Low\n(1)", "5", "▲\n(10)", "20","High\n(35)"), limits = c(1, 35)),
  hos_0_12m_R = list(breaks = c(3, 10, 15.9, 40, 80), labels = c("Low\n(3)", "10", "▲\n(15.9)", "40","High\n(80)"), limits = c(3, 80)),
  hos_12_60m_R = list(breaks = c(0.1, 1, 1.5, 5, 10,20), labels = c("Low\n(0.1)", "1", "▲\n(1.5)", "5","10", "High\n(20)"), limits = c(0.1, 20)),
  
  # N values
  hos_0_60m_N = list(breaks = c(3, 1e3, 1e4, 1e5, 7554e2), labels = c("Low\n(3)", "1K", "10K", "100K", "High\n(755K)"), limits = c(3, 7554e2)),
  hos_0_6m_N = list(breaks = c(2, 1e3, 1e4, 1e5, 4856e2), labels = c("Low\n(2)", "1K", "10K", "100K", "High\n(485K)"), limits = c(1, 4856e2)),
  hos_6_12m_N = list(breaks = c(1, 1e3, 1e4, 1387e2), labels = c("Low\n(1)", "1K", "10K", "High\n(138K)"), limits = c(1, 1387e2)),
  hos_0_12m_N = list(breaks = c(2, 1e3, 1e4, 1e5, 6377e2), labels = c("Low\n(2)", "1K", "10K", "100K", "High\n(637K)"), limits = c(2, 6377e2)),
  #hos_12_60m_N = list(breaks = c(3, 1e3, 1e4, 1e5, 147e3), labels = c("Low\n(3)", "1K", "10K", "100K", "High\n(147K)"), limits = c(3, 147419.71)),
  hos_12_60m_N = list(breaks = c(1, 1e3, 1e4, 223e3), labels = c("Low\n(1)", "1K", "10K", "High\n(222K)"), limits = c(1, 223e3)),
  
  # ---------------- mort_att ----------------
  # R values
  mort_att_0_6m_R = list(breaks = c(5, 10, 67.2, 100, 250), labels = c("Low\n(5)", "10", "▲\n(67.2)", "100", "High\n(250)"), limits = c(5, 250)),
  mort_att_6_12m_R = list(breaks = c(0.5, 10, 30.3, 90, 120), labels = c("Low\n(0.5)", "10", "▲\n(30.3)", "90", "High\n(120)"), limits = c(0.5, 120)),
  mort_att_12_60m_R = list(breaks = c(0.05, 1, 6.5, 10, 30), labels = c("Low\n(0.05)", "1", "▲\n(6.5)", "10", "High\n(30)"), limits = c(0.05, 30)),
  mort_att_0_60m_R = list(breaks = c(0.5, 10, 15, 30, 60), labels = c("Low\n(0.5)", "10", "▲\n(15.0)", "30", "High\n(60)"), limits = c(0.5, 60)),
  mort_att_0_12m_R = list(breaks = c(2, 10, 48.7, 100, 200), labels = c("Low\n(2)", "10", "▲\n(48.7)", "100", "High\n(200)"), limits = c(2, 200)),
  
  # N values
  mort_att_0_6m_N = list(breaks = c(0.01, 0.1, 10, 1000, 7489), labels = c("Low\n(0.01)", "0.1", "10", "1K", "High\n(7.5K)"), limits = c(0.01, 7489)),
  mort_att_6_12m_N = list(breaks = c(0.01, 0.1, 10, 1000, 3698), labels = c("Low\n(0.01)", "0.1", "10", "1K", "High\n(3.7K)"), limits = c(0.01, 3698)),
  mort_att_12_60m_N = list(breaks = c(0.01, 0.1, 10, 1000, 6614), labels = c("Low\n(0.01)", "0.1", "10", "1K", "High\n(6.7K)"), limits = c(0.01, 6614)),
  mort_att_0_60m_N = list(breaks = c(0.1, 1, 10, 1000, 17659), labels = c("Low\n(0.1)", "1", "10", "1K", "High\n(17.7K)"), limits = c(0.1, 17659)),
  mort_att_0_12m_N = list(breaks = c(0.01, 0.1, 10, 1000, 11045), labels = c("Low\n(0.01)", "0.1", "10", "1K", "High\n(11K)"), limits = c(0.01, 11045)),
  
  
  # ---------------- mort_ass ----------------
  # R values
  mort_ass_0_6m_R = list(breaks = c(5, 50, 150, 200, 500), labels = c("Low\n(5)", "50", "▲\n(150)", "200", "High\n(500)"), limits = c(5, 500)),
  mort_ass_6_12m_R = list(breaks = c(0.5, 10, 68.2, 100, 250), labels = c("Low\n(0.5)", "10", "▲\n(68.2)", "100", "High\n(250)"), limits = c(0.5, 250)),
  mort_ass_12_60m_R = list(breaks = c(0.1, 5, 14.9, 30, 60), labels = c("Low\n(0.1)", "5", "▲\n(14.9)", "30", "High\n(60)"), limits = c(0.1, 60)),
  mort_ass_0_60m_R = list(breaks = c(1, 10, 34.0, 50, 150), labels = c("Low\n(1)", "10", "▲\n(34.0)", "50", "High\n(150)"), limits = c(1, 150)),
  mort_ass_0_12m_R = list(breaks = c(1, 50, 109.1, 200, 400), labels = c("Low\n(1)", "50", "▲\n(109.1)", "200", "High\n(400)"), limits = c(1, 400)),
  
  # N values
  mort_ass_0_6m_N = list(breaks = c(0.1, 1, 10, 1000, 17223), labels = c("Low\n(0.1)", "1", "10", "1K", "High\n(17K)"), limits = c(0.1, 17223)),
  mort_ass_6_12m_N = list(breaks = c(0.01, 0.1, 10, 1000, 8446), labels = c("Low\n(0.01)", "0.1", "10", "1K", "High\n(8K)"), limits = c(0.01, 8446)),
  mort_ass_12_60m_N = list(breaks = c(0.01, 0.1, 10, 1000, 15292), labels = c("Low\n(0.01)", "0.1", "10", "1K", "High\n(15K)"), limits = c(0.01, 15292)),
  mort_ass_0_60m_N = list(breaks = c(0.1, 1, 10, 1000, 40356), labels = c("Low\n(0.1)", "1", "10", "1K", "High\n(40K)"), limits = c(0.1, 40356)),
  mort_ass_0_12m_N = list(breaks = c(0.1, 1, 10, 1000, 25065), labels = c("Low\n(0.1)", "1", "10", "1K", "High\n(25K)"), limits = c(0.1, 25065))
)

# legend_breaks_labels2 <- legend_breaks_labels %>%
#   map(~{
#     breaks <- .x$breaks
#     labels <- .x$labels
#     n <- length(breaks)
#     
#     # 替换第一个和最后一个 labels
#     labels[1] <- paste0("Low\n(", breaks[1], ")")
#     labels[n] <- paste0("High\n(", breaks[n], ")")
#     
#     .x$labels <- labels
#     .x
#   })

world_map_mor <- world_map %>% left_join(df_plot_mort_att)

ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "#F5F5F5", color = "#999999", linewidth = 0.2
  ) +
  geom_map(
    data = world_map_mor %>% filter(AGEGR=='0-<12m'),
    map = world_map,
    aes(map_id = region, fill = R),
    color = "blue", linewidth = 0.1
  )+
  scale_fill_gradient2(transform = 'log10',
                       # low = "#9ecae1",
                       # mid = "#FFFFFF",
                       # high = "#f16913",
                       low = "#2b83ba",   # 深邃蓝
                       mid = "#f7f7f7",   # 干净白
                       high = "#d7191c",   # 醒目红
                       midpoint = 48,
                       na.value = "#999999",
                       # name = legend_name,
                       # labels = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$labels,
                       # breaks = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$breaks,
                       # limits = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$limits,
                       # guide = guide_colorbar(
                       #   barwidth = 20, barheight = 0.5,
                       #   title.position = "top", title.hjust = 0.5,
                       #   ticks = TRUE,
                       #   ticks.colour = c("black"),
                       #   draw.ulim = T, draw.llim = T
                       # )
                       )+
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-55, 85)) +
  theme_void()


# 生成地图
plot_res<-c('inc','hos','mort_att','mort_ass') %>% 
  map(~{
    
    df_plot_mort_att <- get(paste0('df_plot_', .x))
    world_map_mor <- world_map %>% left_join(df_plot_mort_att)
    metric <- .x
    
    expand.grid(
      AGEGR = c("0-<6m", "6-<12m", "0-<12m", "12-<60m","0-<60m"),
      metric = c('N','R'), 
      stringsAsFactors = FALSE
    ) %>% 
      pmap(~{
        
        age <- .x
        type <- .y
        
        # legend 名称
        legend_name <- sprintf('%s%s',
                               case_when(
                                 metric=='inc' ~ paste0(if_else(type=='N','Number of ',''),'RSV-associated ALRI ', if_else(type=='N','','incidence rate')),
                                 metric=='hos' ~ paste0(if_else(type=='N','Number of ',''),'RSV-associated ALRI hospital admission', if_else(type=='N','s',' rate')),
                                 metric=='mort_att' ~ paste0(if_else(type=='N','Number of ',''),'RSV-attributable all-cause ', if_else(type=='N','deaths','mortality rate')),
                                 metric=='mort_ass' ~ paste0(if_else(type=='N','Number of ',''),'RSV-associated all-cause ', if_else(type=='N','deaths','mortality rate'))
                               ),
                               if_else(type=='N',
                                       sprintf('\n(%s)', age),
                                       if_else(metric %in% c('inc','hos'),
                                               sprintf('\n(%s, /1,000 person-years)', age),
                                               sprintf('\n(%s, /100,000 person-years)', age)
                                       )
                               )
        )
        
        # 当前 AGEGR 数据
        df_current <- world_map_mor %>% filter(AGEGR == age)
        
        # global midpoint
        mid_R <- df_global_average %>%
          filter(AGEGR == age) %>%
          pull(!!sym(paste0(metric,"_",type,"_est")))
        
        mid_N<-median(df_plot_mort_att %>% filter(AGEGR == age) %>% pull(N))
        
        # ggplot 基础
        p <- ggplot() +
          geom_polygon(
            data = world_map,
            aes(x = long, y = lat, group = group),
            fill = "#F5F5F5", color = "#999999", linewidth = 0.2
          ) +
          geom_map(
            data = df_current,
            map = world_map,
            aes(map_id = region, fill = !!sym(type)),
            color = "blue", linewidth = 0.1
          )
        
        # R / N 色系 + 发散色带 + 中点白色 + legend 标记
        p <- p + 
          if(type == "R") {
            # R：低蓝 → 中白 → 高橙
            scale_fill_gradient2(transform = 'log10',
                                 # low = "#9ecae1",
                                 # mid = "#FFFFFF",
                                 # high = "#f16913",
                                 low = "#2b83ba",   # 深邃蓝
                                 mid = "#f7f7f7",   # 干净白
                                 high = "#d7191c",   # 醒目红
                                 midpoint = ifelse(is.na(mid_R),40,mid_R),
                                 na.value = "#999999",
                                 name = legend_name,
                                 labels = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$labels,
                                 breaks = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$breaks,
                                 limits = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$limits,
                                 guide = guide_colorbar(
                                   barwidth = 20, barheight = 0.5,
                                   title.position = "top", title.hjust = 0.5,
                                   ticks = TRUE,
                                   ticks.colour = c("black"),
                                   draw.ulim = T, draw.llim = T
                                 )
            )
          } else {
            # N：低绿 → 中白 → 高紫
            scale_fill_gradient2(transform = 'log10',
                                 low = "#1b7837",   # 深森绿
                                 mid = "#f7f7f7",   # 纯白
                                 high = "#762a83",
                                 midpoint = mid_N,
                                 na.value = "#999999",
                                 name = legend_name,
                                 labels = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$labels,
                                 breaks = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$breaks,
                                 limits = legend_breaks_labels[[gsub('-<','_',sprintf('%s_%s_%s',metric,age,type))]]$limits,
                                 guide = guide_colorbar(
                                   barwidth = 20, barheight = 0.5,
                                   title.position = "top", title.hjust = 0.5,
                                   ticks = TRUE,
                                   ticks.colour = c("black"),
                                   draw.ulim = T, draw.llim = T
                                 )
            )
          }
        
        # 坐标固定 + theme
        p <- p +
          coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-55, 85)) +
          theme_void() +
          theme(
            legend.position = "top",
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 8),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
        
        # # 保存文件
        # safe_age <- gsub("<", "", age)
        # filename <- sprintf("plot/map_%s_%s_%s.tiff",type,metric,safe_age)
        # ggsave(
        #   filename = filename,
        #   plot = p,
        #   width = 10, height = 6, units = "in", dpi = 600,
        #   compression = "lzw", bg = "white"
        # )
        
      })
  })

plot_res[[1]][[8]]+plot_res[[1]][[10]]+plot_res[[2]][[8]]+plot_res[[2]][[10]]+
  plot_layout(nrow=2)+
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16))

ggsave("plot/Figure_2.tiff", width = 12, height = 8, dpi = 300,compression = "lzw")

plot_res[[3]][[8]]+plot_res[[3]][[10]]+plot_res[[4]][[8]]+plot_res[[4]][[10]]+
  plot_layout(nrow=2)+
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16))

ggsave("plot/Figure_3.tiff", width = 12, height = 8, dpi = 300,compression = "lzw")


plot_res[[1]][[3]]+plot_res[[1]][[5]]+plot_res[[2]][[3]]+plot_res[[2]][[5]]+
  plot_layout(nrow=2)+
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16))

ggsave("plot/Figure_N_1.tiff", width = 12, height = 8, dpi = 300,compression = "lzw")

plot_res[[3]][[3]]+plot_res[[3]][[5]]+plot_res[[4]][[3]]+plot_res[[4]][[5]]+
  plot_layout(nrow=2)+
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16))

ggsave("plot/Figure_N_2.tiff", width = 12, height = 8, dpi = 300,compression = "lzw")


