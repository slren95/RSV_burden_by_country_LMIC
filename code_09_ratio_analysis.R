rm(list=ls())
library(tidyverse)
library(rio)
library(ggsci)
library(scales)

RF.res.impute_1000<-import('rda/RF.res.impute2.rds',trust=T) 
df_hos_sample_final_1000<-import("rda/df_hos_sample_final_1000.rds",trust=T)
df_mort_all_DeCoDe.lmic<-import('rda/df_mort_all_DeCoDe.lmic.rds',trust=T)
df_mort_all_NP.lmic<-import('rda/df_mort_all_NP.lmic.rds',trust=T)
df_lmic_imputed<-import('rda/df_lmic_imputed.rds',trust=T)

# df_pro_all ----
df_pro_all<-list(
RF.res.impute_1000 %>%
  select(ISOCountry,index,AGEGR,N.inc=N,R.inc=IR),

df_hos_sample_final_1000 %>%
  select(index,ISOCountry,starts_with('Hos_'),starts_with('Rate_')) %>%
  pivot_longer(cols=c(starts_with('Hos_'),starts_with('Rate_')),names_pattern = '(.*)_(.*)',names_to = c('.value','AGEGR')) %>%
  rename(N.hos=Hos,R.hos=Rate),

df_mort_all_DeCoDe.lmic %>%
  transmute(ISOCountry,index,
            m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,
            m0060_N=m0006_N+m0612_N+m1260_N,
            m0012_N=m0006_N+m0612_N,
            ) %>%
  rename(
    `N.mor_att__0-<6m`  = m0006_N,
    `N.mor_att__6-<12m` = m0612_N,
    `N.mor_att__12-<60m` = m1260_N,
    `N.mor_att__0-<60m`  = m0060_N,
    `N.mor_att__0-<12m`  = m0012_N
  ) %>%
  pivot_longer(cols = starts_with('N.'),names_pattern = '(N.*)__(.*)',names_to = c('.value','AGEGR')),

df_mort_all_NP.lmic %>%
  transmute(ISOCountry,index,
            m0006_N=m0001_N+m0106_N,m0612_N,m1260_N,
            m0060_N=m0006_N+m0612_N+m1260_N,
            m0012_N=m0006_N+m0612_N,
  ) %>%
  rename(
    `N.mor_ass__0-<6m`  = m0006_N,
    `N.mor_ass__6-<12m` = m0612_N,
    `N.mor_ass__12-<60m` = m1260_N,
    `N.mor_ass__0-<60m`  = m0060_N,
    `N.mor_ass__0-<12m`  = m0012_N
  ) %>%
  pivot_longer(cols = starts_with('N.'),names_pattern = '(N.*)__(.*)',names_to = c('.value','AGEGR'))) %>%
  reduce(left_join) %>%
  mutate(hos_2_inc=N.hos/N.inc,mor_2_inc=N.mor_att/N.inc) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry,CountryName,Income2019)) %>%
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
  )
)
## summarise ----
df_pro_sum<-df_pro_all %>%
  reframe(
    q = c("025", "500", "975"),
    across(
      contains('_2_'),
      ~ quantile(.x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    ),
    .by = c(ISOCountry,AGEGR,Income2019,CountryName,region)
  ) %>%
  pivot_wider(id_cols = c(ISOCountry,AGEGR,Income2019,CountryName,region),names_from = q,values_from = contains('_2_'),names_sep = '.') %>%
  mutate(Income2019 = factor(Income2019, levels = c("L", "LM", "UM")),
         AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','0-<12m','12-<60m','0-<60m'))) %>% 
  mutate(ISOCountry = fct_reorder(ISOCountry, as.numeric(Income2019)))

head(df_pro_sum)

## plot ----

ggplot(df_pro_sum, aes(x = ISOCountry,color=Income2019)) +
  geom_linerange(aes(ymin = hos_2_inc.025, ymax = hos_2_inc.975), size = 0.7) +
  geom_point(aes(y = hos_2_inc.500), size = 1.5)+
  facet_wrap(AGEGR~., scales = "free_y", ncol = 1) +
  scale_color_lancet() + 
  scale_y_continuous(labels=label_percent())+
  theme_bw() +
  labs(
    x = "Country",
    y = "hospital admission to incidence",
    color = "Income level"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,family = "Consolas",size=6),
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray95") # 分面标签背景
  )
ggsave('plot/hos_2_inc.tiff',dpi = 200,width = 2200,height = 1200,units = 'px')

ggplot(df_pro_sum, aes(x = ISOCountry,color=Income2019)) +
  geom_linerange(aes(ymin = mor_2_inc.025, ymax = mor_2_inc.975), size = 0.7) +
  geom_point(aes(y = mor_2_inc.500), size = 1.5)+
  facet_wrap(AGEGR~., scales = "free_y", ncol = 1) +
  scale_color_lancet() + 
  scale_y_continuous(labels=label_percent())+
  theme_bw() +
  labs(
    x = "Country",
    y = "attributable mortality to incidence",
    color = "Income level"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,family = "Consolas",face = 'bold',size=7),
    legend.position = "top",
    strip.background = element_rect(fill = "gray95") # 分面标签背景
  )

ggsave('plot/mor_2_inc.tiff',dpi = 200,width = 2200,height = 1200,units = 'px')

# Map plot ----
world_map <- map_data("world") %>%
  mutate(region=case_when(region=='Taiwan'~'China',T~region))

df_plot_map<-world_map %>% left_join(df_pro_sum)

df_pro_sum %>%
  summarise(across(c(hos_2_inc.500,mor_2_inc.500),
                   list(
                     med = ~ median(.x, na.rm = TRUE),
                     min = ~ min(.x, na.rm = TRUE),
                     max = ~ max(.x, na.rm = TRUE)
                   )),.by = AGEGR)

breaks_list_hos <- list(
  `hos_0-<6m`   = list(breaks = seq(0, 0.60, by = 0.10)),
  `hos_6-<12m`  = list(breaks = seq(0, 0.25, by = 0.05)),
  `hos_12-<60m` = list(breaks = seq(0, 0.25, by = 0.05)),
  `hos_0-<12m`  = list(breaks = seq(0, 0.40, by = 0.10)),
  `hos_0-<60m`  = list(breaks = seq(0, 0.30, by = 0.05))
)

breaks_list_mor <- list(
  `mor_0-<6m`   = list(breaks = seq(0, 0.030, by = 0.005)),
  `mor_6-<12m`  = list(breaks = seq(0, 0.015, by = 0.0025)),
  `mor_12-<60m` = list(breaks = seq(0, 0.010, by = 0.002)),
  `mor_0-<12m`  = list(breaks = seq(0, 0.025, by = 0.005)),
  `mor_0-<60m`  = list(breaks = seq(0, 0.015, by = 0.003))
)


split(df_plot_map,df_plot_map$AGEGR) %>%
  iwalk(~{
    legend_name<-paste0('Hospital admission to incidence ratio\n',.y)
    print(legend_name)
    ggplot() +
      geom_polygon(
        data = world_map,
        aes(x = long, y = lat, group = group),
        fill = "#F5F5F5", color = "#999999", linewidth = 0.2
      ) +
      geom_map(
        data = .x,
        map = world_map,
        aes(map_id = region, fill = hos_2_inc.500),
        color = "blue", linewidth = 0.1
      )+
      coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-55, 85)) +
      theme_void() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )+
      scale_fill_viridis_c(
        na.value = "#F5F5F5",
        option = "magma", # 或者 "viridis", "plasma"
        direction = -1,
        breaks=breaks_list_hos[[paste0('hos_',.y)]]$breaks,
        limits=c(0,max(breaks_list_hos[[paste0('hos_',.y)]]$breaks)),
        labels = label_percent(accuracy = 0.1),
        name = legend_name,
                           guide = guide_colorbar(
                             barwidth = 20, barheight = 0.5,
                             title.position = "top", title.hjust = 0.5,
                             ticks = TRUE,
                             ticks.colour = c("white"),
                             draw.ulim = T, draw.llim = T
                           )
      )
    safe_age <- gsub("<", "", .y)
    filename <- sprintf('plot/map_hos_2_inc_%s.tiff',safe_age)
    ggsave(filename,width = 10, height = 6, units = "in", dpi = 600,compression = "lzw", bg = "white")
  })


split(df_plot_map,df_plot_map$AGEGR) %>%
  iwalk(~{
    legend_name<-paste0('Attributable mortality to incidence ratio\n',.y)
    print(legend_name)
    ggplot() +
      geom_polygon(
        data = world_map,
        aes(x = long, y = lat, group = group),
        fill = "#F5F5F5", color = "#999999", linewidth = 0.2
      ) +
      geom_map(
        data = .x,
        map = world_map,
        aes(map_id = region, fill = mor_2_inc.500),
        color = "blue", linewidth = 0.1
      )+
      coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-55, 85)) +
      theme_void() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )+
      scale_fill_viridis_c(
        na.value = "#F5F5F5",
        option = "magma", # 或者 "viridis", "plasma"
        direction = -1,
        breaks=breaks_list_mor[[paste0('mor_',.y)]]$breaks,
        limits=c(0,max(breaks_list_mor[[paste0('mor_',.y)]]$breaks)),
        labels = label_percent(accuracy = 0.01),
        name = legend_name,
        guide = guide_colorbar(
          barwidth = 20, barheight = 0.5,
          title.position = "top", title.hjust = 0.5,
          ticks = TRUE,
          ticks.colour = c("white"),
          draw.ulim = T, draw.llim = T
        )
      )
    safe_age <- gsub("<", "", .y)
    filename <- sprintf('plot/map_mor_2_inc_%s.tiff',safe_age)
    ggsave(filename,width = 10, height = 6, units = "in", dpi = 600,compression = "lzw", bg = "white")
  })

# Top 5 ----

df_pro_sum %>%
  filter(AGEGR=='0-<12m') %>%
  slice_max(mor_2_inc.500,n=5) %>%
  select(ISOCountry,CountryName,contains('mor')) %>%
  set_names(~str_remove_all(.x,'(m0012_)')) %>%
  mutate(across(starts_with('mor_2_inc'),~sprintf('%.1f',round(.x*100,1)))) %>%
  mutate(str_ratio=sprintf('%s. %s %s (%s–%s) %%',row_number(),CountryName,mor_2_inc.500,mor_2_inc.025,mor_2_inc.975)
         ) %>%
  mutate(str_ratio2=paste0(str_ratio,collapse = '\n')) %>%
  pull(str_ratio2) %>%
  head(1) %>%
  cat("\n\n Mort_2_inc \n", ., file = "docs/top_5.txt", append = TRUE)


