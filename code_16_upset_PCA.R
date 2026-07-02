rm(list=ls())
library(ggplot2)
library(tidyr)
library(patchwork)
library(scales)
library(dplyr)
library(rio)
library(gt)

df_pca_10 <- import('rda/df_pca_10.rds', trust = TRUE) %>%
  mutate(U5MR = U5MR2019, NMR = NMR2019)

str(df_pca_10)

# Upset plot ----

intersect_cols <- c("U5MR", "NMR", "MMRT", "BEDS", "PHYS", "NUMW", "OUTP", "INPA")

df_models <- df_pca_10 %>%
  select(all_of(intersect_cols), Metric = var_dim1) %>%
  arrange(desc(Metric)) %>%
  mutate(model_id = factor(row_number(), levels = row_number()))

df_long <- df_models %>%
  pivot_longer(cols = all_of(intersect_cols), names_to = "Variable", values_to = "Selected") %>%
  mutate(Variable = factor(Variable, levels = intersect_cols))

p_top <- ggplot(df_models, aes(x = model_id, y = Metric)) +
  geom_col(fill = "#2b5c8f", width = 0.55, alpha = 0.9) +
  geom_text(aes(label = percent(Metric, accuracy = 0.1)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = "", y = "Proportion of variance explained by PC1") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

p_bottom <- ggplot(df_long, aes(x = model_id, y = Variable)) +
  geom_line(aes(group = Variable), color = "#f2f2f2", linewidth = 2) +
  geom_point(data = filter(df_long, !Selected), color = "#e0e0e0", size = 3) +
  geom_line(data = filter(df_long, Selected), aes(group = model_id), color = "#2c3e50", linewidth = 1.2) +
  geom_point(data = filter(df_long, Selected), color = "#2c3e50", size = 3.5) +
  geom_text(
    data = df_long %>% distinct(Variable), 
    aes(x = 0.7, y = Variable, label = Variable), 
    hjust = 1,              # 保持文字右对齐
    vjust = 0.5, 
    fontface = "bold", 
    size = 3.2,             # 对应之前的 size=8 左右
    color = "#2c3e50"
  )+
  labs(x = "Variable combinations included in models", y = "Candidate explanatory variables") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    #axis.text.y = element_text(face = "bold", size = 8, hjust = 1)
  )+
  coord_cartesian(clip = "off", xlim = c(1, NA))

p_top / p_bottom + plot_layout(heights = c(2, 1))

ggsave('plot/PCA_Upset.tiff', width = 10, height = 6, dpi = 300)

# Table ----

library(tidyverse)
library(gt)

# 定义变量顺序
raw_vars <- c("U5MR2019", "NMR2019", "MMRT", "BEDS", "PHYS", "NUMW", "OUTP", "INPA")

gt_sci_table <- df_pca_10 %>%
  # 提取解释方差（转为 0-100 的数值，方便表头统一注明显百分比）
  mutate(
    Model = paste("Model", row_id),
    Var_Expl = map_dbl(variance, ~ .x %>% filter(PC == 1) %>% pull(percent)) * 100
  ) %>%
  select(Model, Var_Expl, loadings) %>%
  
  # 展开并转为宽表
  unnest(loadings) %>%
  select(Model, Var_Expl, column, PC1) %>%
  pivot_wider(names_from = column, values_from = PC1) %>%
  arrange(desc(Var_Expl)) %>%
  
  # 动态补齐缺失列
  {
    missing_cols <- setdiff(raw_vars, names(.))
    if(length(missing_cols) > 0) {
      new_cols <- setNames(rep(list(as.numeric(NA)), length(missing_cols)), missing_cols)
      bind_cols(., as_tibble(new_cols))
    } else .
  } %>%
  select(Model, Var_Expl, all_of(raw_vars)) %>%
  
  # 构建符合 SCI 规范的 gt 表格
  gt() %>%
  # 设置专业表格标题
  tab_header(
    title = "Table S1. Principal component analysis (PCA) of country-level indicators and performance of the first principal component (PC1)."
  ) %>%
  
  # 格式化数值（方差占比不带百分号，保留2位；载荷保留3位）
  fmt_number(
    columns = Var_Expl,
    decimals = 2
  ) %>%
  fmt_number(
    columns = all_of(raw_vars),
    decimals = 3
  ) %>%
  
  # 缺失值规范化为 "-"
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  
  # 规范列名，将百分号放入表头
  cols_label(
    Model = "Model",
    Var_Expl = "PC1 Variance Explained (%)",
    U5MR2019 = "U5MR",
    NMR2019 = "NMR",
    MMRT = "MMRT",
    BEDS = "BEDS",
    PHYS = "PHYS",
    NUMW = "NUMW",
    OUTP = "OUTP",
    INPA = "INPA"
  ) %>%
  
  # 核心：添加专业合并表头（Spanner Header）说明这是第一主成分的载荷
  tab_spanner(
    label = "Loadings of the First Principal Component (PC1)",
    columns = all_of(raw_vars)
  ) %>%
  
  # 严格的 SCI 三线表样式配置
  tab_options(
    # 字体与对齐
    table.font.name = "Arial",
    table.font.size = px(12),
    heading.title.font.size = px(13),
    heading.title.font.weight = "bold",
    heading.align = "left",
    column_labels.font.weight = "bold",
    
    # 边框设置：经典的 SCI 三线表（Top, Bottom, Banner 下划线）
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    
    # 移除内部网格线
    column_labels.border.top.width = px(0),
    table_body.border.top.width = px(0),
    stub.border.width = px(0),
    
    # 紧凑型版面
    data_row.padding = px(6)
  )

# 打印表格
gt_sci_table

gtsave(gt_sci_table, filename = "docs/Table_S1_PCA_Results.docx")
