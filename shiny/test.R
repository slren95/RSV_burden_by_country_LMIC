library(tidyverse)
library(plotly)
library(scales)

c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr")

# ==========================================
# 1. 数据清洗与 20 个细分组合
# ==========================================
df_plot_data <- df_master %>%
  filter(!is.na(N), !is.na(CountryName), CountryName != "") %>%
  mutate(
    short_metric = case_when(
      metric == "RSV-associated ALRI incidence" ~ "Incidence",
      metric == "RSV-associated ALRI hospital admission" ~ "Admission",
      metric == "RSV-attributable mortality" ~ "Att. Mortality",
      metric == "RSV-associated mortality" ~ "Ass. Mortality"
    ),
    burden_segment = paste0(short_metric, " (", AGEGR, ")")
  ) %>%
  group_by(burden_segment, CountryName) %>%
  summarise(N = sum(N, na.rm = TRUE), .groups = "drop")

# ==========================================
# 2. 纵轴排序（组合总体降序）
# ==========================================
segment_order <- df_plot_data %>%
  group_by(burden_segment) %>%
  summarise(total_segment_n = sum(N, na.rm = TRUE)) %>%
  arrange(desc(total_segment_n)) %>%
  pull(burden_segment)

# ==========================================
# 3. 核心修复：行内数据归一化与排序（绕过 ggplotly 转换缺陷）
# ==========================================
df_plotly_ready <- df_plot_data %>%
  mutate(burden_segment = factor(burden_segment, levels = rev(segment_order))) %>%
  # 💡 关键：按组合升序、病例数降序（确保大国排在前面）
  arrange(burden_segment, desc(N)) %>%
  group_by(burden_segment) %>%
  # 动态计算百分比贡献与累加坐标，彻底替代 position_fill
  mutate(
    segment_total = sum(N),
    pct = (N / segment_total) * 100,
    # 计算堆叠形变，构建绝对安全的行内从左到右位置
    x_start = cumsum(pct) - pct,
    x_end = cumsum(pct)
  ) %>%
  ungroup()

# ==========================================
# 4. 用原生 plot_ly 绘制完美的双降序百分比图
# ==========================================
fig <- plot_ly(df_plotly_ready) %>%
  add_segments(
    y = ~burden_segment, yend = ~burden_segment,
    x = ~x_start, xend = ~x_end,
    color = ~CountryName,
    colors = "Set1", # 你可以使用任意喜欢的调色板
    line = list(width = 18), # 用加粗的线段完美模拟条形图
    showlegend = TRUE,
    # 定制顶级学术悬浮框
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "Country: %{text}<br>",
      "Contribution: %{customdata:.1f}%<br>",
      "Cases (N): %{x:,.0f}<br>", # 这里通过特殊接线映射显示绝对值
      "<extra></extra>"
    ),
    text = ~CountryName,
    customdata = ~pct
  ) %>%
  layout(
    title = list(
      text = "<b>Global RSV Disease Burden Stratification (Absolute Cases N)</b>",
      font = list(size = 14, color = "#2c3e50")
    ),
    xaxis = list(
      title = "Cumulative Country Contribution Percentage (%)",
      ticksuffix = "%",
      range = c(0, 100),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    yaxis = list(
      title = "",
      type = "category",
      tickfont = list(size = 11, fontweight = "bold")
    ),
    margin = list(l = 180, r = 20, t = 60, b = 40),
    legend = list(title = list(text = "<b>Countries</b>"), orientation = "v", x = 1.02, y = 1),
    hovermode = "closest"
  )

# 渲染清洗后的无错图表
fig
