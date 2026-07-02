library(ggplot2)
library(dplyr)
library(tidyr)
library(logitnorm)
library(patchwork)
library(scales) # 用于百分比刻度

# 1. 参数与数据准备
df_params <- data.frame(
  mu = c(-1.18, -1.60, -1.60),
  sigma = c(0.810, 0.841, 1.02),
  AGEGR = factor(c("0-<6m", "6-<12m", "0-<60m"), 
                 levels = c("0-<6m", "6-<12m", "0-<60m"))
)

sd_pc1 <- 1.87
# 代表性国家 A 和 B
df_countries <- data.frame(
  Country = c("Country A", "Country B"),
  PC1 = c(1.3, 2.2), 
  Color = c("#0072B2", "#D55E00")
) %>%
  mutate(Perc = pnorm(PC1, 0, sd_pc1))

# 构造曲线数据
x_pc1 <- seq(-6, 6, length.out = 500)
df_pc1 <- data.frame(x = x_pc1, pdf = dnorm(x_pc1, 0, sd_pc1), cdf = pnorm(x_pc1, 0, sd_pc1))

x_hosp <- seq(0, 1, length.out = 500)
df_hosp <- expand.grid(x = x_hosp, AGEGR = df_params$AGEGR) %>%
  left_join(df_params, by = "AGEGR") %>%
  mutate(pdf = dlogitnorm(x, mu, sigma), cdf = plogitnorm(x, mu, sigma))

# 计算映射交叉点
points_hosp <- df_params %>%
  crossing(df_countries) %>%
  mutate(val = qlogitnorm(Perc, mu, sigma),
         dens = dlogitnorm(val, mu, sigma))

# 颜色定义
country_colors <- c("Country A" = "#0072B2", "Country B" = "#D55E00")
age_colors <- c("0-<6m"="#F8766D", "6-<12m"="#00BA38", "0-<60m"="#619CFF")

# --- 绘图部分 ---

# A. PDF (Input)
pA <- ggplot(df_pc1) +
  geom_area(aes(x, pdf), fill = "grey95", alpha = 0.5) + 
  geom_line(aes(x, pdf)) +
  geom_segment(data = df_countries, aes(x = PC1, xend = PC1, y = 0, yend = dnorm(PC1, 0, sd_pc1), color = Country), linetype = "dotted") +
  geom_point(data = df_countries, aes(x = PC1, y = dnorm(PC1, 0, sd_pc1), color = Country), size = 2) +
  geom_text(data = df_countries, aes(x = PC1, y = 0, label = sprintf("%.1f", PC1), color = Country), 
            vjust = 1.5, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = country_colors) +
  labs(subtitle = "A. PDF", x = "Composite index\n(PC1)", y = "Probability Density") +
  theme_classic() + theme(legend.position = "none") +
  coord_cartesian(clip = "off")

# D. PDF (Output)
pD <- ggplot() +
  geom_line(data = df_hosp, aes(x, pdf, color = AGEGR), size = 0.8) +
  geom_segment(data = filter(points_hosp, AGEGR == "0-<60m"), 
               aes(x = val, xend = val, y = 0, yend = dens, color = Country), linetype = "dotted") +
  geom_point(data = points_hosp, aes(x = val, y = dens, color = Country), size = 2) +
  geom_text(data = filter(points_hosp, AGEGR == "0-<60m"), 
            aes(x = val, y = 0, label = sprintf("%.1f%%", val*100), color = Country), 
            vjust = 1.5, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(
    name = NULL,
    values = c(country_colors, age_colors),
    breaks = c("0-<6m", "6-<12m", "0-<60m", "Country A", "Country B")
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(subtitle = "D. PDF", x = "Hospitalisation Proportion", y = "Probability Density") +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(
    shape = c(NA, NA, NA, 16, 16), 
    linetype = c(1, 1, 1, NA, NA)
  ))) +
  coord_cartesian(clip = "off")

# B. CDF (Mapping I)
pB <- ggplot(df_pc1) +
  geom_line(aes(x, cdf), linetype = "solid") +
  geom_hline(data = df_countries, aes(yintercept = Perc, color = Country), linetype = "dotted") +
  geom_segment(data = df_countries, aes(x = PC1, xend = PC1, y = 0, yend = Perc, color = Country), linetype = "dotted") +
  geom_point(data = df_countries, aes(x = PC1, y = Perc, color = Country), size = 2) +
  geom_text(data = df_countries, aes(x = -5.8, y = Perc, label = sprintf("%.2f (Quantile)", Perc), color = Country), 
            hjust = 0, vjust = -0.5, size = 3, fontface = "bold", show.legend = FALSE) +
  geom_text(data = df_countries, aes(x = PC1, y = 0, label = sprintf("%.1f", PC1), color = Country), 
            vjust = 1.5, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Composite index (PC1)", y = "Cumulative Probability") +
  theme_classic() + theme(legend.position = "none") +
  coord_cartesian(clip = "off")

# C. CDF (Mapping II)
pC <- ggplot(df_hosp) +
  geom_line(aes(x, cdf, color = AGEGR), size = 0.8) +
  geom_hline(data = df_countries, aes(yintercept = Perc, color = Country), linetype = "dotted") +
  geom_segment(data = filter(points_hosp, AGEGR == "0-<60m"), 
               aes(x = val, xend = val, y = 0, yend = Perc, color = Country), linetype = "dotted") +
  geom_point(data = points_hosp, aes(x = val, y = Perc, color = Country), size = 1.5) +
  geom_text(data = df_countries, aes(x = 0.01, y = Perc, label = sprintf("%.2f  (Quantile)", Perc), color = Country), 
            hjust = 0, vjust = -0.5, size = 3, fontface = "bold", show.legend = FALSE) +
  geom_text(data = filter(points_hosp, AGEGR == "0-<60m"), 
            aes(x = val, y = 0, label = sprintf("%.1f%%", val*100), color = Country), 
            vjust = 1.5, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c(country_colors, age_colors), name=NULL) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Hospitalisation Proportion", y = "Cumulative Probability") +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(
    shape = c(NA, NA, NA, 16, 16), 
    linetype = c(1, 1, 1, NA, NA)
  ))) +
  coord_cartesian(clip = "off")

# --- 2x2 组合 ---
(pA + pD) / (pB + pC) + 
  plot_layout(guides = 'collect') & 
  theme(plot.margin = unit(c(0.5, 0.5, 1.5, 0.5), "lines"))


(pB + pC) + 
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A')

ggsave(filename = "plot/PC1_method.tiff", device = "tiff", 
       dpi = 300, width = 300, height = 150, units = "mm", compression = "lzw")

