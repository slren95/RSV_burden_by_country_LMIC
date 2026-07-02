# ==========================================
# Tab: Burden Composition 后端业务逻辑 (物理 Bar 动态行内排序版)
# ==========================================

init_composition_server <- function(input, output, session) {
  
  # ==========================================
  # 1. 核心控制器：快捷预设与滑块联合控制主多选框
  # ==========================================
  observeEvent(input$comp_preset, {
    preset_mode <- input$comp_preset
    if (preset_mode == "none") return() 
    
    df_current_sub <- df_master %>%
      filter(metric %in% input$comp_metrics, AGEGR %in% input$comp_ages, !is.na(N))
    
    target_iso <- NULL
    if (preset_mode == "all") {
      target_iso <- unique(df_current_sub$ISOCountry)
    } else if (preset_mode %in% c("L", "LM", "UM")) {
      target_iso <- df_current_sub %>% filter(Income2019 == preset_mode) %>% pull(ISOCountry) %>% unique()
    } else if (preset_mode %in% c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO")) {
      target_iso <- df_current_sub %>% filter(WHORegion == preset_mode) %>% pull(ISOCountry) %>% unique()
    }
    
    updateSelectizeInput(session, "comp_countries", selected = target_iso)
  })
  
  observeEvent(input$comp_top_n_slider, {
    n_limit <- input$comp_top_n_slider
    
    top_iso <- df_master %>%
      filter(metric %in% input$comp_metrics, AGEGR %in% input$comp_ages, !is.na(N)) %>%
      group_by(ISOCountry) %>%
      summarise(total_n = sum(N, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_n)) %>%
      slice_max(total_n, n = n_limit, with_ties = FALSE) %>%
      pull(ISOCountry)
    
    updateSelectizeInput(session, "comp_countries", selected = top_iso)
    
    if (input$comp_preset != "none" && input$comp_preset != "top_n") {
      updateSelectInput(session, "comp_preset", selected = "none")
    }
  })
  
  observeEvent(input$btn_clear_comp_countries, {
    updateSelectInput(session, "comp_preset", selected = "none")
    updateSelectizeInput(session, "comp_countries", selected = character(0))
  })
  
  # ==========================================
  # 2. 高性能数据清洗流 (精细独立组内累计计算)
  # ==========================================
  composition_filtered_data <- reactive({
    req(input$comp_metrics, input$comp_ages)
    if (length(input$comp_countries) == 0) return(NULL)
    
    df_sub <- df_master %>%
      filter(
        metric %in% input$comp_metrics,
        AGEGR %in% input$comp_ages,
        ISOCountry %in% input$comp_countries,
        !is.na(N), !is.na(CountryName), CountryName != ""
      )
    
    if (nrow(df_sub) == 0) return(NULL)
    
    df_sub <- df_sub %>%
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
    
    # 纵轴总体排序
    segment_order <- df_sub %>%
      group_by(burden_segment) %>%
      summarise(total_segment_n = sum(N, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_segment_n)) %>%
      pull(burden_segment)
    
    # 💡 核心修正：严格在各自指标组合 (burden_segment) 内部实现【因地制宜】的绝对独立降序
    df_ready <- df_sub %>%
      mutate(burden_segment = factor(burden_segment, levels = rev(segment_order))) %>%
      # 每一行内部按照自己的 N 从大到小重新大洗牌
      group_by(burden_segment) %>%
      arrange(desc(N), .by_group = TRUE) %>%
      mutate(
        segment_total = sum(N),
        pct = (N / segment_total) * 100,
        
        # 精准计算累计值
        cum_abs = cumsum(N),
        cum_pct = cumsum(pct),
        
        # 🛠️【突破性修复】：显式手算格子在图表上的物理边界，彻底废除 Plotly 默认的随机堆叠顺序
        # 模式 A (百分比) 的左侧基底和格子宽度
        base_pct = cum_pct - pct,
        width_pct = pct,
        
        # 模式 B (绝对值) 的左侧基底和格子宽度
        base_abs = cum_abs - N,
        width_abs = N,
        
        # 四合一高维完美同步 Hover 标签文本
        text_unified_hover = paste0(
          "<b>", burden_segment, "</b><br>",
          "Country: ", CountryName, "<br>",
          "───────────────────────────<br>",
          "<b>Current Country:</b><br>",
          " ⁃ Cases (N): ", comma(round(N, 0)), "<br>",
          " ⁃ Share: ", sprintf("%.1f", pct), "%<br>",
          "<b>Cumulative (From Left):</b><br>",
          " ⁃ Cum. Cases: ", comma(round(cum_abs, 0)), "<br>",
          " ⁃ Cum. Share: ", sprintf("%.1f", cum_pct), "%<br>",
          "───────────────────────────<br>",
          "<span style='font-size:10px;'>Ensemble Total Cases: ", comma(round(segment_total, 0)), "</span>",
          "<extra></extra>"
        )
      ) %>%
      ungroup()
    
    df_ready
  })
  
  # ==========================================
  # 3. Plotly 渲染器 (完美支持格子内部居中静态刻字)
  # ==========================================
  output$composition_plotly <- renderPlotly({
    df_plot <- composition_filtered_data()
    if (is.null(df_plot) || nrow(df_plot) == 0) {
      return(plotly_empty() %>% layout(title = "Please select at least one country to populate matrix."))
    }
    
    is_percent <- input$comp_is_percent
    
    # 💡 根据当前切换的维度，动态决定格子内部印什么字
    # 百分比图就在格子内部印 "12.5%"，绝对值图就在格子内部印 "45,201"
    df_plot <- df_plot %>%
      mutate(
        label_inside = if(is_percent) {
          sprintf("%.1f%%", pct)
        } else {
          scales::comma(round(N, 0))
        }
      )
    
    # 利用基础的 add_bars 并手工喂入 base 参数
    fig <- plot_ly(
      data = df_plot,
      y = ~burden_segment,
      x = if(is_percent) ~width_pct else ~width_abs,  # 格子本身的宽度
      base = if(is_percent) ~base_pct else ~base_abs, # 手工指定当前格子离纵轴左侧起点的距离
      color = ~CountryName,
      colors = "Set1",
      type = "bar",
      orientation = "h",
      
      # 🛠️【核心改造】：控制格子内部的静态文本
      text = ~label_inside,            # 🟢 赋给 text 的变量会被直接印在格子里
      textposition = "inside",         # 🟢 显式指定文字强制塞在柱条内部
      insidetextanchor = "middle",     # 🟢 强迫文字在格子内部绝对居中对齐
      
      # 控制字体样式（加粗、白色，在 Set1 鲜艳底色下阅读体验极佳）
      textfont = list(color = "#ffffff", font = list(weight = "bold", size = 10)),
      
      # 保持高级四合一悬浮框绑定到 hovertext，完全不受内部刻字干扰
      hovertext = ~text_unified_hover,
      hovertemplate = "%{hovertext}"
    )
    
    # 全局样式排版
    fig <- fig %>% layout(
      xaxis = list(
        title = if(is_percent) "Cumulative Country Contribution Percentage (%)" else "Total Scale of Absolute Cases (N)",
        ticksuffix = if(is_percent) "%" else "",
        range = if(is_percent) c(0, 100) else NULL,
        showgrid = TRUE,
        gridcolor = "#f0f0f0"
      ),
      yaxis = list(title = "", type = "category", tickfont = list(size = 11, fontweight = "bold")),
      
      # 强制所有国家的 Bar 覆盖在同一条水平中心线上
      barmode = "overlay", 
      bargap = 0.25, 
      
      margin = list(l = 180, r = 20, t = 40, b = 40),
      legend = list(title = list(text = "<b>Countries</b>"), orientation = "v", x = 1.02, y = 1),
      hovermode = "closest"
    )
    
    fig
  })
}