init_map_server <- function(input, output, session) {
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id) && !(click$id %in% input$comparison_countries)) {
      updateSelectizeInput(session, "comparison_countries", selected = c(input$comparison_countries, click$id))
    }
  })
  
  # 全量矩阵区间提取
  all_metrics_broad_data <- reactive({
    
    req(input$select_age)
    age_filter <- input$select_age
    
    # 🔧【已修复】：删除了导致崩溃的非法字符 "... ="
    ci_inc <- RF.res.impute2 %>%
      filter(AGEGR == age_filter) %>%
      reframe(
        across(
          c(IR, N),
          list(
            q500 = ~quantile(.x, 0.5, na.rm = TRUE),
            q025 = ~quantile(.x, 0.025, na.rm = TRUE),
            q975 = ~quantile(.x, 0.975, na.rm = TRUE)
          )
        ),
        .by = ISOCountry
      ) %>%
      mutate(across(starts_with('IR_'),~round(.x,1)),
             across(starts_with('N_'),
                    function(x) {
                      case_when(
                        x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                        x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                        TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                      )
                    })) %>%
      transmute(
        ISOCountry = toupper(trimws(ISOCountry)),
        inc_R      = IR_q500,
        inc_R_ci   = sprintf("%.1f (%.1f–%.1f)", IR_q500, IR_q025, IR_q975),
        inc_N      = N_q500,
        inc_N_ci   = sprintf(
          "%s (%s–%s)",
          comma(round(N_q500, 0)),
          comma(round(N_q025, 0)),
          comma(round(N_q975, 0))
        )
      )
    
    df_base <- df_hos_by_country2_1000 %>%
      mutate(ISOCountry = toupper(trimws(ISOCountry))) %>%
      mutate(across(starts_with('Rate_'),~round(.x,1)),
             across(starts_with('Hos_'),
                    function(x) {
                      case_when(
                        x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                        x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                        TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                      )
                    }))
    
    if (age_filter != "12-<60m") {
      
      df_tmp <- df_base %>%
        select(ISOCountry, contains(age_filter)) %>%
        rename_with(~"R",   matches(paste0("Rate_", age_filter, "_q500"))) %>%
        rename_with(~"R_l", matches(paste0("Rate_", age_filter, "_q025"))) %>%
        rename_with(~"R_u", matches(paste0("Rate_", age_filter, "_q975"))) %>%
        rename_with(~"N",   matches(paste0("Hos_", age_filter, "_q500"))) %>%
        rename_with(~"N_l", matches(paste0("Hos_", age_filter, "_q025"))) %>%
        rename_with(~"N_u", matches(paste0("Hos_", age_filter, "_q975")))
      
    } else {
      
      df_tmp <- df_base %>%
        select(ISOCountry, ends_with("pos")) %>%
        rename_with(~"R",   matches(paste0("Rate_", age_filter, "_q500pos"))) %>%
        rename_with(~"R_l", matches(paste0("Rate_", age_filter, "_q025pos"))) %>%
        rename_with(~"R_u", matches(paste0("Rate_", age_filter, "_q975pos"))) %>%
        rename_with(~"N",   matches(paste0("Hos_", age_filter, "_q500pos"))) %>%
        rename_with(~"N_l", matches(paste0("Hos_", age_filter, "_q025pos"))) %>%
        rename_with(~"N_u", matches(paste0("Hos_", age_filter, "_q975pos")))
    }
    
    ci_hos <- df_tmp %>%
      transmute(
        ISOCountry,
        hos_R    = R,
        hos_R_ci = sprintf("%.1f (%.1f-%.1f)", R, R_l, R_u),
        hos_N    = N,
        hos_N_ci = sprintf(
          "%s (%s–%s)",
          comma(round(N, 0)),
          comma(round(N_l, 0)),
          comma(round(N_u, 0))
        )
      )
    
    raw_age <- case_when(
      age_filter == "0-<6m"   ~ "0006",
      age_filter == "6-<12m"  ~ "0612",
      age_filter == "12-<60m" ~ "1260",
      age_filter == "0-<60m"  ~ "0060",
      age_filter == "0-<12m"  ~ "0012"
    )
    
    ci_mort_att <- df_sum_all_DeCoDe %>%
      mutate(ISOCountry = toupper(trimws(ISOCountry))) %>%
      mutate(across(contains('_N_'),
                    function(x) {
                      case_when(
                        x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                        x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                        TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                      )
                    }),
             across(contains('_R_'),~round(.x*100,1))) %>%
      select(
        ISOCountry,
        r  = !!sym(paste0("m", raw_age, "_R_500")),
        rl = !!sym(paste0("m", raw_age, "_R_025")),
        ru = !!sym(paste0("m", raw_age, "_R_975")),
        n  = !!sym(paste0("m", raw_age, "_N_500")),
        nl = !!sym(paste0("m", raw_age, "_N_025")),
        nu = !!sym(paste0("m", raw_age, "_N_975"))
      ) %>%
      transmute(
        ISOCountry,
        att_R    = r,
        att_R_ci = sprintf("%.1f (%.1f–%.1f)", r, rl, ru),
        att_N    = n,
        att_N_ci = sprintf(
          "%s (%s–%s)",
          comma(round(n, 0)),
          comma(round(nl, 0)),
          comma(round(nu, 0))
        )
      )
    
    ci_mort_ass <- df_sum_all_NP %>%
      mutate(ISOCountry = toupper(trimws(ISOCountry))) %>%
      mutate(across(contains('_N_'),
                    function(x) {
                      case_when(
                        x < 10 ~ round(x),                    # 个位数：保留原样（四舍五入到整数）
                        x < 100 ~ round(x / 10) * 10,         # 两位数：末尾取0（如 86 -> 90）
                        TRUE ~ round(x / 100) * 100           # 三位及以上：最后两位取0（如 1583 -> 1600）
                      )
                    }),
             across(contains('_R_'),~round(.x*100,1))) %>%
      select(
        ISOCountry,
        r  = !!sym(paste0("m", raw_age, "_R_500")),
        rl = !!sym(paste0("m", raw_age, "_R_025")),
        ru = !!sym(paste0("m", raw_age, "_R_975")),
        n  = !!sym(paste0("m", raw_age, "_N_500")),
        nl = !!sym(paste0("m", raw_age, "_N_025")),
        nu = !!sym(paste0("m", raw_age, "_N_975"))
      ) %>%
      transmute(
        ISOCountry,
        ass_R    = r,
        ass_R_ci = sprintf("%.1f (%.1f–%.1f)", r, rl, ru),
        ass_N    = n,
        ass_N_ci = sprintf(
          "%s (%s–%s)",
          comma(round(n, 0)),
          comma(round(nl, 0)),
          comma(round(nu, 0))
        )
      )
    
    ci_inc %>%
      left_join(ci_hos,      by = "ISOCountry") %>%
      left_join(ci_mort_att, by = "ISOCountry") %>%
      left_join(ci_mort_ass, by = "ISOCountry")
    
  })
  
  filtered_map_data <- reactive({
    req(input$select_metric, input$select_age)
    df_sub <- df_master %>% filter(metric == input$select_metric, AGEGR == input$select_age) %>% select(ISOCountry, CountryName, value_to_plot = !!sym(input$select_type)) %>% distinct(ISOCountry, .keep_all = TRUE)
    world_sf %>% left_join(df_sub, by = c("iso_a3" = "ISOCountry")) %>% left_join(all_metrics_broad_data(), by = c("iso_a3" = "ISOCountry")) %>%
      mutate(is_lmic = iso_a3 %in% lmic_countries, ISO2_lower = tolower(trimws(iso_a2)))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = 20, lat = 10, zoom = 2)
  })
  
  observe({
    req(filtered_map_data())
    map_data <- filtered_map_data()
    val <- map_data$value_to_plot
    val_clean <- val[is.finite(val) & !is.na(val)]
    
    leafletProxy("map") %>% clearControls() %>% clearShapes()
    if (length(val_clean) == 0) return()
    
    min_val <- min(val_clean, na.rm = TRUE)
    max_val <- max(val_clean, na.rm = TRUE)
    
    if (input$select_type == "R") {
      color_palette <- c("#2b83ba", "#f7f7f7", "#d7191c")
      legend_title <- "Rate"
    } else {
      color_palette <- c("#1b7837", "#f7f7f7", "#762a83")
      legend_title <- "Number of cases"
    }
    
    pal <- colorNumeric(palette = color_palette, domain = c(min_val, max_val), na.color = "#cccccc")
    
    # ⚙️【完全重构学术级 Popup 小表 — 固定两表列宽完美对齐】
    popups <- sapply(1:nrow(map_data), function(idx) {
      row <- map_data[idx, ]
      if (is.na(row$value_to_plot)) return(as.character(NA))
      
      flag_iso <- ifelse(is.na(row$ISO2_lower) || row$ISO2_lower == "", "un", row$ISO2_lower)
      c_name <- ifelse(is.na(row$CountryName), row$iso_a3, row$CountryName)
      income_status <- ifelse(row$is_lmic, "LMIC", "Non-LMIC")
      paste0(
        # 头部
        '<div style="display:flex; justify-content:space-between; align-items:center; margin-bottom:12px; padding-bottom:8px; border-bottom:2px solid #e0e0e0;">',
        '<div style="display:flex; align-items:center; gap:8px;">',
        '<img src="https://flagpedia.net/data/flags/h80/', flag_iso, '.png" style="height:16px; width:auto; border:1px solid #ddd;" alt="flag">',
        '<strong style="font-size:15px; color:#2c3e50;">', c_name, '</strong>',
        '</div>',
        '<span style="background:', ifelse(income_status == "LMIC", "#e74c3c", "#95a5a6"), '; color:white; padding:2px 8px; border-radius:12px; font-size:10px; font-weight:bold;">', income_status, '</span>',
        '</div>',
        
        # Rate 表格 - 使用 CSS Grid
        '<div style="margin-bottom:12px;">',
        '<div style="background:#2b83ba; color:white; padding:6px 10px; border-radius:4px 4px 0 0; font-weight:bold; font-size:12px;">📊 Rate (95% UI)</div>',
        '<div style="display:grid; grid-template-columns:1fr auto; gap:4px 8px; padding:8px; background:#f9f9f9; border-radius:0 0 4px 4px;">',
        '<div style="color:#555; font-weight:500;">RSV-associated ALRI incidence</div>',
        '<div style="text-align:right; font-weight:bold; color:#d7191c;">', ifelse(is.na(row$inc_R), "NA", row$inc_R_ci), '</div>',
        '<div style="color:#555; font-weight:500;">RSV-associated ALRI hospital admission</div>',
        '<div style="text-align:right; font-weight:bold; color:#d7191c;">', ifelse(is.na(row$hos_R), "NA", row$hos_R_ci), '</div>',
        '<div style="color:#555; font-weight:500;">RSV-attributable mortality</div>',
        '<div style="text-align:right; font-weight:bold; color:#d7191c;">', ifelse(is.na(row$att_R), "NA", row$att_R_ci), '</div>',
        '<div style="color:#555; font-weight:500;">RSV-associated mortality</div>',
        '<div style="text-align:right; font-weight:bold; color:#d7191c;">', ifelse(is.na(row$ass_R), "NA", row$ass_R_ci), '</div>',
        '</div>',
        
        # Cases 表格
        '<div>',
        '<div style="background:#1b7837; color:white; padding:6px 10px; border-radius:4px 4px 0 0; font-weight:bold; font-size:12px;">🔢 Absolute Cases (95% UI)</div>',
        '<div style="display:grid; grid-template-columns:1fr auto; gap:4px 8px; padding:8px; background:#f9f9f9; border-radius:0 0 4px 4px;">',
        '<div style="color:#555; font-weight:500;">RSV-associated ALRI incidence</div>',
        '<div style="text-align:right; font-weight:bold; color:#762a83;">', ifelse(is.na(row$inc_N), "NA", row$inc_N_ci), '</div>',
        '<div style="color:#555; font-weight:500;">RSV-associated ALRI hospital admission</div>',
        '<div style="text-align:right; font-weight:bold; color:#762a83;">', ifelse(is.na(row$hos_N), "NA", row$hos_N_ci), '</div>',
        '<div style="color:#555; font-weight:500;">RSV-attributable mortality</div>',
        '<div style="text-align:right; font-weight:bold; color:#762a83;">', ifelse(is.na(row$att_N), "NA", row$att_N_ci), '</div>',
        '<div style="color:#555; font-weight:500;">RSV-associated mortality</div>',
        '<div style="text-align:right; font-weight:bold; color:#762a83;">', ifelse(is.na(row$ass_N), "NA", row$ass_N_ci), '</div>',
        '</div>',
        '</div>',
        '</div>'
      )
    })
    
    labels <- sprintf("<strong>%s</strong>", ifelse(is.na(map_data$CountryName), map_data$iso_a3, map_data$CountryName)) %>% lapply(htmltools::HTML)
    
    leafletProxy("map", data = map_data) %>%
      addPolygons(
        layerId = ~iso_a3, fillColor = ~pal(value_to_plot), weight = ~ifelse(is_lmic, 1.5, 0.8), opacity = 1,
        color = ~ifelse(is_lmic, "#2c3e50", "#cccccc"), fillOpacity = 0.75,
        highlightOptions = highlightOptions(weight = 2, color = "#ff7800", fillOpacity = 0.85, bringToFront = TRUE),
        label = labels, popup = popups, 
        popupOptions = popupOptions(maxWidth = 500, minWidth = 460, closeOnClick = FALSE),
        group = "countries"
      ) %>%
      addLegend(
        pal = pal, values = ~value_to_plot, opacity = 0.8, title = legend_title, position = "bottomright",
        labFormat = labelFormat(transform = function(x) if (input$select_type == "N") round(x, 0) else round(x, 2))
      )
  })
  output$grid_style_trigger <- renderUI({
    if (input$show_floating_card == "TRUE" && length(input$comparison_countries) > 0) {
      # 状态 A: 当需要显示时 —— 地图占 2 份宽度，图表卡片占 1 份宽度
      tags$style(HTML("
      #map_box { 
        flex: 2; 
        width: 0; /* 触发 flex 精准按比例分配的核心小技巧 */
        height: 100%; 
      }
      #main_flex_container > div[data-display-if] { 
        flex: 1; 
        width: 0; 
        height: 100%; 
        display: block !important; 
      }
      #floating_card_box { 
        height: 100% !important; 
      }
    "))
    } else {
      # 状态 B: 当不展示或没选国家时 —— 图表卡片彻底隐匿，地图完全撑满 100%
      tags$style(HTML("
      #map_box { 
        flex: 1;       /* 此时独霸整个容器的伸缩权重 */
        width: 100%; 
        height: 100%; 
      }
      #main_flex_container > div[data-display-if] { 
        display: none !important; /* 彻底移除，不占空间 */
        width: 0px !important;
      }
    "))
    }
  })

  output$bar_plot <- renderPlotly({
    # 1. 统一阻断检查
    req(input$comparison_countries, input$comparison_metrics, input$select_age, input$select_type)
    
    # 2. 数据清洗与过滤
    df_bar <- df_master %>% 
      filter(
        ISOCountry %in% input$comparison_countries, 
        metric %in% input$comparison_metrics, 
        AGEGR == input$select_age
      ) %>% 
      select(ISOCountry, CountryName, metric, Value = !!sym(input$select_type)) %>% 
      mutate(Display_Name = coalesce(CountryName, ISOCountry)) # 用 coalesce 代替 ifelse 更高效
    
    # 3. 空数据安全退出
    if (nrow(df_bar) == 0 || all(is.na(df_bar$Value))) {
      return(plotly_empty() %>% layout(title = "No descriptive data available"))
    }
    
    # 4. 定义 metric 专属颜色映射表
    # 在这里配置，如果未来有新增的指标，直接往列表里加对应的颜色即可
    color_map <- c(
      "RSV-associated ALRI incidence"         = "#abdda4", # 浅绿
      "RSV-associated ALRI hospital admission" = "#2b83ba", # 蓝色
      "RSV-attributable mortality"            = "#d7191c", # 红色
      "RSV-associated mortality"             = "#fdae61"  # 橙色
    )
    
    # 5. 循环绘制每个指标的子图
    plot_list <- lapply(input$comparison_metrics, function(m) {
      df_sub <- df_bar %>% filter(metric == m)
      
      # 根据数据维度（率或绝对数）格式化条形图上的标签文字
      is_rate <- input$select_type == "R"
      local_text <- if (is_rate) sprintf("%.1f", round(df_sub$Value,1)) else comma(round(df_sub$Value, 0))
      
      # 动态匹配颜色与 Y 轴标题（包含具体指标简称，避免多子图时混淆）
      bar_color <- coalesce(color_map[m], "#7f8c8d") # 找不到匹配时用灰色兜底
      metric_short <- switch(m,
                             "RSV-associated ALRI incidence"          = "Incidence",
                             "RSV-associated ALRI hospital admission" = "Hospital admission",
                             "RSV-attributable mortality"             = "Attributable mortality",
                             "RSV-associated mortality"              = "Associated mortality",
                             m # 如果没匹配上，用原始名称兜底
      )
      y_title   <- metric_short
      
      plot_ly(
        data = df_sub, 
        x = ~Display_Name, 
        y = ~Value, 
        type = "bar", 
        name = m, 
        marker = list(color = bar_color), 
        text = local_text, 
        textposition = 'auto', 
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = y_title, titlefont = list(size = 11)) # 稍微缩小字体防止挤压
        )
    })
    
    # 6. 合并子图并强制锁定高度自适应
    subplot(plot_list, nrows = length(plot_list), shareX = TRUE, titleY = TRUE, margin = 0.08) %>% 
      layout(
        margin = list(l = 55, r = 10, b = 30, t = 15), 
        showlegend = FALSE,
        autosize = TRUE # 关键：强制让 plotly 听从外层 CSS Grid 的高度安排，绝不自己乱涨
      )
  })
  
}