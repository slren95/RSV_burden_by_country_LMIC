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
    ci_inc <- RF.res.impute2 %>% filter(AGEGR == age_filter) %>%
      reframe(across(c(IR, N), list(q500 = ~quantile(.x, 0.5, na.rm = TRUE), q025 = ~quantile(.x, 0.025, na.rm = TRUE), q975 = ~quantile(.x, 0.975, na.rm = TRUE))), .by = ISOCountry) %>%
      transmute(ISOCountry = toupper(trimws(ISOCountry)), inc_R = IR_q500, inc_R_ci = sprintf("%.1f (%.1f–%.1f)", IR_q500, IR_q025, IR_q975),
                inc_N = N_q500, inc_N_ci = sprintf("%s (%s–%s)", comma(round(N_q500,0)), comma(round(N_q025,0)), comma(round(N_q975,0))))
    
    df_base <- df_hos_by_country2_1000 %>%
      mutate(ISOCountry = toupper(trimws(ISOCountry)))
    
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
        hos_R = R,
        hos_R_ci = sprintf("%.1f (%.1f-%.1f)", R, R_l, R_u),
        hos_N = N,
        hos_N_ci = sprintf("%s (%s–%s)",
                           comma(round(N,0)),
                           comma(round(N_l,0)),
                           comma(round(N_u,0)))
      )
    
    raw_age <- case_when(age_filter == "0-<6m" ~ "0006", age_filter == "6-<12m" ~ "0612", age_filter == "12-<60m" ~ "1260", age_filter == "0-<60m" ~ "0060", age_filter == "0-<12m" ~ "0012")
    
    ci_mort_att <- df_sum_all_DeCoDe %>% mutate(ISOCountry = toupper(trimws(ISOCountry))) %>%
      select(ISOCountry, r=!!sym(paste0("m",raw_age,"_R_500")), rl=!!sym(paste0("m",raw_age,"_R_025")), ru=!!sym(paste0("m",raw_age,"_R_975")), n=!!sym(paste0("m",raw_age,"_N_500")), nl=!!sym(paste0("m",raw_age,"_N_025")), nu=!!sym(paste0("m",raw_age,"_N_975"))) %>%
      transmute(ISOCountry, att_R = r*100, att_R_ci = sprintf("%.1f (%.1f–%.1f)", r*100, rl*100, ru*100), att_N = n, att_N_ci = sprintf("%s (%s–%s)", comma(round(n,0)), comma(round(nl,0)), comma(round(nu,0))))
    
    ci_mort_ass <- df_sum_all_NP %>% mutate(ISOCountry = toupper(trimws(ISOCountry))) %>%
      select(ISOCountry, r=!!sym(paste0("m",raw_age,"_R_500")), rl=!!sym(paste0("m",raw_age,"_R_025")), ru=!!sym(paste0("m",raw_age,"_R_975")), n=!!sym(paste0("m",raw_age,"_N_500")), nl=!!sym(paste0("m",raw_age,"_N_025")), nu=!!sym(paste0("m",raw_age,"_N_975"))) %>%
      transmute(ISOCountry, ass_R = r*100, ass_R_ci = sprintf("%.1f (%.1f–%.1f)", r*100, rl*100, ru*100), ass_N = n, ass_N_ci = sprintf("%s (%s–%s)", comma(round(n,0)), comma(round(nl,0)), comma(round(nu,0))))
    
    ci_inc %>% left_join(ci_hos, by = "ISOCountry") %>% left_join(ci_mort_att, by = "ISOCountry") %>% left_join(ci_mort_ass, by = "ISOCountry")
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
        '<div style="background:#2b83ba; color:white; padding:6px 10px; border-radius:4px 4px 0 0; font-weight:bold; font-size:12px;">📊 Rate (95% CI)</div>',
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
        '<div style="background:#1b7837; color:white; padding:6px 10px; border-radius:4px 4px 0 0; font-weight:bold; font-size:12px;">🔢 Absolute Cases (95% CI)</div>',
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
  
  output$floating_card_ui <- renderUI({
    if (length(input$comparison_countries) == 0) return(NULL)
    card(height = 400, full_screen = TRUE, card_header("Cross-Country Profile"), card_body(padding = 5, plotlyOutput("bar_plot", height = "100%")))
  })
  
  output$bar_plot <- renderPlotly({
    req(input$comparison_countries, input$comparison_metrics, input$select_age, input$select_type)
    df_bar <- df_master %>% filter(ISOCountry %in% input$comparison_countries, metric %in% input$comparison_metrics, AGEGR == input$select_age) %>% select(ISOCountry, CountryName, metric, Value = !!sym(input$select_type)) %>% mutate(Display_Name = ifelse(is.na(CountryName), ISOCountry, CountryName))
    if(nrow(df_bar) == 0 || all(is.na(df_bar$Value))) return(plotly_empty() %>% layout(title = "No descriptive data available"))
    
    plot_list <- lapply(input$comparison_metrics, function(m) {
      df_sub <- df_bar %>% filter(metric == m); local_text_vec <- if(input$select_type == "N") comma(round(df_sub$Value, 0)) else sprintf("%.2f", df_sub$Value)
      plot_ly(data = df_sub, x = ~Display_Name, y = ~Value, type = "bar", name = m, marker = list(color = if_else(grepl("mortality", m), "#d7191c", "#2b83ba")), text = local_text_vec, textposition = 'auto', hoverinfo = "text") %>%
        layout(yaxis = list(title = if_else(input$select_type == "R", "Rate", "Cases")), xaxis = list(title = ""))
    })
    subplot(plot_list, nrows = length(plot_list), shareX = TRUE, titleY = TRUE, margin = 0.06) %>% layout(margin = list(l = 45, r = 10, b = 30, t = 10), showlegend = FALSE)
  })
  
}