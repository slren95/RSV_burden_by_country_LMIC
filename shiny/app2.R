library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)
library(rio)
library(scales)
library(leaflet)
library(sf)
library(rnaturalearth)
library(plotly)
library(gt) 

# ==========================================
# 1. 数据读取与预处理 (Data Loading)
# ==========================================


# tryCatch({
#   df_sum_all_DeCoDe <- import('rda/df_sum_all_DeCoDe.rds', trust=TRUE)
#   df_sum_all_NP <- import('rda/df_sum_all_NP.rds', trust=TRUE)
#   RF.res.impute2 <- import('rda/RF.res.impute2.rds', trust=TRUE)
#   df_hos_by_country2_1000 <- import("rda/df_hos_by_country2_1000.rds", trust=TRUE)
#   df_lmic_imputed <- import('rda/df_lmic_imputed.rds', trust=TRUE)
# }, error = function(e) {
#   stop("Error loading data files. Please check file paths: ", e$message)
# })
# 
# save.image('shiny/shiny_data.RData')
# load('shiny/shiny_data.RData')
load('shiny_data.RData')

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3 = iso_a3_eh, iso_a2, economy, geometry) %>%
  filter(!is.na(iso_a3)) %>%
  mutate(iso_a3 = toupper(trimws(iso_a3)))


# 统一提取各指标的 R 和 N 矩阵点估计值
df_inc <- RF.res.impute2 %>%
  reframe(across(c(IR, N), list(q500 = ~quantile(.x, 0.5, na.rm = TRUE))), .by = c(ISOCountry, AGEGR)) %>%
  transmute(ISOCountry, AGEGR, metric = "RSV-associated ALRI incidence", R = IR_q500, N = N_q500)

df_hos <- df_hos_by_country2_1000 %>%
  select(ISOCountry, contains('q500'), -c(`Rate_12-<60m_q500`, `Hos_12-<60m_q500`)) %>% 
  pivot_longer(cols = contains("_"), names_to = c("type", "AGEGR"), names_pattern = "(.*)_(.*)_q500.*", values_to = "value") %>%
  mutate(type = case_when(type == "Hos" ~ "N", type == "Rate" ~ "R")) %>%
  pivot_wider(id_cols = c(ISOCountry, AGEGR), names_from = type, values_from = value) %>%
  mutate(metric = "RSV-associated ALRI hospital admission")

df_mort_att <- df_sum_all_DeCoDe %>%
  select(ISOCountry, ends_with('500')) %>%
  pivot_longer(cols = starts_with("m"), names_to = c("AGEGR", "type"), names_pattern = "m(.*)_([NR])_500", values_to = "value") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(AGEGR = recode(AGEGR, "0006" = "0-<6m", "0612" = "6-<12m", "1260" = "12-<60m", "0060" = "0-<60m", "0012" = "0-<12m"),
         R = R * 100, metric = "RSV-attributable mortality")

df_mort_ass <- df_sum_all_NP %>%
  select(ISOCountry, ends_with('500')) %>%
  pivot_longer(cols = starts_with("m"), names_to = c("AGEGR", "type"), names_pattern = "m(.*)_([NR])_500", values_to = "value") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(AGEGR = recode(AGEGR, "0006" = "0-<6m", "0612" = "6-<12m", "1260" = "12-<60m", "0060" = "0-<60m", "0012" = "0-<12m"),
         R = R * 100, metric = "RSV-associated mortality")

df_master <- bind_rows(df_inc, df_hos, df_mort_att, df_mort_ass) %>%
  mutate(ISOCountry = toupper(trimws(ISOCountry))) %>%
  left_join(df_lmic_imputed %>% select(ISOCountry, CountryName) %>% distinct(), by = "ISOCountry") %>%
  left_join(world_sf %>% select(iso_a3,iso_a2),by=c('ISOCountry'='iso_a2'))

country_choices <- df_master %>% 
  filter(!is.na(CountryName)) %>% 
  distinct(ISOCountry, CountryName) %>% 
  mutate(label = paste0(CountryName, " (", ISOCountry, ")")) %>%
  { setNames(.$ISOCountry, .$label) }

lmic_countries <- df_lmic_imputed %>% pull(ISOCountry) %>% unique()

# ==========================================
# 2. Shiny 界面设计 (UI)
# ==========================================
my_theme <- bs_theme(version = 5, bootswatch = "yeti", primary = "#2c3e50")

ui <- page_navbar(
  title = "RSV disease burden in low and middle income country",
  theme = my_theme,
  
  # -------------------- Tab 1: Map 交互系统 --------------------
  nav_panel(
    title = "Map",
    value = "map_tab",
    layout_sidebar(
      sidebar = sidebar(
        width = 340, open = TRUE, bg = "#fdfdfd",
        selectInput("select_metric", "Map Base Indicator:",
                    choices = c("RSV-associated ALRI incidence" = "RSV-associated ALRI incidence", "RSV-associated ALRI hospital admission" = "RSV-associated ALRI hospital admission", "RSV-attributable mortality" = "RSV-attributable mortality", "RSV-associated mortality" = "RSV-associated mortality")),
        selectInput("select_age", "Age Stratification:",
                    choices = c("0-6 months" = "0-<6m", "6-12 months" = "6-<12m", "0-12 months" = "0-<12m", "12-60 months" = "12-<60m", "0-60 months" = "0-<60m")),
        radioButtons("select_type", "Data Dimension:", choices = c("Rate" = "R", "Absolute Cases (N)" = "N"), inline = TRUE),
        hr(),
        selectizeInput("comparison_metrics", "Compare Indicators:", choices = c("RSV-associated ALRI incidence" = "RSV-associated ALRI incidence", "RSV-associated ALRI hospital admission" = "RSV-associated ALRI hospital admission", "RSV-attributable mortality" = "RSV-attributable mortality", "RSV-associated mortality" = "RSV-associated mortality"), selected = c("RSV-associated ALRI incidence", "RSV-associated ALRI hospital admission"), multiple = TRUE, options = list(plugins = list('remove_button'))),
        selectizeInput("comparison_countries", "Selected Countries:", choices = country_choices, selected = c("IND", "BGD"), multiple = TRUE, options = list(plugins = list('remove_button'))),
        hr(),
        div(style = "padding: 2px; font-size: 12px; color: #7f8c8d; line-height: 1.4;",
            p("Notes:", style = "font-weight: bold; margin-bottom: 5px; color: #2c3e50;"),
            p("• Incidence & Admission Rate: /1,000 person-years"), p("• Mortality Rate: /100,000 person-years"), p("• LMIC Status: Solid border")
        )
      ),
      div(style = "position: relative; overflow: hidden;",
          leafletOutput("map", height = "calc(100vh - 120px)"),
          absolutePanel(id = "controls", fixed = TRUE, draggable = TRUE, top = "90px", right = "20px", width = "580px", height = "auto", uiOutput("floating_card_ui"))
      )
    )
  ),
  
  # -------------------- Tab 2: Table 数据矩阵大表 --------------------
  nav_panel(
    title = "Table",
    value = "table_tab",
    layout_sidebar(
      sidebar = sidebar(
        width = '15%', open = TRUE, bg = "#fdfdfd",
        selectInput("table_age", "Age Stratification:", choices = c("0-6 months" = "0-<6m", "6-12 months" = "6-<12m", "0-12 months" = "0-<12m", "12-60 months" = "12-<60m", "0-60 months" = "0-<60m")),
        hr(),
        div(style = "padding: 2px; font-size: 12px; color: #7f8c8d; line-height: 1.4;",
            p("Notes:", style = "font-weight: bold; margin-bottom: 5px; color: #2c3e50;"),
            p("• Incidence & Admission Rate: /1,000 person-years"), p("• Mortality Rate: /100,000 person-years")
        ),
        downloadButton("download_table_csv", "Download", class = "btn-primary", style = "width: 100%;")
      ),
      card(
        min_height = "calc(100vh - 140px)",
        card_header(textOutput("matrix_title"), class = "bg-light font-weight-bold"),
        card_body(
          padding = 0, 
          gt_output("national_gt_table")
        )
      )
    )
  ),
  # -------------------- 新增 Tab 3: Table 2 跨年龄组指标矩阵 --------------------
  nav_panel(
    title = "Table (By Metric)",
    value = "table2_tab",
    layout_sidebar(
      sidebar = sidebar(
        width = '15%', open = TRUE, bg = "#fdfdfd",
        selectInput("table2_metric", "Select Disease Indicator:",
                    choices = c("RSV-associated ALRI incidence" = "RSV-associated ALRI incidence", 
                                "RSV-associated ALRI hospital admission" = "RSV-associated ALRI hospital admission", 
                                "RSV-attributable mortality" = "RSV-attributable mortality", 
                                "RSV-associated mortality" = "RSV-associated mortality")),
        hr(),
        div(style = "padding: 2px; font-size: 12px; color: #7f8c8d; line-height: 1.4;",
            p("Notes:", style = "font-weight: bold; margin-bottom: 5px; color: #2c3e50;"),
            p("• Incidence & Admission Rate: /1,000 person-years"), 
            p("• Mortality Rate: /100,000 person-years")
        ),
        downloadButton("download_table2_csv", "Download Matrix", class = "btn-primary", style = "width: 100%;")
      ),
      card(
        min_height = "calc(100vh - 140px)",
        card_header(textOutput("matrix2_title"), class = "bg-light font-weight-bold"),
        card_body(
          padding = 0, 
          gt_output("metric_gt_table")
        )
      )
    )
  )
)

# ==========================================
# 3. 后端业务逻辑 (Server)
# ==========================================
server <- function(input, output, session) {
  
  observeEvent(input$select_age, { updateSelectInput(session, "table_age", selected = input$select_age) })
  observeEvent(input$table_age, { updateSelectInput(session, "select_age", selected = input$table_age) })
  
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
    card(height = 400, full_screen = TRUE, card_header("Cross-Country Profile Workstation"), card_body(padding = 5, plotlyOutput("bar_plot", height = "100%")))
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
  
  # ==========================================
  # 4. GT 交互矩阵表格逻辑
  # ==========================================
  output$matrix_title <- renderText({
    paste("RSV disease burden by country | ", input$table_age)
  })
  
  table_matrix_data <- reactive({
    req(input$table_age)
    df_raw <- all_metrics_broad_data() %>% 
      left_join(world_sf %>% as.data.frame() %>% select(iso_a3,iso_a2),by=c('ISOCountry'='iso_a3'))
    
    df_names <- df_master %>% distinct(ISOCountry, CountryName) %>% filter(!is.na(CountryName))
    df_res <- df_raw %>% 
      left_join(df_names, by = "ISOCountry") %>%
      mutate(Country = ifelse(is.na(CountryName), ISOCountry, CountryName)) %>%
      # 💡 核心新增：先用 tolower() 确保国家代码是小写，然后把国旗的 HTML 标签直接拼到 Country 列最前面
      mutate(
        Country = paste0(
          '<img src="https://flagpedia.net/data/flags/h80/', tolower(iso_a2), '.png" ',
          'style="height:14px; width:auto; border:1px solid #ddd; margin-right:8px; vertical-align:middle;" alt="flag">',
          Country
        )
      ) %>%
      select(Country, everything(), -CountryName,-iso_a2,
             v1_R = inc_R, txt1_R = inc_R_ci,
             v2_R = hos_R, txt2_R = hos_R_ci,
             v3_R = att_R, txt3_R = att_R_ci,
             v4_R = ass_R, txt4_R = ass_R_ci,
             # Number 组
             v1_N = inc_N, txt1_N = inc_N_ci,
             v2_N = hos_N, txt2_N = hos_N_ci,
             v3_N = att_N, txt3_N = att_N_ci,
             v4_N = ass_N, txt4_N = ass_N_ci
      ) %>%
      mutate(across(c(v1_N,v2_N,v3_N,v4_N), ~log(.x+1)))
  })
  
  # Table 2
  # 针对特定指标提取所有年龄组的 Rate 和 Cases
  table2_matrix_data <- reactive({
    req(input$table2_metric)
    target_metric <- input$table2_metric
    
    # 1. 从 df_master 动态提取基础点估计
    df_sub <- df_master %>% 
      filter(metric == target_metric) %>%
      select(ISOCountry, CountryName, AGEGR, R, N)
    
    # 2. 为每个年龄组匹配 95% CI 文本
    # 🛠️【已修复】：修正分位数变量名，由 N_025 / N_975 改为正确的 N_q025 / N_q975
    ci_inc <- RF.res.impute2 %>%
      reframe(across(c(IR, N), list(q500 = ~quantile(.x, 0.5, na.rm = TRUE), 
                                    q025 = ~quantile(.x, 0.025, na.rm = TRUE), 
                                    q975 = ~quantile(.x, 0.975, na.rm = TRUE))), .by = c(ISOCountry, AGEGR)) %>%
      transmute(ISOCountry = toupper(trimws(ISOCountry)), AGEGR, 
                R_ci = sprintf("%.1f (%.1f–%.1f)", IR_q500, IR_q025, IR_q975), 
                N_ci = sprintf("%s (%s–%s)", comma(round(N_q500,0)), comma(round(N_q025,0)), comma(round(N_q975,0))))
    
    # 提取 Hospitalization CI
    df_hos_base <- df_hos_by_country2_1000 %>% mutate(ISOCountry = toupper(trimws(ISOCountry)))
    ages_hos <- c("0-<6m", "6-<12m", "0-<12m", "12-<60m", "0-<60m")
    
    list_hos <- lapply(ages_hos, function(a) {
      suffix <- if(a == "12-<60m") "pos" else ""
      col_R <- paste0("Rate_", a, "_q500", suffix)
      col_Rl <- paste0("Rate_", a, "_q025", suffix)
      col_Ru <- paste0("Rate_", a, "_q975", suffix)
      col_N <- paste0("Hos_", a, "_q500", suffix)
      col_Nl <- paste0("Hos_", a, "_q025", suffix)
      col_Nu <- paste0("Hos_", a, "_q975", suffix)
      
      if(col_R %in% names(df_hos_base)) {
        df_hos_base %>% 
          select(ISOCountry, r=!!sym(col_R), rl=!!sym(col_Rl), ru=!!sym(col_Ru), n=!!sym(col_N), nl=!!sym(col_Nl), nu=!!sym(col_Nu)) %>%
          transmute(ISOCountry, AGEGR = a, R_ci = sprintf("%.1f (%.1f-%.1f)", r, rl, ru), N_ci = sprintf("%s (%s–%s)", comma(round(n,0)), comma(round(nl,0)), comma(round(nu,0))))
      } else { NULL }
    })
    ci_hos <- bind_rows(list_hos)
    
    # 提取 Mortality CI
    extract_mort_ci <- function(df_raw) {
      age_map <- c("0-<6m"="0006", "6-<12m"="0612", "12-<60m"="1260", "0-<60m"="0060", "0-<12m"="0012")
      df_mort_base <- df_raw %>% mutate(ISOCountry = toupper(trimws(ISOCountry)))
      
      lapply(names(age_map), function(a) {
        raw_age <- age_map[a]
        df_mort_base %>%
          select(ISOCountry, r=!!sym(paste0("m",raw_age,"_R_500")), rl=!!sym(paste0("m",raw_age,"_R_025")), ru=!!sym(paste0("m",raw_age,"_R_975")), n=!!sym(paste0("m",raw_age,"_N_500")), nl=!!sym(paste0("m",raw_age,"_N_025")), nu=!!sym(paste0("m",raw_age,"_N_975"))) %>%
          transmute(ISOCountry, AGEGR = a, R_ci = sprintf("%.1f (%.1f–%.1f)", r*100, rl*100, ru*100), N_ci = sprintf("%s (%s–%s)", comma(round(n,0)), comma(round(nl,0)), comma(round(nu,0))))
      }) %>% bind_rows()
    }
    
    ci_mort_att <- extract_mort_ci(df_sum_all_DeCoDe)
    ci_mort_ass <- extract_mort_ci(df_sum_all_NP)
    
    ci_master <- case_when(
      target_metric == "RSV-associated ALRI incidence" ~ list(ci_inc),
      target_metric == "RSV-associated ALRI hospital admission" ~ list(ci_hos),
      target_metric == "RSV-attributable mortality" ~ list(ci_mort_att),
      target_metric == "RSV-associated mortality" ~ list(ci_mort_ass)
    )[[1]]
    
    # 3. 合并转换大宽表
    df_merge <- df_sub %>%
      left_join(ci_master, by = c("ISOCountry", "AGEGR"))
    
    df_wide <- df_merge %>%
      pivot_wider(
        id_cols = c(ISOCountry, CountryName),
        names_from = AGEGR,
        values_from = c(R, N, R_ci, N_ci),
        names_glue = "{.value}_{AGEGR}"
      ) %>%
      left_join(world_sf %>% as.data.frame() %>% select(iso_a3, iso_a2), by = c("ISOCountry" = "iso_a3")) %>%
      mutate(Country = ifelse(is.na(CountryName), ISOCountry, CountryName)) %>%
      mutate(
        Country = paste0(
          '<img src="https://flagpedia.net/data/flags/h80/', tolower(iso_a2), '.png" ',
          'style="height:14px; width:auto; border:1px solid #ddd; margin-right:8px; vertical-align:middle;" alt="flag">',
          Country
        )
      ) %>%
      select(Country, ISOCountry, everything(), -CountryName, -iso_a2)
    
    # 对所有 N_ 开头的点估计列（如 N_0-<6m 等）做 log 转换以适配背景着色
    col_v_n <- names(df_wide)[grepl("^N_[0-9]", names(df_wide))]
    df_wide <- df_wide %>% mutate(across(all_of(col_v_n), ~log(.x + 1)))
    
    df_wide
  })
  
  output$national_gt_table <- render_gt({
    req(table_matrix_data()) 
    raw_df <- table_matrix_data()
    
    gt_obj <- gt(raw_df) %>%
      # 💡 核心新增：必须加这一行！告诉 gt 去把 Country 列里的 HTML 源码真正渲染成图片图标
      fmt_markdown(columns = Country) %>%
      
      # 1️⃣ 全自动独立列染色
      data_color(
        columns = c(v1_R, v2_R, v3_R, v4_R, v1_N, v2_N, v3_N, v4_N),
        fn = scales::col_numeric(palette = "Blues", domain = NULL)
      ) %>%
      
      # 2️⃣ 简单直接的文本替换
      text_transform(locations = cells_body(columns = v1_R), fn = function(x) raw_df$txt1_R) %>%
      text_transform(locations = cells_body(columns = v2_R), fn = function(x) raw_df$txt2_R) %>%
      text_transform(locations = cells_body(columns = v3_R), fn = function(x) raw_df$txt3_R) %>%
      text_transform(locations = cells_body(columns = v4_R), fn = function(x) raw_df$txt4_R) %>%
      text_transform(locations = cells_body(columns = v1_N), fn = function(x) raw_df$txt1_N) %>%
      text_transform(locations = cells_body(columns = v2_N), fn = function(x) raw_df$txt2_N) %>%
      text_transform(locations = cells_body(columns = v3_N), fn = function(x) raw_df$txt3_N) %>%
      text_transform(locations = cells_body(columns = v4_N), fn = function(x) raw_df$txt4_N) %>%
      
      cols_hide(columns = c(matches("^txt"), ISOCountry)) %>%
      tab_spanner(label = "Rate", columns = c(v1_R, v2_R, v3_R, v4_R)) %>%
      tab_spanner(label = "Number of Cases", columns = c(v1_N, v2_N, v3_N, v4_N)) %>%
      
      # 5️⃣ 统一命名底层的小表头标签
      cols_label(
        Country = "Country",
        v1_R = "RSV-associated ALRI incidence", v2_R = "RSV-associated ALRI hospital admission",
        v3_R = "RSV-attributable mortality", v4_R = "RSV-associated mortality",
        v1_N = "RSV-associated ALRI incidence", v2_N = "RSV-associated ALRI hospital admission",
        v3_N = "RSV-attributable mortality", v4_N = "RSV-associated mortality"
      ) %>%
      
      # 6️⃣ 样式与对齐
      # 💡 细节优化：加了国旗图标后，国家名建议改成左对齐（align = "left"），数值列继续居中，这样排版极具美感
      cols_align(align = "left", columns = Country) %>% 
      cols_align(align = "center", columns = -Country) %>%
      cols_label_with(columns = c(Country), fn = toupper) %>%
      sub_missing(missing_text = "—") %>%
      
      # 7️⃣ 开启交互式功能
      opt_interactive(
        use_compact_mode = TRUE, 
        use_search = TRUE, 
        use_sorting = TRUE, 
        use_page_size_select = FALSE, 
        page_size_default = 150
      ) %>%
      tab_options(
        table.font.size = "12px", 
        column_labels.font.weight = "bold", 
        column_labels.background.color = "#f4f6f7"
      )
    
    gt_obj
  })
  
  # Table 2 动态标题
  output$matrix2_title <- renderText({
    paste("RSV age-stratified matrix | ", input$table2_metric)
  })
  
  # Table 2 gt 渲染器
  output$metric_gt_table <- render_gt({
    req(table2_matrix_data())
    raw_df <- table2_matrix_data()
    
    # 定义 5 个标准的年龄组标签
    age_groups <- c("0-<6m", "6-<12m", "0-<12m", "12-<60m", "0-<60m")
    
    # 动态匹配列名
    cols_R_val  <- paste0("R_", age_groups)
    cols_R_txt  <- paste0("R_ci_", age_groups)
    cols_N_val  <- paste0("N_", age_groups)
    cols_N_txt  <- paste0("N_ci_", age_groups)
    
    gt_obj <- gt(raw_df) %>%
      fmt_markdown(columns = Country) %>%
      
      # 1️⃣ 渐变色背景（基于点估计）
      data_color(
        columns = c(all_of(cols_R_val), all_of(cols_N_val)),
        fn = scales::col_numeric(palette = "Purples", domain = NULL) # 换个颜色体系以示区分
      )
    
    # 2️⃣ 循环进行文本替换 (把点估计数字替换为 CI 字符串)
    for(i in seq_along(age_groups)) {
      local_txt_R <- raw_df[[cols_R_txt[i]]]
      local_txt_N <- raw_df[[cols_N_txt[i]]]
      
      gt_obj <- gt_obj %>%
        text_transform(locations = cells_body(columns = !!sym(cols_R_val[i])), fn = function(x) local_txt_R) %>%
        text_transform(locations = cells_body(columns = !!sym(cols_N_val[i])), fn = function(x) local_txt_N)
    }
    
    # 3️⃣ 隐藏原始文本列和代码列
    gt_obj <- gt_obj %>%
      cols_hide(columns = c(all_of(cols_R_txt), all_of(cols_N_txt), ISOCountry)) %>%
      
      # 4️⃣ 构建 Rate 和 Cases 的大表头
      tab_spanner(label = "Rate (per person-years)", columns = all_of(cols_R_val)) %>%
      tab_spanner(label = "Number of Cases", columns = all_of(cols_N_val)) %>%
      
      # 5️⃣ 命名小表头（展示年龄段）
      cols_label(
        Country = "Country",
        `R_0-<6m` = "0-6m", `R_6-<12m` = "6-12m", `R_0-<12m` = "0-12m", `R_12-<60m` = "12-60m", `R_0-<60m` = "0-60m",
        `N_0-<6m` = "0-6m", `N_6-<12m` = "6-12m", `N_0-<12m` = "0-12m", `N_12-<60m` = "12-60m", `N_0-<60m` = "0-60m"
      ) %>%
      
      # 6️⃣ 样式、对齐与交互
      cols_align(align = "left", columns = Country) %>% 
      cols_align(align = "center", columns = -Country) %>%
      sub_missing(missing_text = "—") %>%
      opt_interactive(
        use_compact_mode = TRUE, 
        use_search = TRUE, 
        use_sorting = TRUE, 
        use_page_size_select = FALSE, 
        page_size_default = 150
      ) %>%
      tab_options(
        table.font.size = "12px", 
        column_labels.font.weight = "bold", 
        column_labels.background.color = "#f4f6f7"
      )
    
    gt_obj
  })
  
  # 数据下载处理器
  output$download_table_csv <- downloadHandler(
    filename = function() { 
      safe_age <- gsub("[^A-Za-z0-9]+", "_", input$table_age)
      sprintf("RSV_burder_by_country_%s.csv", safe_age)
    },
    content = function(file) {
      raw_mat <- all_metrics_broad_data()
      df_names <- df_master %>% distinct(ISOCountry, CountryName) %>% filter(!is.na(CountryName))
      export_df <- raw_mat %>% left_join(df_names, by = "ISOCountry") %>%
        relocate(CountryName,ISOCountry)
      write_csv(export_df, file)
    }
  )
}

shinyApp(ui, server)