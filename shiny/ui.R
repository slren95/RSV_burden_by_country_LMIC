my_theme <- bs_theme(version = 5, bootswatch = "yeti", primary = "#2c3e50")


ui <- page_navbar(
  tags$head(
    tags$style(HTML("
    /* 基础容器：占满高度 */
    #main_grid_container {
      display: grid;
      height: calc(100vh - 100px);
      gap: 15px;
      width: 100%;
    }
    
    /* 状态 1：显示面板时的 1:2 布局 */
    #main_grid_container.show-panel {
      grid-template-rows: 1fr 2fr;
    }
    #main_grid_container.show-panel #floating_card_box { display: flex; }
    
    /* 状态 2：隐藏面板时的纯地图满屏布局 */
    #main_grid_container.hide-panel {
      grid-template-rows: 1fr;
    }
    #main_grid_container.hide-panel #floating_card_box { display: none; }
  "))
  ),
  title = tags$div(
    # 💡 核心修正：使用 inline-flex，并将自适应高度交给内容撑开，彻底切断系统默认行高的干扰
    style = "display: inline-flex; align-items: center; gap: 15px; height: 44px; padding: 0; vertical-align: middle;",
    
    # 1️⃣ Logo 先行 (上下居中对齐)
    tags$div(
      style = "display: flex; align-items: center; justify-content: center; flex: 0 0 auto; width: 50px; height: 50px; background-color: #ffffff; border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      tags$a(
        href = "https://leoly2017.github.io/group/", target = "_blank",
        tags$img(src = "logo_IDEM.png", height = "60px", width = "60px", style = "display: block; object-fit: contain;"))
    ),
    
    # 2️⃣ 文字集团：强制垂直方向居中对齐
    tags$div(
      style = "display: flex; flex-direction: column; justify-content: center; flex: 1 1 auto; height: 100%;",
      
      # 上行：系统主标题
      tags$div(
        style = "font-size: 20px; font-weight: bold; color: #ffffff; line-height: 1.2; display: block; padding: 0; margin: 0;", 
        "RSV disease burden in low and middle income countries"
      ),
      
      # 下行：作者名与通讯邮箱
      tags$div(
        style = "font-size: 11px; color: #a0aec0; margin-top: 3px; line-height: 1.1; display: block; padding: 0; margin: 0;",
        "Shaolong Ren, You Li (you.li@njmu.edu.cn)"
      )
    )
  ),
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
        checkboxInput(
          inputId = "show_floating_card", 
          label = "Show Comparison Panel", 
          value = FALSE # 默认勾选
        ),
        selectizeInput("comparison_metrics", "Compare Indicators:", choices = c("RSV-associated ALRI incidence" = "RSV-associated ALRI incidence", "RSV-associated ALRI hospital admission" = "RSV-associated ALRI hospital admission", "RSV-attributable mortality" = "RSV-attributable mortality", "RSV-associated mortality" = "RSV-associated mortality"), selected = c("RSV-associated ALRI incidence", "RSV-associated ALRI hospital admission"), multiple = TRUE, options = list(plugins = list('remove_button'))),
        selectizeInput("comparison_countries", "Selected Countries:", choices = country_choices, selected = c("IND", "BGD"), multiple = TRUE, options = list(plugins = list('remove_button'))),
        hr(),
        div(style = "padding: 2px; font-size: 12px; color: #7f8c8d; line-height: 1.4;",
            p("Notes:", style = "font-weight: bold; margin-bottom: 5px; color: #2c3e50;"),
            p("• Incidence & Admission Rate: /1,000 person-years"), p("• Mortality Rate: /100,000 person-years"), p("• LMIC Status: Solid border")
        )
      ),
      div(
        id = "main_flex_container",
        style = "display: flex; height: calc(100vh - 120px); gap: 15px; width: 100%; overflow: hidden;",
        
        # 1. 动态样式触发器：用来在 Show/Hide 时优雅地控制左右宽度分配
        uiOutput("grid_style_trigger"), 
        
        # 2. 左侧：地图组件（放在前面，保持第一位）
        card(
          id = "map_box",
          full_screen = TRUE,
          leafletOutput("map", height = "100%")
        ),
        
        # 3. 右侧：比较卡片
        conditionalPanel(
          style = "height: 100%;", # 强行让条件面板自身高度吃满
          condition = "input.show_floating_card == 'TRUE' && input.comparison_countries.length > 0",
          card(
            id = "floating_card_box",
            full_screen = TRUE,
            style = "height: 100%;", 
            card_header("Cross-Country Comparison"),
            card_body(
              padding = 5, 
              style = "height: 100%; overflow: hidden; min-height: 0;", # 锁死高度，防止 plotly 无限长高
              plotlyOutput("bar_plot", height = "100%")
            )
          )
        )
      )
    )
  ),
  
  # -------------------- Tab 2: Table 数据矩阵大表 --------------------
  nav_panel(
    title = "Table (by age)",
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
  # -------------------- Tab 3: Table 2 跨年龄组指标矩阵 --------------------
  nav_panel(
    title = "Table (by metric)",
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
        downloadButton("download_table2_csv", "Download", class = "btn-primary", style = "width: 100%;")
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
  ),
  # ==========================================
  # UI Panel: Burden Composition (Accordion 升级版)
  # ==========================================
  nav_panel(
    title = "Burden Composition",
    value = "composition_tab",
    layout_sidebar(
      sidebar = sidebar(
        width = '15%', open = TRUE, bg = "#fdfdfd",
        
        # 💡 使用 bslib 的 accordion 组件对侧边栏进行高级学术归类
        accordion(
          id = "comp_sidebar_accordion",
          multiple = TRUE, # 支持多个面板同时展开
          
          # ────────── 面板 1：疾病特征核心过滤器 ──────────
          accordion_panel(
            title = "1. Core Dimensions",
            icon = icon("filter"),
            checkboxGroupInput("comp_metrics", "Disease Indicators:", 
                               choices = c("RSV-associated ALRI incidence\n(Incidence)"="RSV-associated ALRI incidence", 
                                           "RSV-associated ALRI hospital admission\n(Admission)"="RSV-associated ALRI hospital admission", 
                                           "RSV-attributable mortality\n(Att.Mortality)"="RSV-attributable mortality", 
                                           "RSV-associated mortality\n(Ass.Mortality)"="RSV-associated mortality"),
                               selected = c("RSV-associated ALRI hospital admission", "RSV-associated mortality")),
            
            checkboxGroupInput("comp_ages", "Age Stratification:", 
                               choices = c("0-6 months" = "0-<6m", 
                                           "6-12 months" = "6-<12m", 
                                           "0-12 months" = "0-<12m", 
                                           "12-60 months" = "12-<60m", 
                                           "0-60 months" = "0-<60m"),
                               selected = c("0-<6m", "6-<12m", "12-60m")),
            hr(),
            checkboxInput("comp_is_percent", "Enable Percentage Stacked Bar", value = TRUE)
          ),
          
          # ────────── 面板 2：国家与集群快捷控制 ──────────
          accordion_panel(
            title = "2. Quick Select Presets",
            icon = icon("globe"),
            
            # 快捷宏单选下拉框 (改变时会重写下方的多选框)
            selectInput("comp_preset", "Quick Select Preset:",
                        choices = c(
                          "Manual Fine-tuning" = "none",
                          "Select All Countries" = "all",
                          "Select All Low-Income (L)" = "L",
                          "Select All Lower-Middle Income (LM)" = "LM",
                          "Select All Upper-Middle Income (UM)" = "UM",
                          "WHO Region: AFRO" = "Afr",
                          "WHO Region: AMRO" = "Amr",
                          "WHO Region: EMRO" = "Emr",
                          "WHO Region: EURO" = "Eur",
                          "WHO Region: SEARO" = "Sear",
                          "WHO Region: WPRO" = "Wpr"
                        ), selected = "none"),
            
            # 快捷滑块：拉动它也会动态改变下方的选中项目
            sliderInput("comp_top_n_slider", "Quick Top N Countries:", 
                        min = 1, max = 135, value = 10, step = 1),
            
            actionButton("btn_clear_comp_countries", "Clear All Selection", 
                         class = "btn-outline-danger btn-sm", style = "width: 100%; margin-bottom: 10px;")
          ),
          
          # ────────── 面板 3：显式国家名单与微调 ──────────
          accordion_panel(
            title = "3. Selected Countries List",
            icon = icon("list-check"),
            
            # 主国家多选框（所有快捷操作的核心终点站，带一键移除插件）
            selectizeInput("comp_countries", "Selected Countries / Ensembles:", 
                           choices = country_choices, selected = country_choices[1:10], 
                           multiple = TRUE, options = list(plugins = list('remove_button')))
          )
        )
      ),
      card(
        min_height = "calc(100vh - 140px)",
        card_header("RSV Burden Composition (Number of Cases)", class = "bg-light font-weight-bold"),
        card_body(
          plotlyOutput("composition_plotly", height = "100%")
        )
      )
    )
  )
)