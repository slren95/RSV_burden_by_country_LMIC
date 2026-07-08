init_table1_server <- function(input, output, session) {
  
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
  
  output$matrix_title <- renderText({
    paste("RSV disease burden by country | ", input$table_age)
  })
  
  table_matrix_data <- reactive({
    req(input$table_age)
    df_raw <- all_metrics_broad_data() %>% 
      left_join(world_sf %>% as.data.frame() %>% select(iso_a3,iso_a2),by=c('ISOCountry'='iso_a3'))
    
    df_names <- df_master %>% distinct(ISOCountry, CountryName,Income2019,WHORegion) %>% filter(!is.na(CountryName))
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
      mutate(across(c(v1_N,v2_N,v3_N,v4_N), ~log(.x+1))) %>%
      relocate(Income2019,WHORegion,.after=Country) %>%
      mutate(WHORegion=toupper(WHORegion))
  })
  
  # Table 2
  # 针对特定指标提取所有年龄组的 Rate 和 Cases
  # 针对特定指标提取所有年龄组的 Rate 和 Cases
  # 针对特定指标提取所有年龄组的 Rate 和 Cases
  
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
      data_color(
        columns = Income2019,
        fn = scales::col_factor(
          palette = c("#feb24c", "#ffeda0", "#ffffcc"), # 经典蓝紫淡色系渐变
          levels = c("L", "LM", "UM"),                 # 显式指定学术等级顺序
          na.color = "#f4f6f7"
        )
      ) %>%
      
      # 3️⃣ 【核心新增】：针对 WHORegion 进行无序分类染色 (使用离散柔和色调)
      data_color(
        columns = WHORegion,
        fn = scales::col_factor(
          palette = "Pastel2",                         # ColorBrewer 的经典柔和无序调色板
          domain = NULL,                               # 动态识别出现的区域 Level
          na.color = "#f4f6f7"
        )
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
        Income2019="Income level(2019)",
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

