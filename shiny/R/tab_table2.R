init_table2_server <- function(input, output, session) {
  
  # 1. 搬入数据提取逻辑
  table2_matrix_data <- reactive({
    req(input$table2_metric)
    target_metric <- input$table2_metric
    
    # 1. 从 df_master 动态提取基础点估计
    df_sub <- df_master %>% 
      filter(metric == target_metric) %>%
      select(ISOCountry, CountryName, ,Income2019,WHORegion,AGEGR, R, N)
    
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
        id_cols = c(ISOCountry, CountryName,WHORegion,Income2019),
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
      select(Country, ISOCountry, everything(), -CountryName, -iso_a2) %>%
      relocate(Income2019,WHORegion,.after=Country) %>%
      mutate(WHORegion=toupper(WHORegion))
    
    # 对所有 N_ 开头的点估计列（如 N_0-<6m 等）做 log 转换以适配背景着色
    col_v_n <- names(df_wide)[grepl("^N_[0-9]", names(df_wide))]
    df_wide <- df_wide %>% mutate(across(all_of(col_v_n), ~log(.x + 1)))
    
    df_wide
  })
  
  # Table 2 动态标题
  output$matrix2_title <- renderText({
    paste("RSV disease burden by country | ", input$table2_metric)
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
      ) %>%
      data_color(
        columns = Income2019,
        fn = scales::col_factor(
          palette = c("#feb24c", "#ffeda0", "#ffffcc"),# 经典蓝紫淡色系渐变
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
      tab_spanner(label = "Rate", columns = all_of(cols_R_val)) %>%
      tab_spanner(label = "Number of Cases", columns = all_of(cols_N_val)) %>%
      
      # 5️⃣ 命名小表头（展示年龄段）
      cols_label(
        Country = "Country",
        Income2019="Income level(2019)",
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
  
  # Table 2 专属数据下载处理器
  output$download_table2_csv <- downloadHandler(
    filename = function() { 
      # 将指标名称中的空格和特殊字符替换为下划线，作为安全的文件名
      safe_metric <- gsub("[^A-Za-z0-9]+", "_", input$table2_metric)
      sprintf("RSV_burder_by_country_%s.csv", safe_metric)
    },
    content = function(file) {
      # 1. 拿到当前的响应式宽表数据
      raw_df <- table2_matrix_data() 
      
      # 2. 移除带有 HTML <span> 和 <img> 国旗标签的 Country 列（避免导出的 CSV 里面有乱码）
      #    并重新补一个干净的 CountryName 或者是把 ISOCountry 排到最前面
      if ("ISOCountry" %in% names(raw_df)) {
        export_df <- raw_df %>% 
          select(-Country) %>%          # 删掉带 HTML 国旗标签的列
          relocate(ISOCountry)          # 把 ISO 代码放到第一列
      } else {
        export_df <- raw_df
      }
      
      # 3. 写入 CSV
      write_csv(export_df, file)
    }
  )
}