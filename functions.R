export_flextable_word <- function(
    df,
    file_path,
    header_labels = NULL,
    caption = NULL,
    font_family = "Calibri",
    font_size = 8,
    orientation = c("portrait", "landscape"),
    big_number_format = TRUE,
    theme_style = "blue"
) {
  library(flextable)
  library(officer)
  library(dplyr)
  
  orientation <- match.arg(orientation)
  
  # ==== 1. 参数检查 ====
  if(!is.data.frame(df)) stop("df must be a data.frame")
  if(!is.null(header_labels)) {
    colnames(df) <- header_labels
  }
  
  # ==== 2. 处理大数字加逗号 ====
  if(big_number_format) {
    format_with_comma <- function(x) {
      if(!is.character(x)) return(x)
      gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    }
    df <- df %>% mutate(across(where(is.character), format_with_comma))
  }
  
  # ==== 3. 创建 flextable ====
  ft <- flextable(df)
  ft <- font(ft, fontname = font_family, part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  
  # 对齐处理
  ft <- align(ft, align = "center", part = "all")
  ft <- align(ft, j = 1:2, align = "left", part = "all") # 前两列左对齐
  
  # --- 为特定收入级别文本加粗 ---
  income_levels <- c('Lower-income', 'Lower-middle-income', 'Upper-middle-income')
  for(level in income_levels) {
    # 在数据框的所有列中查找匹配的单元格并加粗
    for(col in seq_len(ncol(df))) {
      # 找到当前列中匹配的行
      matches <- which(df[[col]] == level)
      if(length(matches) > 0) {
        ft <- bold(ft, i = matches, j = col, bold = TRUE, part = "body")
      }
    }
  }
  # 找出第一列中不在收入级别列表的行
  non_income_rows <- which(!df[[1]] %in% income_levels)
  
  if(length(non_income_rows) > 0) {
    # 为这些行添加左侧缩进（2个字符）
    ft <- padding(ft, 
                  i = non_income_rows,  # 这些行
                  j = 1,                 # 第一列
                  padding.left = 10,     # 左侧缩进20磅（约2个字符）
                  part = "body")
  }
  
  # --- 核心样式修改：只保留三条关键线 ---
  ft <- border_remove(ft) # 首先移除所有默认线条
  
  # 定义边框样式
  std_border <- fp_border(color = "black", width = 1)
  
  # 1. 整页开头线 (表格最顶端)
  ft <- hline_top(ft, part = "header", border = std_border)
  
  # 2. 标题下框线 (表头与正文交界处)
  ft <- hline_bottom(ft, part = "header", border = std_border)
  
  # 3. 整页结尾线 (表格最底端)
  ft <- hline_bottom(ft, part = "body", border = std_border)
  
  # 设置背景颜色
  if(theme_style == "blue") {
    # 蓝色表头
    ft <- color(ft, color = "white", part = "header")
    ft <- bg(ft, bg = "#0073C2", part = "header")
    ft <- bold(ft, part = "header")
    
    # 隔行浅蓝色背景 (跳过表头，从正文开始)
    ft <- bg(ft, i = seq(2, nrow(df), by = 2), bg = "#E6F2FA", part = "body")
  }
  
  ft <- autofit(ft)
  # ft <- set_table_properties(
  #   ft,
  #   layout = "autofit",
  #   width = 1
  # )
  # 调整第一列宽度变窄，同时保持整体撑满
  current_widths <- ft$body$colwidths  # 获取当前列宽
  total_width <- sum(current_widths)    # 计算总宽度
  
  # 将第一列宽度缩小为原来的一半，其他列按比例增加
  first_col_width <- current_widths[1] * 0.1  # 第一列缩小为一半
  remaining_width <- total_width - first_col_width
  
  # 计算其他列的新宽度（按比例分配剩余宽度）
  if(ncol(df) > 1) {
    other_cols_widths <- current_widths[-1] / sum(current_widths[-1]) * remaining_width
    # 合并新的列宽
    new_widths <- c(first_col_width, other_cols_widths)
  } else {
    # 如果只有一列
    new_widths <- first_col_width
  }
  
  # 应用新的列宽
  ft <- width(ft, width = new_widths)
  
  ft <- set_table_properties(
    ft,
    layout = "autofit",
    width = 1
  )
  
  # ==== 4. 写入 Word ====
  doc <- read_docx()
  if(orientation == "landscape") {
    ps <- prop_section(page_size = page_size(orient = "landscape"), type = "continuous")
    doc <- body_add_flextable(doc, ft) %>%
      body_end_block_section(value = block_section(property = ps))
  } else {
    doc <- body_add_flextable(doc, ft)
  }
  
  print(doc, target = file_path)
  cat("表格已成功导出，仅保留三处框线。",file_path,'\n')
}


export_table_to_doc <- function(
    df,
    file_path,
    header_labels = NULL,
    font_family = "Calibri",
    font_size = 10,
    orientation = c("portrait", "landscape"),
    big_number_format = TRUE
) {
  library(flextable)
  library(officer)
  library(dplyr)
  
  orientation <- match.arg(orientation)
  
  # ==== 1. 参数检查与列名设定 ====
  if (!is.data.frame(df)) stop("df 必须是 data.frame 或 tibble")
  if (!is.null(header_labels)) colnames(df) <- header_labels
  
  # ==== 2. 处理大数字加逗号 ====
  if (big_number_format) {
    format_with_comma <- function(x) {
      if (!is.character(x)) return(x)
      gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    }
    df <- df %>% mutate(across(where(is.character), format_with_comma))
  }
  
  # ==== 3. 创建 flextable 并重构双层表头 ====
  ft <- flextable(df)
  
  real_cols <- colnames(df)
  if (length(real_cols) < 6) stop("表格至少需要 6 列才能匹配当前的复合表头样式！")
  
  # 【关键点 1】：line1 的 1-2 列留空或写上层分类，line2 写具体列名
  
  if('N_inc' %in% real_cols) {
    header_structure <- data.frame(
      col_keys = real_cols[1:6],
      line1 = c("Country", "Age", "Incidence in the community", "Incidence in the community", "Hospital admission", "Hospital admission"),
      line2 = c( "", "","Incidence rate\n(per 1,000)", "Number of episodes", "Incidence rate\n(per 1,000)", "Number of episodes"),
      stringsAsFactors = FALSE
    )
  } else {
    header_structure <- data.frame(
      col_keys = real_cols[1:6],
      line1 = c("Country", "Age", "RSV-attributable mortality", "RSV-attributable mortality", "RSV-associated mortality", "RSV-associated mortality"),
      line2 = c( "", "","Mortality rate\n(per 100,000)", "Number of deaths", "Mortality rate\n(per 100,000)", "Number of deaths"),
      stringsAsFactors = FALSE
    )
  }

  
  # 映射双层表头
  ft <- set_header_df(ft, mapping = header_structure, key = "col_keys")
  
  # 只在第 1 行横向合并右侧的 3-6 列（不要用 merge_v 合并 1-2 列！）
  ft <- merge_h(ft, part = "header")
  
  # ==== 4. 表头背景色与文字颜色设置 ====
  # 第 1 行：整行填深蓝背景，白字
  ft <- bg(ft, i = 1, bg = "#1B365D", part = "header")
  ft <- color(ft, i = 1, color = "white", part = "header")
  
  # 第 2 行：整行（包含 Country 和 Age）填浅蓝背景，黑字
  ft <- bg(ft, i = 2, bg = "#D9E1F2", part = "header")
  ft <- color(ft, i = 2, color = "black", part = "header")
  
  ft <- bold(ft, part = "header")
  
  # ==== 5. 设置字体、行距与对齐方式（全部左对齐） ====
  ft <- flextable::font(ft, fontname = font_family, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  
  ft <- line_spacing(ft, space = 1.0, part = "all") 
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 3, padding.right = 3, part = "all")
  
  ft <- align(ft, align = "left", part = "all")
  ft <- valign(ft, val = "top", part = "all")
  ft <- valign(ft, val = "center",part = "header")
  
  # ==== 6. Income 级别行处理 ====
  income_levels <- c('Lower-income', 'Lower-middle-income', 'Upper-middle-income', 'Low-income', 'High-income')
  income_rows <- which(df[[1]] %in% income_levels)
  
  if (length(income_rows) > 0) {
    for (r in income_rows) {
      ft <- merge_at(ft, i = r, j = 1:ncol(df), part = "body")
    }
    ft <- bold(ft, i = income_rows, bold = TRUE, part = "body")
    ft <- bg(ft, i = income_rows, bg = "#FFFFCC", part = "body")
  }
  
  # ==== 7. Age 行处理 ====
  age_col_idx <- which(tolower(colnames(df)) == "age")
  if (length(age_col_idx) > 0) {
    age_target_rows_1 <- which(df[[age_col_idx[1]]] %in% c("0-<12m", "0-<6m"))
    age_target_rows_2 <- which(df[[age_col_idx[1]]] %in% c("6-<12m"))
    
    if (length(age_target_rows_1) > 0) {
      ft <- bg(ft, i = age_target_rows_1, bg = "#E6F2FA", part = "body")
    }
    if (length(age_target_rows_2) > 0) {
      ft <- bg(ft, i = age_target_rows_2, bg = "#F2F2F2", part = "body")
    }
  }
  
  # ==== 8. 三线表框线设置 ====
  ft <- border_remove(ft)
  std_border <- fp_border(color = "black", width = 1)
  ft <- hline_top(ft, part = "header", border = std_border)
  ft <- hline_bottom(ft, part = "header", border = std_border)
  ft <- hline_bottom(ft, part = "body", border = std_border)
  
  # ==== 9. 关键：在 autofit 模式下精准指定前两列宽度 ====
  ft <- autofit(ft)                             # 先自适应，避免全盘布局错乱
  ft <- set_table_properties(ft, layout = "autofit", width = 1) # 依然保留 autofit 模式
  ft <- width(ft, j = 1:2, width = cm(2.5))     # 强行把 Country 和 Age 覆盖为 2.5cm
  
  # ==== 10. 写入 Word 文件 ====
  doc <- read_docx()
  if (orientation == "landscape") {
    ps <- prop_section(page_size = page_size(orient = "landscape"), type = "continuous")
    doc <- body_add_flextable(doc, ft) %>%
      body_end_block_section(value = block_section(property = ps))
  } else {
    doc <- body_add_flextable(doc, ft)
  }
  
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }
  
  print(doc, target = file_path)
  cat("表格已成功导出至：", file_path, "\n")
}

export_rsv_table_doc <- function(
    df,
    file_path,
    font_family = "Calibri",
    font_size = 10,
    orientation = c("landscape", "portrait")
) {
  library(flextable)
  library(officer)
  library(dplyr)
  
  orientation <- match.arg(orientation)
  
  if (!is.data.frame(df)) stop("df 必须是 data.frame 或 tibble")
  
  # ==== 1. 重构表头名称 (10 列) ====
  new_headers <- c(
    "Age", 
    "Country", 
    "Income\nlevel", 
    "References", 
    "Location", 
    "Study\nPeriod",
    "Type",
    "Estimates from\nindividual studies", 
    "Estimates used for\nvalidation, main\nanalysis†", 
    "Estimates used for\nvalidation,\nsensitivity analysis†"
  )
  
  new_headers_mort <- c(
    "Age", 
    "Country", 
    "Income\nlevel", 
    "References", 
    "Location", 
    "Study\nPeriod",
    "Estimates from\nindividual studies"
  )
  
  if (ncol(df) == 10) {
    colnames(df) <- new_headers
  } else {
    colnames(df) <- new_headers_mort
  }
  
  # ==== 2. 识别大标题行 (黄色高亮行) ====
  category_rows <- which(grepl("incidence rate|RSV-associated|mortality", df[[1]], ignore.case = TRUE))
  
  # ==== 3. 借助 which() 直接算组行号，不改变原始数据 ====
  # 找出所有非大标题行，且 Country 或 Age 不为空的“新组起始行”
  data_rows <- setdiff(1:nrow(df), category_rows)
  
  # 在数据行中，找到 Country 或 Age 有值的行号（即切分点）
  start_indices <- which(
    (df$Country != "" | df$Age != "") & 
      !(1:nrow(df) %in% category_rows)
  )
  
  # 计算每组的起始与结束位置
  end_indices <- c(start_indices[-1] - 1, max(data_rows))
  
  # 处理跨越 category_rows 的边界情况
  even_group_rows <- c()
  
  for (g in seq_along(start_indices)) {
    group_start <- start_indices[g]
    group_end   <- end_indices[g]
    
    # 提取当前组包含的有效数据行（避开大标题行）
    current_group_rows <- setdiff(group_start:group_end, category_rows)
    
    # 偶数组收集所有属于它的行号
    if (g %% 2 == 0) {
      even_group_rows <- c(even_group_rows, current_group_rows)
    }
  }
  
  # ==== 4. 创建 flextable 对象 ====
  ft <- flextable(df)
  
  # ==== 5. 设置全表字体、行距与左对齐 ====
  ft <- flextable::font(ft, fontname = font_family, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- line_spacing(ft, space = 1.05, part = "all") 
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 3, padding.right = 3, part = "all")
  
  ft <- align(ft, align = "left", part = "all")
  ft <- valign(ft, val = "top", part = "all")
  
  # ==== 6. 表头样式 ====
  ft <- bold(ft, part = "header")
  ft <- bg(ft, bg = "transparent", part = "header")
  
  # ==== 7. 染色：直接给偶数组的行集合涂上淡蓝色 ====
  if (length(even_group_rows) > 0) {
    ft <- bg(ft, i = even_group_rows, bg = "#EBF3F9", part = "body")
  }
  
  # 黄色大标题行覆盖
  if (length(category_rows) > 0) {
    for (r in category_rows) {
      ft <- merge_at(ft, i = r, j = 1:ncol(df), part = "body")
    }
    ft <- bold(ft, i = category_rows, bold = TRUE, part = "body")
    ft <- bg(ft, i = category_rows, bg = "#FFF2CC", part = "body")
  }
  
  # ==== 8. 纵向合并前三列 ====
  # ft <- merge_v(ft, j = c(1, 2, 3), part = "body")
  
  # ==== 9. 三线表框线设置 ====
  ft <- border_remove(ft)
  header_border <- fp_border(color = "#418AB3", width = 1.5)
  bottom_border <- fp_border(color = "black", width = 1)
  
  ft <- hline_top(ft, part = "header", border = header_border)
  ft <- hline_bottom(ft, part = "header", border = header_border)
  ft <- hline_bottom(ft, part = "body", border = bottom_border)
  
  # ==== 10. autofit + 锁定前三列宽度 ====
  ft <- autofit(ft)
  ft <- width(ft, j = 1, width = cm(1.8)) # Age: 1.8 cm
  ft <- width(ft, j = 2, width = cm(2.5)) # Country: 2.5 cm
  ft <- width(ft, j = 3, width = cm(1.8)) # Income level: 1.8 cm
  
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  
  # ==== 11. 导出至 Word 文档 ====
  doc <- read_docx()
  
  if (orientation == "landscape") {
    ps <- prop_section(page_size = page_size(orient = "landscape"), type = "continuous")
    doc <- body_add_flextable(doc, ft) %>%
      body_end_block_section(value = block_section(property = ps))
  } else {
    doc <- body_add_flextable(doc, ft)
  }
  
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }
  
  print(doc, target = file_path)
  cat("表格已成功导出至：", file_path, "\n")
}
