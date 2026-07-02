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

