rm(list=ls())
library(officer)
library(tidyverse)
library(flextable)

# Tables ----
source_files <- dir('docs/', pattern='Table_.*\\.rds', full.names = TRUE)[c(2,1,3,4)]

table_titles <- c(
  "Table 1. RSV-associated ALRI incidence by country",
  "Table 2. RSV-associated ALRI hospital admission by country",
  "Table 3. RSV-attributable all-cause deaths by country",
  "Table 4. RSV-associated all-cause deaths by country"
)

make_flextable_style <- function(
    df,
    font_family = "Calibri",
    font_size = 8,
    theme_style = "blue",
    big_number_format = TRUE
) {
  
  library(flextable)
  library(dplyr)
  
  # ==== 1. format numbers ====
  if(big_number_format) {
    format_with_comma <- function(x) {
      if(!is.character(x)) return(x)
      gsub("(\\d)(?=(\\d{3})+(?!\\d))", "\\1,", x, perl = TRUE)
    }
    df <- df %>% mutate(across(where(is.character), format_with_comma))
  }
  
  # ==== 2. create flextable ====
  ft <- flextable(df)
  ft <- font(ft, fontname = font_family, part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  
  # align
  ft <- align(ft, align = "center", part = "all")
  ft <- align(ft, j = 1:2, align = "left", part = "all")
  
  # ==== 3. highlight income levels ====
  income_levels <- c('Lower-income', 'Lower-middle-income', 'Upper-middle-income')
  
  for(level in income_levels) {
    for(col in seq_len(ncol(df))) {
      matches <- which(df[[col]] == level)
      if(length(matches) > 0) {
        ft <- bold(ft, i = matches, j = col, part = "body")
      }
    }
  }
  
  # indent
  non_income_rows <- which(!df[[1]] %in% income_levels)
  if(length(non_income_rows) > 0) {
    ft <- padding(ft,
                  i = non_income_rows,
                  j = 1,
                  padding.left = 10,
                  part = "body")
  }
  
  # ==== 4. borders ====
  ft <- border_remove(ft)
  std_border <- fp_border(color = "black", width = 1)
  
  ft <- hline_top(ft, part = "header", border = std_border)
  ft <- hline_bottom(ft, part = "header", border = std_border)
  ft <- hline_bottom(ft, part = "body", border = std_border)
  
  # ==== 5. theme ====
  if(theme_style == "blue") {
    ft <- color(ft, color = "white", part = "header")
    ft <- bg(ft, bg = "#0073C2", part = "header")
    ft <- bold(ft, part = "header")
    
    ft <- bg(ft, i = seq(2, nrow(df), by = 2),
             bg = "#E6F2FA", part = "body")
  }
  
  # ==== 6. size ====
  ft <- autofit(ft)      
  
  current_widths <- ft$body$colwidths
  total_width <- sum(current_widths)
  
  first_col_width <- current_widths[1] * 0.1
  remaining_width <- total_width - first_col_width
  
  if(ncol(df) > 1) {
    other_cols <- current_widths[-1] / sum(current_widths[-1]) * remaining_width
    new_widths <- c(first_col_width, other_cols)
  } else {
    new_widths <- first_col_width
  }
  
  ft <- width(ft, width = new_widths)
  
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  
  return(ft)
}


export_four_tables_word <- function(
    source_files,
    table_titles,
    output_file = "tables.docx",
    landscape = TRUE
) {
  doc <- read_docx('../paper/template.docx')
  
  for(i in seq_along(source_files)) {
    
    df <- readRDS(source_files[i])
    ft <-make_flextable_style(df)   # 你的样式函数
    
    ps <- prop_section(
      page_size = page_size(orient = "landscape"),
      type = "continuous"
    )
    
    doc <- doc %>%
      body_add_par(table_titles[i], style = "heading 3") %>%
      body_add_flextable(ft) %>%
      body_end_block_section(block_section(property = ps)) %>%
      body_add_break()
  }
  message(output_file)
  print(doc, target = output_file)
}


export_four_tables_word(source_files,table_titles,output_file = "docs/Tables_1_4.docx")

# Figures map -----
age_map <- c(
  "0-6m" = "0–<6 months",
  "6-12m" = "6–<12 months",
  "0-12m" = "0–<12 months",
  "12-60m" = "12–<60 months",
  "0-60m" = "0–<60 months"
)
N_map <- c(
  inc = "RSV-associated ALRI incidence cases",
  hos = "RSV-associated ALRI hospital admissions",
  mort.ass = "RSV-associated all-cause deaths",
  mort.att = "RSV-attributable all-cause deaths"
)

R_map <- c(
  inc = "RSV-associated ALRI incidence rate",
  hos = "RSV-associated ALRI hospital admission rate",
  mort.ass = "RSV-associated all-cause mortality rate",
  mort.att = "RSV-attributable all-cause mortality rate"
)


tiff_NR<-data.frame(
  path=dir('plot/', pattern='map_(N|R).*\\.tiff', full.names = TRUE)
) %>%
  mutate(name = basename(path) %>%
           gsub("\\.tiff$", "", .) %>%
           gsub("mort_ass","mort.ass",.) %>%
           gsub("mort_att","mort.att",.) %>%
           substr(.,5,str_count(.))
         ) %>%
  separate(name,into=c('type','metric','age'),sep='_',remove = F) %>%
  mutate(age=age_map[age]) %>%
  mutate(caption=sprintf('Global distribution of %s in %s aged %s in LMICs',
                         if_else(type=='N',N_map[metric],R_map[metric]),
                         if_else(age %in% age_map[1:3],'infants','children'),
                         age),
         note=paste0("RSV = respiratory syncytial virus; ALRI = acute lower respiratory infection; LMICs = low- and middle-income countries.\u00A0",
                     "Color scale is based on log10-transformed values. ",
                     if_else(type=='R','Triangle (▲) denotes the global mean.','')
         )) %>%
  mutate(metric=factor(metric,levels=c('inc','hos','mort.att','mort.ass')),
         age=factor(age,levels=age_map)) %>%
  arrange(metric,type,age)


export_images_word <- function(
    image_files,
    image_captions,
    image_notes,
    start_index = 1,
    output_file = "docs/Figures_NR_map.docx",
    width = 9.2,
    height = 5.2
) {
  doc <- read_docx('../paper/template.docx')
  
  for (i in seq_along(image_files)) {
    
    fig_id <- start_index + i - 1
    
    caption <- paste0('Figure ',fig_id,'. ',image_captions[i])
    note <- image_notes[i]
    
    ps <- prop_section(
      page_size = page_size(orient = "landscape"),
      type = "continuous"
    )

    doc <- doc %>%
      body_add_img(src = image_files[i], width = width, height = height) %>%
      body_add_par(caption, style = "heading 3") %>%
      body_add_fpar(fpar(ftext(note, prop = fp_text(font.family = "Calibri", font.size = 10.5)))) %>%
      body_end_block_section(block_section(property = ps))
  }
  message(output_file)
  print(doc, target = output_file)
}

export_images_word(tiff_NR$path,tiff_NR$caption,tiff_NR$note,start_index = 7)

# Figure1 ratio ----

tiff_ratio<-data.frame(
  path=dir('plot/', pattern='map_(mor)_2_.*\\.tiff', full.names = TRUE)
) %>%
  mutate(name = basename(path) %>%
           gsub("\\.tiff$", "", .) %>%
           gsub("_2_","2",.) %>%
           substr(.,5,str_count(.))
  ) %>%
  separate(name,into=c('metric','age'),sep='_',remove = F) %>%
  mutate(age=age_map[age]) %>%
  mutate(caption=sprintf(if_else(metric=='hos2inc','Ratio of RSV-associated ALRI hospital admission rate to incidence rate in %s aged %s in LMICs','Ratio of RSV-attributable all-cause mortality rate to RSV-associated ALRI incidence rate in %s aged %s in LMICs'),
                         if_else(age %in% age_map[1:3],'infants','children'),
                         age),
         note=paste0("RSV = respiratory syncytial virus; ALRI = acute lower respiratory infection; LMICs = low- and middle-income countries.")
         ) %>%
  mutate(age=factor(age,levels=age_map)) %>%
  arrange(metric,age)

export_images_word(tiff_ratio$path,tiff_ratio$caption,tiff_ratio$note,start_index = 47,output_file = "docs/Figures_ratio_map.docx")

# Combine report ----
doc <- read_docx('../paper/report_template.docx')

doc <- doc %>%
  cursor_bookmark("TABLES_1_4") %>%
  body_import_docx(src = "docs/Tables_1_4.docx") %>%
  cursor_bookmark("FIGURES_NP") %>%
  body_import_docx(src = "docs/Figures_NR_map.docx") %>%
  body_end_section_landscape()

print(doc, target ='docs/my_report1.docx')



