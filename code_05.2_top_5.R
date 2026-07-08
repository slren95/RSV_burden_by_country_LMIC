rm(list=ls())
library(tidyverse)
library(rio)
library(psych)

dir('docs',pattern = '.rds',full.names = T) %>%
  walk(~{
    assign(str_remove_all(.x,'(docs/|.rds)'),import(.x,trust=T),envir = .GlobalEnv)
  })

df_inc<-Table_incidence %>%
  rename_with(~ "metric", .cols = 2) %>%
  filter(!is.na(`0-<12m`)) %>%
  mutate(Country=na_if(Country,'')) %>%
  fill(Country,.direction = 'down') %>% 
  mutate(V_0012=parse_number(str_remove(`0-<12m`,"\\s*\\(.*\\)")),
         V_0060=parse_number(str_remove(`0-<60m`,"\\s*\\(.*\\)")))

df_inc %>%
  split(.$metric) %>%
  walk(~{
    print(.x[1,'metric'])
    .x %>%
      slice_max(V_0012,n=5) %>%
      select(Country,V_0012,`0-<12m`) %>%
      print()
  })

df_hos<-Table_Hospitalisation_1000 %>%
  rename_with(~ "metric", .cols = 2) %>%
  filter(!is.na(`0-<12m`)) %>%
  mutate(Country=na_if(Country,'')) %>%
  fill(Country,.direction = 'down') %>% 
  mutate(V_0012=parse_number(str_remove(`0-<12m`,"\\s*\\(.*\\)")),
         V_0060=parse_number(str_remove(`0-<60m`,"\\s*\\(.*\\)")))

df_mort.att<-Table_mortality_all_DeCoDe %>%
  rename_with(~ "metric", .cols = 2) %>%
  filter(!is.na(`0-<12m`)) %>%
  mutate(Country=na_if(Country,'')) %>%
  fill(Country,.direction = 'down') %>% 
  mutate(V_0012=parse_number(str_remove(`0-<12m`,"\\s*\\(.*\\)")),
         V_0060=parse_number(str_remove(`0-<60m`,"\\s*\\(.*\\)")))

df_mort.ass<-Table_mortality_all_NP %>%
  rename_with(~ "metric", .cols = 2) %>%
  filter(!is.na(`0-<12m`)) %>%
  mutate(Country=na_if(Country,'')) %>%
  fill(Country,.direction = 'down') %>% 
  mutate(V_0012=parse_number(str_remove(`0-<12m`,"\\s*\\(.*\\)")),
         V_0060=parse_number(str_remove(`0-<60m`,"\\s*\\(.*\\)")))

## byincome ----
Table_incidence.byincome %>%
  select(`0-<12m`,`0-<60m`) %>%
  separate(`0-<12m`,into=c('N_0012','R_0012'),sep='\n') %>%
  separate(`0-<60m`,into=c('N_0060','R_0060'),sep='\n')

Table_Hospitalisation.byincome %>%
  select(`0-<12m`,`0-<60m`) %>%
  separate(`0-<12m`,into=c('N_0012','R_0012'),sep='\n') %>%
  separate(`0-<60m`,into=c('N_0060','R_0060'),sep='\n')

Table_mortality_all_DeCoDe.byincome %>%
  select(`0-<12m`,`0-<60m`) %>%
  separate(`0-<12m`,into=c('N_0012','R_0012'),sep='\n') %>%
  separate(`0-<60m`,into=c('N_0060','R_0060'),sep='\n')

Table_mortality_all_NP.byincome %>%
  select(`0-<12m`,`0-<60m`) %>%
  separate(`0-<12m`,into=c('N_0012','R_0012'),sep='\n') %>%
  separate(`0-<60m`,into=c('N_0060','R_0060'),sep='\n')


# Batch output ----


export_top_inc <- function(df, file = "docs/TOP_inc.txt") {
  writeLines("", file)
  
  # 处理 V_0012
  sink(file, append = TRUE)
  cat("==================== Top 5 - V_0012 (0-<12m) ====================\n\n")
  df %>%
    split(.$metric) %>%
    walk(~{
      cat("Metric: ", .x[1,'metric'], "\n", sep = "")
      cat("  Range (min-max): ", 
          round(min(.x$V_0012, na.rm = TRUE), 1), " - ", 
          round(max(.x$V_0012, na.rm = TRUE), 1), "\n", sep = "")
      cat("  Top 5:\n")
      .x %>%
        slice_max(V_0012, n = 5) %>%
        select(Country, V_0012, `0-<12m`) %>%
        print()
      cat("\n")
    })
  sink()
  
  # 处理 V_0060
  sink(file, append = TRUE)
  cat("\n\n==================== Top 5 - V_0060 (0-<60m) ====================\n\n")
  df %>%
    split(.$metric) %>%
    walk(~{
      cat("Metric: ", .x[1,'metric'], "\n", sep = "")
      cat("  Range (min-max): ", 
          round(min(.x$V_0060, na.rm = TRUE), 1), " - ", 
          round(max(.x$V_0060, na.rm = TRUE), 1), "\n", sep = "")
      cat("  Top 5:\n")
      .x %>%
        slice_max(V_0060, n = 5) %>%
        select(Country, V_0060, `0-<60m`) %>%
        print()
      cat("\n")
    })
  sink()
  
  message("Results written to: ", file)
}
# 使用
export_top_inc(df_inc,'docs/TOP_inc.txt')
export_top_inc(df_hos,'docs/TOP_hos.txt')
export_top_inc(df_mort.att,'docs/TOP_mort.att.txt')
export_top_inc(df_mort.ass,'docs/TOP_mort.ass.txt')
