rm(list=ls())
library(PRISMA2020)
library(tidyverse)
library(rio)
library(janitor)

# Literature search ----
df<-import('data/Literature_search.xlsx') %>%
  filter(!if_all(everything(), is.na)) %>%
  select(1:13) %>%
  mutate(reason2 = str_trim(str_remove(reason, "\\s*\\d+(?:\\+\\d+)?$")))

df %>% count(reason2,wt=n) %>%
  adorn_totals() %>%
  rio::export('data/Exclude_reason.xlsx')

df %>% count(reason2,wt=n) %>%
  filter(reason2!='No full-text available') %>%
  mutate(str=paste(reason2,n,sep=',',collapse = '; ')) %>%
  pull(str)


example(PRISMA_flowdiagram)

csvFile <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
csvData <- read.csv(csvFile)
data <- PRISMA_data(csvData)

PRISMA_flowdiagram(data,fontsize = 12,
                   interactive = F,previous = FALSE,other = TRUE)

rio::export(csvData,'data/PRISMA_copy.xlsx')

plot<-import('data/PRISMA.xlsx') %>%
  PRISMA_data() %>% 
  PRISMA_flowdiagram(fontsize = 12,previous = T,other = T,
                     detail_databases=T)
plot

plot[["x"]][["diagram"]] <- gsub(
  '\\nReports of total included studies\\n\\(n = NA\\)',
  '',
  plot[["x"]][["diagram"]]
)

plot

PRISMA_save(plot,filename = 'plot/PRISMA2020_flowdiagram.png',overwrite = T)
PRISMA_save(plot,filename = 'plot/PRISMA2020_flowdiagram.svg',overwrite = T)
PRISMA_save(plot,filename = 'plot/PRISMA2020_flowdiagram.pdf',overwrite = T)

?PRISMA_flowdiagram
?PRISMA_data
?PRISMA_save

