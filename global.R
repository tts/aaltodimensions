library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)

data <- as.data.frame(readRDS("shiny_data.RDS"), stringsAsFactors = FALSE)

data <- data %>% 
  mutate(text = paste0('<b>',parent,'</b>','<br>',name,'<br><b>',title,'</b><br>',journal_name,'<br>',publisher,'(',year,')','<br>Nr of fields: ',fieldcount,'<br>Nr of authors: ',authors,'<br>OA status in VIRTA: ',oa,'<br>OA full text?: ',is_oa))

metrics <- sort(c("times_cited", "times_cited_wos", "recent_citations", "relative_citation_ratio", "field_citation_ratio", "oa", "fieldcount", "authors"))

schools <- sort(unique(data$parent))




