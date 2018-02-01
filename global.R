library(shiny)
library(shinydashboard)
library(ggvis)
library(DT)
library(tidyverse)

data <- as.data.frame(readRDS("shiny_data.RDS"), stringsAsFactors = FALSE)

metrics <- sort(c("times_cited", "recent_citations", "relative_citation_ratio", "field_citation_ratio", "oa", "fieldcount", "authors"))

schools <- sort(unique(data$parent))

scales <- c("linear", "log")




