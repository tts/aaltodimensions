library(shiny)
library(shinydashboard)
library(ggvis)
library(DT)

data <- readRDS("data_units.RDS")

data$times_cited <- as.integer(data$times_cited)
data$recent_citations <- as.integer(data$recent_citations)
data$relative_citation_ratio <- as.numeric(data$relative_citation_ratio)
data$field_citation_ratio <- as.numeric(data$field_citation_ratio)

metrics <- sort(c("times_cited", "recent_citations", "relative_citation_ratio", "field_citation_ratio", "oa", "fieldcount", "authors"))

schools <- sort(unique(data$parent))

scales <- c("linear", "log")




