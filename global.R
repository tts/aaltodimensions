library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)

data <- as.data.frame(readRDS("shiny_data.RDS"), stringsAsFactors = FALSE)
f_means <- readRDS("f_means.RDS")

metrics <- sort(c("times_cited", "times_cited_wos", "recent_citations", "relative_citation_ratio", "field_citation_ratio", "oa", "fieldcount", "authors"))

schools <- c("Aalto University", sort(unique(data$parent)))

#----------------------
# Plotly heatmap conf
#----------------------

f2 <- list(
  size = 10
)

ax <- list(
  title = "",
  tickangle = 45,
  tickfont = f2
)

ay <- list(
  title = "",
  tickfont = f2
)

m <- list(
  l = 300,
  r = 10,
  b = 150,
  t = 50,
  pad = 6
)

w <- 700
h <- 600




