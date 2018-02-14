library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)

data <- as.data.frame(readRDS("shiny_data.RDS"), stringsAsFactors = FALSE)
f_means <- readRDS("f_means.RDS")

metrics <- sort(c("times_cited", "wos", "recent_citations", "relative_citation_ratio", "field_citation_ratio", 
                  "oa", "fieldcount", "authors", "mendeley", "tweets", "altm_score"))

scales <- c("linear", "log")

schools <- c("Aalto University", sort(unique(data$parent)))

uniquedois <- data[!duplicated(data[,1]), ]
nrow_uniquedois <- nrow(uniquedois)
with_oa_uniquedois <- paste0(nrow(uniquedois[!is.na(uniquedois$urls),]),
                                " (",
                                floor(nrow(uniquedois[!is.na(uniquedois$urls),]) / nrow_uniquedois * 100),
                                "%)")


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




