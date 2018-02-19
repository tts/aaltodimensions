#--------------------------
# Transform data returned
# by Unpaywall to JSON
# for a D3 collapsible tree
#--------------------------
library(jsonlite)
library(tidyverse)

oa <- readRDS("unpaywall.RDS")
oa <- oa[!duplicated(oa[,c('doi')]),]
oa <- oa[!is.na(oa$free_fulltext_url), c("title", "evidence", "free_fulltext_url")]
names(oa) <- c("title", "evidence", "url")

oa <- oa %>%
  mutate(name = paste0(substr(title,1,100), " ", substr(url,1,50))) %>%
  arrange(name) %>%
  select(-title)

nest_data <- oa %>%
  group_by(evidence) %>%
  nest(.key = children) %>%
  rename(name = evidence) %>%
  mutate(children = map(children, ~.x %>%
                          group_by(name, url)))

nest_data_2 <- data.frame(name="Free fulltext", children=I(list(nest_data)))
json <- toJSON(nest_data_2, pretty = T)

json <- gsub("^\\[", "", json)
json <- gsub("\\]$","", json)

sink("oa.json")
cat(json)
sink()
