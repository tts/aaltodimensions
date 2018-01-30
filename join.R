library(readxl)
library(tidyverse)

# From CRIS
units <- read_excel("Organisaatiolistaus-30_01_2018.xls")
units <- units %>% 
  filter(!is.na(`Ids-1`)) %>%
  select(-`Visibility-0`)

units$`Ids-1` <- gsub("Organisation ID: ","",units$`Ids-1`)
units <- units[grep("^[AETU].*", units$`Ids-1`),]
names(units) <- c("id", "name", "parent")

# Join with Dimensions API data
data <- readRDS("dim_data.RDS")
data_units <- left_join(data, units, by = c("units"="id"))

data_units <- data_units %>% 
  filter(!parent %in% c("University level", "University Joint Units"),
         !is.na(name),
         year %in% c("2015", "2016", "2017")) %>% 
  mutate(relative_citation_ratio = ifelse(relative_citation_ratio == "", 0, relative_citation_ratio),
         field_citation_ratio = ifelse(field_citation_ratio == "", 0, field_citation_ratio),
         color = ifelse(parent == "School of Arts, Design and Architecture", "#E3A220",
                        ifelse(parent == "School of Business", "#8BC12D",
                               ifelse(parent == "School of Chemical Engineering", "#4AA871",
                                      ifelse(parent == "School of Electrical Engineering", "#6132A6",
                                             ifelse(parent == "School of Engineering", "#9D2FA0",
                                                    "#D26717")))))) %>% 
  filter(!is.na(name)) 

saveRDS(data_units, "data_units.RDS")
