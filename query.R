library(RCurl)
library(data.table)
library(jsonlite)
library(tidyverse)
library(httr)
library(readxl)
library(roadoi)

#---------------------------------------------------------------
# Helper functions to parse JSON result by the Dimensions API
# see https://stackoverflow.com/a/35497845
# Copied as is except stringsAsFactors=FALSE in the cbind function call
#----------------------------------------------------------------

col_fixer <- function(x, vec2col = FALSE) {
  if (!is.list(x[[1]])) {
    if (isTRUE(vec2col)) {
      as.data.table(data.table::transpose(x))
    } else {
      vapply(x, toString, character(1L))
    }
  } else {
    temp <- rbindlist(x, use.names = TRUE, fill = TRUE, idcol = TRUE)
    temp[, .time := sequence(.N), by = .id]
    value_vars <- setdiff(names(temp), c(".id", ".time"))
    dcast(temp, .id ~ .time, value.var = value_vars)[, .id := NULL]
  }
}

Flattener <- function(indf, vec2col = FALSE) {
  indf <- flatten(indf)
  listcolumns <- sapply(indf, is.list)
  newcols <- do.call(cbind, lapply(indf[listcolumns], col_fixer, vec2col))
  indf[listcolumns] <- list(NULL)
  cbind(indf, newcols, stringsAsFactors = FALSE)
}

#------------------------------
# Get Aalto entries from VIRTA
#------------------------------

virtaurl <- "https://virta-jtp.csc.fi/api/julkaisut/haku/xml?&"
org <- "organisaatioTunnus=10076"
query <- paste0(virtaurl, org)
con <- GET(query)
writeBin(content(con, "raw"), "aaltopubl_2018-01-25.zip")

#--------------------------------
# XSLT transformation XML->CSV
#--------------------------------

system(command = "java -jar ~/saxonee/saxon9ee.jar aaltopubl_2018-01-25.zip parse.xsl -o:aaltopubl_2018-01-25.csv")

#------------------------------------------
# Replace '|' in one title with a space 
# because '|' is used as a field separator
#------------------------------------------

system(command='sed -i "s/Microstructural evolution and mechanical properties of Au-20wt.%Sn|Ni interconnection/Microstructural evolution and mechanical properties of Au-20wt.%Sn Ni interconnection/g" aaltopubl_2018-01-25.csv')

data <- read.csv("aaltopubl_2018-01-25.csv", sep = "|", quote = "", header = FALSE, stringsAsFactors = FALSE)
names(data) <- c("doi", "year", "title", "journal", "authors", "oa", "units", "fieldcount", "fields")

#----------------------------------------------------------
# Quick and dirty cleaning. Does not fix all DOIs but most
#----------------------------------------------------------

data$doi <- gsub("https?://[^0-9]+([0-9].*)", "\\1", data$doi)
data$doi <- tolower(data$doi)
data$doi <- gsub("%28", "(", data$doi)
data$doi <- gsub("%29", ")", data$doi)
data$doi <- gsub("%2f", "/", data$doi)
data$doi <- gsub("^([^\\?]+)\\?.*", "\\1", data$doi)
data$doi <- gsub("^([^#]+)#.*", "\\1", data$doi)
data$doi <- gsub("^([^ ]+) .*", "\\1", data$doi)

#-----------------------
# Query Dimensions API
#-----------------------

results <- list()
baseurl <- "http://metrics-api.dimensions.ai/doi/"
rows <- nrow(data)

#----------------------------------------
# Handle bad DOIs in the query
# see https://stackoverflow.com/a/8094059
#----------------------------------------

for( i in 1:rows ) {
  possibleError <- tryCatch(
    res <- fromJSON(paste0(baseurl, data[i, "doi"])),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next

  message("Retrieving DOI nr ", i, " out of ", rows)
  results[[i]] <- res
}

saveRDS(results, "raw_dimensions_results.RDS")

#----------------------------------------------------------
# Bad DOIs add a NULL list element, so leaving them out now
#----------------------------------------------------------

results_notnull <- results[-which(sapply(results, is.null))]

#----------------------------------------------------------------
# First the list of lists to a data frame of lists.
# Then, flattening lists with helper functions above
#
# see https://stat.ethz.ch/pipermail/r-help/2006-August/111368.html
#-----------------------------------------------------------------
df <- as.data.frame(t(sapply(results_notnull, rbind)), stringsAsFactors = FALSE)
names(df) <- c("doi", "times_cited", "recent_citations",
               "highly_cited_1", "highly_cited_5", "highly_cited_10", 
               "relative_citation_ratio", "field_citation_ratio")

df_f <- Flattener(df)

saveRDS(df_f, "dimensions_results_virta.RDS")

#------------------------------
#-- Join with VIRTA metadata 
#------------------------------

# One entry not matched because there was a field separator error
# in 10.1007/s11664-015-4152-3 so that one was not queried. 
# Resulted in a DOI '214'. Corrected now in raw VIRTA data.
# Quick onetime query for that item only.
oneitem <- fromJSON(paste0(baseurl, "10.1007/s11664-015-4152-3"))
oneitem_df <- as.data.frame(t(sapply(oneitem, rbind)), stringsAsFactors = FALSE)
oneitem_f <- Flattener(oneitem_df)
df_f <- rbind(df_f, oneitem_f)
dim_metadata <- left_join(df_f, data)
dim_metadata <- dim_metadata[dim_metadata$doi != "214",]

#-----------------------------------------------------------
# Separate multiple units, i.e make new rows from rows 
# where there are space separated values in the units column
#-----------------------------------------------------------

dim_metadata_units_separated <- separate_rows(dim_metadata, "units", sep = " ")

#--------------------------------------------------
# Join with org data from CRIS, and add School color
#--------------------------------------------------
units <- read_excel("Organisaatiolistaus-30_01_2018.xls")
units <- units %>% 
  filter(!is.na(`Ids-1`)) %>%
  select(-`Visibility-0`)

units$`Ids-1` <- gsub("Organisation ID: ","",units$`Ids-1`)
units <- units[grep("^[AETU].*", units$`Ids-1`),]
names(units) <- c("id", "name", "parent")

data_units <- left_join(dim_metadata_units_separated, units, by = c("units"="id"))

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


#---------------------------
#--  Query Unpaywall API
#---------------------------

results <- data.frame()
rows <- nrow(dim_metadata_units_separated)

for( i in 1:rows ) {
  possibleError <- tryCatch(
    res <- oadoi_fetch(dim_metadata_units_separated[i, "doi"], email="sonkkilat@gmail.com"),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  message("Retrieving DOI nr ", i, " out of ", rows)
  
  if ( ncol(res)==13 ) # sometimes title is missing
  {
    results <- rbind(results, res)
  }
  
}

saveRDS(results, "unpaywall.RDS")


#----------------------------------------------------
# Join Unpaywall results with the rest of data 
#----------------------------------------------------

data_oa <- left_join(dim_metadata_units_separated, results, by = c("doi"="doi"))

data_oa_url <- data_oa %>% 
  filter(!is.na(url))

data_oa_url_unique <- data_oa_url[!duplicated(data_oa_url[,c('doi','units')]),]

data_oa_url_unique <- data_oa_url_unique %>% 
  rename(title = title.x) %>% 
  mutate(evidence = gsub("\\s\\(.*\\)", "", evidence)) %>% 
  rowwise() %>% 
  mutate(stroke = if ( is.na(evidence) ) "white" else {ifelse(evidence %in% c("open", "hybrid"), "bronze",
                                                              ifelse(evidence=="oa repository", "green",
                                                                     ifelse(evidence=="oa journal", "gold", "white")))}) %>% 
  select(doi, times_cited, recent_citations, relative_citation_ratio, field_citation_ratio,
         year, title, authors, oa, units, fieldcount, name, parent, color,
         evidence, free_fulltext_url, stroke)

data_oa_url_unique$times_cited <- as.integer(data_oa_url_unique$times_cited)
data_oa_url_unique$recent_citations <- as.integer(data_oa_url_unique$recent_citations)
data_oa_url_unique$relative_citation_ratio <- as.numeric(data_oa_url_unique$relative_citation_ratio)
data_oa_url_unique$field_citation_ratio <- as.numeric(data_oa_url_unique$field_citation_ratio)

saveRDS(data_oa_url_unique, "shiny_data.RDS")



