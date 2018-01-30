library(RCurl)
library(data.table)
library(jsonlite)
library(tidyverse)
library(httr)

# https://stackoverflow.com/a/35497845
# Just added stringsAsFactors=FALSE to the cbind function call
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

#--------------------------------------------------------------------------------------------------
# XSLT transformation XML->CSV
# java -jar ~/saxonee/saxon9ee.jar aaltopubl_2018-01-25.zip parse.xsl -o:aaltopubl_2018-01-25.csv
#--------------------------------------------------------------------------------------------------

# Replaced '|' in one title with a space because '|' is used as a field separator
#
# sed -i "s/Microstructural evolution and mechanical properties of Au-20wt.%Sn|Ni interconnection/
#       Microstructural evolution and mechanical properties of Au-20wt.%Sn Ni interconnection/g" aaltopubl_2018-01-25.csv

data <- read.csv("aaltopubl_2018-01-25.csv", sep = "|", quote = "", header = FALSE, stringsAsFactors = FALSE)
names(data) <- c("doi", "year", "title", "journal", "authors", "oa", "units", "fieldcount", "fields")

# Quick and dirty cleaning. Does not fix all DOIs but most
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
  
# Handle bad DOI queries
# https://stackoverflow.com/a/8094059
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

# Bad DOIs throw an error and add a NULL list element, so leaving them out
results_notnull <- results[-which(sapply(results, is.null))]

# https://stat.ethz.ch/pipermail/r-help/2006-August/111368.html
df <- as.data.frame(t(sapply(results_notnull, rbind)), stringsAsFactors = FALSE)
names(df) <- c("doi", "times_cited", "recent_citations",
               "highly_cited_1", "highly_cited_5", "highly_cited_10", 
               "relative_citation_ratio", "field_citation_ratio")

# And then truly flatten with helper functions above
df_f <- Flattener(df)

saveRDS(df_f, "dimensions_results_virta.RDS")

#--------------------
#-- Join with metadata
#--------------------

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

#-----------------------------------
# Separate units,
# i.e make new rows from rows with
# space separated units column
#------------------------------------

dim_metadata_units_separated <- separate_rows(dim_metadata, "units", sep = " ")

#--------------------------
# Make a shorter title
#--------------------------

data_to_shiny <- dim_metadata_units_separated %>% 
  select(doi, times_cited, recent_citations, relative_citation_ratio, field_citation_ratio,
         year, title, authors, oa, units, fieldcount)

saveRDS(data_to_shiny, "dim_data.RDS")

