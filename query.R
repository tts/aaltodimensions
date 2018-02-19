library(RCurl)
library(data.table)
library(jsonlite)
library(tidyverse)
library(httr)
library(readxl)
library(roadoi)
library(rvest)
library(rAltmetric)

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

saveRDS(data, "virta_data_temp.RDS")

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
rows <- nrow(data_units)

for( i in 1:rows ) {
  
  possibleError <- tryCatch(
    res <- oadoi_fetch(data_units[i, "doi"], email="sonkkilat@gmail.com"),
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
  
results_oa_loc <- results %>%
  mutate(urls = purrr::map(best_oa_location, "url") %>%
           purrr::map_if(purrr::is_empty, ~ NA_character_) %>%
           purrr::flatten_chr())

#----------------------------------------------------
# Join Unpaywall results with the rest of data 
#----------------------------------------------------

data_oa <- left_join(data_units, results_oa_loc, by = c("doi"="doi"))
data_oa <- data_oa[!duplicated(data_oa[,c('doi','units')]),]

data_oa <- data_oa %>%
  rename(year = year.x,
         title = title.x,
         is_oa = is_oa.x,
         journal_name = journal_name.x,
         publisher = publisher.x,
         urls = urls.x) %>%
  mutate(symbol = ifelse(is_oa == TRUE, 3, 1)) %>%
  select(doi, times_cited, recent_citations, relative_citation_ratio, field_citation_ratio,
         year, title, journal_name, publisher, authors, oa, units, fieldcount, fields, name, parent, 
         color, is_oa, symbol, urls)

data_oa$times_cited <- as.integer(data_oa$times_cited)
data_oa$recent_citations <- as.integer(data_oa$recent_citations)
data_oa$relative_citation_ratio <- as.numeric(data_oa$relative_citation_ratio)
data_oa$field_citation_ratio <- as.numeric(data_oa$field_citation_ratio)

#--------------------------------------------
# Join with WoS citation data 
#
# 1. make char vector of DOIs
# 2. split in two (WoS query max is 6000)
# 3. paste to a "DO='doi' OR DO='doi'..." string
# 4. copy-paste to the WoS GUI Advanced Search
# 5. download result set in 500 item chunks
# 6. in Excel, filter columns DI and TC (cites)
#    and prepend to a single sheet
# 7. save as CSV
#--------------------------------------------

dois <- unique(data_oa$doi)

dois_1 <- dois[1:5000] 
dois_2 <- dois[5001:9493]
dois1_q <- paste(dois_1, collapse='" OR DO="')
dois1_q <- paste0('DO="', dois1_q, '"')
cat(dois1_q,file="dois1_q.txt")

dois2_q <- paste(dois_2, collapse='" OR DO="')
dois2_q <- paste0('DO="', dois2_q, '"')
cat(dois2_q,file="dois2_q.txt")

wos <- read.csv("wos.csv", stringsAsFactors = FALSE, sep = ";")

data_wos <- left_join(data_oa, wos, by = c("doi"="DI"))
data_wos <- data_wos %>% 
  rename(wos = TC) %>% 
  mutate(oa = ifelse(oa == 0, "Not OA",
                     ifelse(oa == 1, "OA",
                            ifelse(oa == 2, "Green OA",
                                   "Not known"))),
         shorttext = ifelse(nchar(title) > 100, substr(title,1,100), title),
         shortj = ifelse(nchar(journal_name) > 50, substr(journal_name,1,50), journal_name),
         text = paste0('<b>',parent,'</b>','<br>',name,'<br><b>',shorttext,'</b><br>',shortj,'<br>',publisher,' (',year,')','<br>Nr of authors: ',authors,'<br>OA full text available? ',is_oa))

saveRDS(data_wos, "shiny_data.RDS")

#-----------------------------------
# Join with some Altmetric data:
# Altmetric score, Mendeley readers,
# Tweets
#-----------------------------------
results <- list()
rows <- nrow(data_wos)

for( i in 1:rows ) {
  possibleError <- tryCatch(
    res <- plyr::llply(paste0('doi:',data_wos[i, "doi"]), altmetrics),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  message("Querying DOI nr ", i, " out of ", rows)
  results[i] <- res
}

metric_data <- plyr::ldply(results, altmetric_data)
write.csv(metric_data, file = "altm.csv")

data_wos_altm <- left_join(data_wos, metric_data, by = c("doi"="doi")) %>% 
  select(doi, times_cited, recent_citations, wos, relative_citation_ratio, field_citation_ratio,
         year, title.x, journal_name, publisher, authors.x, oa, units, fieldcount, name, parent, 
         color, is_oa.x, symbol, urls, text, shorttext, shortj, cited_by_tweeters_count, mendeley, score) %>% 
  rename(altm_score = score,
         title = title.x,
         is_oa = is_oa.x,
         authors = authors.s,
         tweets = cited_by_tweeters_count)

data_wos_altm$mendeley <- as.numeric(data_wos_altm$mendeley)

saveRDS(data_wos_altm, "shiny_data.RDS")


#------------------------------------------
# Scrape English names of the research
# fields from Statistics Finland
#------------------------------------------

doc <- read_html("http://www.tilastokeskus.fi/meta/luokitukset/tieteenala/001-2010/index_en.html")

codes <- doc %>%
  html_nodes(xpath = "//table[1]//table//tr/td[1]") %>%
  html_text()

fields <- doc %>%
  html_nodes(xpath = "//table[1]//table//tr/td[2]") %>%
  html_text()

codes_fields <- as.data.frame(cbind(codes, fields), stringsAsFactors = F)

write.csv(codes_fields, "codes_fields.csv", row.names = F)

#----------------------------------------
# Split VIRTA field codes 
# and join with its name
#----------------------------------------

data_wos_f <- separate_rows(data_wos, "fields", sep = " ")
data_wos_f <- data_wos_f[!duplicated(data_wos_f[,c('doi','units','fields')]),]

data_wos_f <- data_wos_f %>% 
  rename(year = year.x,
         title = title.x,
         authors = authors.x,
         oa = oa.x,
         units = units.x,
         fieldcount = fieldcount.x) %>% 
  select(-ends_with(".y"))

data_wos_f <- left_join(data_wos_f, codes_fields, by = c("fields"="codes")) %>% 
  filter(!is.na(fields)) %>% 
  rename(field_name = fields.y)

data_wos_f$fields <- as.integer(data_wos_f$fields)

#----------------------------------
# Calculate means for the heatmap
#----------------------------------

f_means <- data_wos_f %>% 
  group_by(parent, name, fields) %>% 
  mutate(wos = ifelse(is.na(wos), 0, wos)) %>% 
  summarise(Field_citation_ratio = round(mean(field_citation_ratio),2),
            Times_cited = round(mean(times_cited),2),
            Times_cited_wos = round(mean(wos),2),
            Relative_citation_ratio = round(mean(relative_citation_ratio),2),
            Recent_citations = round(mean(recent_citations),2)) %>% 
  ungroup()

saveRDS(f_means, "f_means.RDS")



