library(wosr)

Sys.setenv(WOS_USERNAME = "...", WOS_PASSWORD = "...")

sid <- auth()
q <- 'DO = 10.1002/jctb.4777'
res <<- pull_wos(q, editions = c("SCI", "SSCI"))
res$publication$tot_cites
# 3