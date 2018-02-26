#--------------------
# Plot NA values
#--------------------
library(tidyverse)
library(lattice)

data <- readRDS("shiny_data.RDS")
data <- data[!duplicated(data[,c('doi')]),]

#---------------
# Publishers
#---------------
publishers <- unique(data$publisher)

# http://www.rensenieuwenhuis.nl/r-sessions-30-visualizing-missing-values/
var.NA <- matrix(ncol=length(publishers), nrow=26)

for (i in 1:dim(data)[2])
{
  var.NA[i,] <- tapply(data[[i]], data$publisher, function(x) sum(is.na(x)) / length(x))
}

dimnames(var.NA) <- list(
  names(data),
  sort(publishers))

publ_stats <- data %>%
  group_by(publisher) %>%
  summarise(n = n()) %>%
  filter(n >= 20) %>%
  arrange(desc(n))

publ_names <- publ_stats$publisher

var_names <- c("urls", "times_cited_wos")

# https://stackoverflow.com/a/7352287
p <- var.NA[rownames(var.NA) %in% var_names,
            colnames(var.NA) %in% publ_names, 
            drop=FALSE]

trellis.par.set(canonical.theme(color = FALSE))
levelplot(p,
          scales=list(x=list(rot=90)),
          main="Percentage of missing variables",
          xlab="Variable",
          ylab="Publisher")

# Export fron RStudio as PNG 1500x800 

#-----------------
# Schools
#-----------------
data <- readRDS("shiny_data.RDS")
schools <- unique(data$parent)

var_s.NA <- matrix(ncol=length(schools), nrow=23)

for (i in 1:dim(data)[2])
{
  var_s.NA[i,] <- tapply(data[[i]], data$parent, function(x) sum(is.na(x)) / length(x))
}

dimnames(var_s.NA) <- list(
  names(data),
  sort(schools))

var_names <- c("urls", "times_cited_wos")

p2 <- var_s.NA[rownames(var_s.NA) %in% var_names,,
            drop=FALSE]

trellis.par.set(canonical.theme(color = FALSE))

levelplot(p2,
          scales=list(x=list(rot=90)),
          main="Percentage of missing variables",
          xlab="Variable",
          ylab="School")
