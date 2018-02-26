library(tidyverse)
library(lattice)

plot_na <- function(data, var, metrics) {
  
  v <- enquo(var)
  
  d_rows <- data %>% 
    distinct(!!v)

  var.NA <- matrix(ncol=nrow(d_rows), nrow=ncol(data))
  
  for (i in 1:dim(data)[2])
  {
    var.NA[i,] <- tapply(X = data[[i]], INDEX = data %>% select(!!v), function(x) sum(is.na(x)) / length(x))
  }
  
  dimnames(var.NA) <- list(
    names(data),
    sort(d_rows[,1]))
  
  stats <- data %>%
    group_by(!!v) %>%
    summarise(n = n()) %>%
    filter(n >= 20) %>%
    arrange(desc(n))
  
  col_names <- stats %>%
    ungroup() %>% 
    select(!!v)
  
  col_names <- data.frame(col_names)
  
  col_names_v <- col_names[,1]
  
  var_names <- metrics
  
  p <- var.NA[rownames(var.NA) %in% var_names,
              colnames(var.NA) %in% col_names_v,
              drop=FALSE]

  trellis.par.set(canonical.theme(color = FALSE))
  
  levelplot(p,
            scales=list(x=list(rot=90)),
            main="Percentage of missing variables",
            xlab="Variable",
            ylab = "")
  
}


data <- readRDS("shiny_data.RDS")
data_uniq <- data[!duplicated(data[,c('doi')]),]
metrics <- c("urls", "wos")

plot_na(data_uniq, publisher, metrics)
plot_na(data, parent, metrics)


