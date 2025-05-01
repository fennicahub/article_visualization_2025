library(pxweb)
library(tidyverse)
library(ggplot2)

#d <- pxweb_interactive(names(pxweb_apis)[[2]])
# saveRDS(d, file="pxweb_query.Rds")
d <- readRDS("pxweb_query.Rds")
x <- d$data[2:310, ]