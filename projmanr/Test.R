library(projmanr)
library(hash)
library(plotly)
library(igraph)
data <- taskdata1
data <- read.csv("test.csv")
res <- critical_path(data, gantt = T)
(res)
res$critical_path

gantt(data, raw = T)
gantt(res$results, raw = F)
