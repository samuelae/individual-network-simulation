library(igraph)
library(tidyverse)

base_graph <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_0.2.rds") 

aspl <- mean_distance(base_graph, weights = NA)
aspl # 1.871978

