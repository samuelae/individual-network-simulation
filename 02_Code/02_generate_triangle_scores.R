# dependencies -----------------------------------------------------------------

# packages
library(tidyverse)
library(igraph)

# functions
source("02_Code/Functions/Helpers/chunk.R")
source("02_Code/Functions/Network/triangle_score.R")

# read fastText data from fs ---------------------------------------------------

graph <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_fully_connected.rds")

# calculate triangle scores ----------------------------------------------------

# create adjacency matrix for fast triangle_score function
graph_adj <- as_adj(graph, attr = "weight") %>% as.matrix()
saveRDS(graph_adj, "00_Cold_Storage/02_fastText_Network/base_graph_fully_connected_adj.rds")
gc()

# Testing chunking on sciCORE (edges total: 90.929.355)

# chunk size     ; n cores; duration ; per edge     ; total duration
#    20.000 edges; 2 cores;   86.58 s; 0.0043 s/edge; 4.5 days total
#    40.000 edges; 4 cores;  126.64 s; 0.0032 s/edge; 3.3 days total
#    40.000 edges; 6 cores;  127.41 s; 0.0032 s/edge; 3.4 days total
#   100.000 edges; 4 cores;  230.22 s; 0.0023 s/edge; 2.4 days total
# 1.000.000 edges; 4 cores; 1999.50 s; 0.0020 s/edge; 2.1 days total

# chunk the total edges
n_runs <- ceiling(ecount(graph) / 1000000)
n_workers <- 4

# run the calculation on 4 cores (in 2.1 days) on sciCORE (~ 20GB RAM per core)
for(run in 1:n_runs) {
  edge_ids <- ((run - 1) * 1000000 + 1):(run * 1000000)
  if(run == max(n_runs)) {
    edge_ids <- edge_ids[1]:ecount(graph)
  }
  chunks <- chunk(edge_ids, n_workers)
  future::plan("cluster", workers = n_workers)
  result_mc4 <- furrr::future_map(chunks, 
                                  triangle_score, 
                                  graph = graph, 
                                  graph_adj = graph_adj)
  combined_results <- as.numeric(unlist(result_mc4))
  saveRDS(combined_results, 
          paste0("00_Cold_Storage/02_fastText_Network/triangle_scores/ts_",
                 sprintf("%03d", run),
                 ".rds"))
  rm(result_mc4)
  rm(combined_results)
  gc()
}

# combine triangle_score files into one
ts_files <- list.files("00_Cold_Storage/02_fastText_Network/triangle_scores/")
triangle_scores <- numeric(0)
pb <- txtProgressBar(style = 3, min = 0, max = length(ts_files))
for(f in 1:length(ts_files)) {
  ts_file <- readRDS(paste0("00_Cold_Storage/02_fastText_Network/triangle_scores/",
                            ts_files[f]))
  triangle_scores <- c(triangle_scores, ts_file)
  setTxtProgressBar(pb, f)
}
close(pb)

# save to file system
saveRDS(triangle_scores, "00_Cold_Storage/02_fastText_Network/base_graph_fully_connected_triangle_scores.rds")




