library(qgraph)
library(igraph)
library(tidyverse)
library(parallel)

source("02_Code/Functions/Network/edge_weight_threshold.R")

# get all indyNets
networks_folder <- "00_Cold_Storage/04_Individual_Networks/igraph"

indy_net_files <- dir(networks_folder, full.names = TRUE)
indy_nets <- tibble(
  param = str_sub(indy_net_files, start = -14, end = -13) %>% parse_integer(),
  subj = str_sub(indy_net_files, start = -6, end = -5) %>% parse_integer(),
  path = indy_net_files
)
results <- tibble(
  param = rlang::int(),
  subj = rlang::int(),
  transitivity = rlang::new_double(0),
  transitivity_random = rlang::new_double(0),
  APL = rlang::new_double(0),
  APL_random = rlang::new_double(0),
  index = rlang::new_double(0)
)


# define analysis function -----------------------------------------------------

analyze_swi <- function(path, prop = 0.5) {
  
  graph <- readRDS(path)
  g <- delete_edge_attr(edge_weight_threshold(graph, quantile(E(graph)$weight, prop)), "weight")
  smallworldIndex(g)
  
}

# run the analysis -------------------------------------------------------------

for(indy_subj in 1:10) {
  
  tictoc::tic()
  cl <- makeForkCluster(nnodes = 25)
  output <- clusterApply(cl = cl,
                         indy_nets %>% filter(subj == indy_subj) %>% pull(path),
                         analyze_swi)
  stopCluster(cl)
  gc()
  tictoc::toc()
  # takes about 10 min on sciCORE, uses 6GB/core RAM 
  
  x = output %>% map(as_tibble_row)
  results_processed <- x[[1]]
  for (i in 2:length(x)) {
    results_processed <- results_processed %>% bind_rows(x[[i]])  
  }
  results_processed <- results_processed %>%
    mutate(param = 1:25, subj = indy_subj)
  results <- results %>% bind_rows(results_processed)
  
  print(paste("done with", indy_subj))
}

write_csv(results, "01_Data/05_indyNets/small_world_index.csv")




