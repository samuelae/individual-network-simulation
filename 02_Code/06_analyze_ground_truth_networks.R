# dependencies -----------------------------------------------------------------

library(tidyverse)
library(igraph)
library(parallel)

source("02_Code/Functions/Network/analyze_network.R")

# set up analysis --------------------------------------------------------------

networks_folder <- "00_Cold_Storage/04_Individual_Networks/igraph"
analysis_results_folder <- "00_Cold_Storage/04_Individual_Networks/network_properties_full/"

indy_net_files <- dir(networks_folder, full.names = TRUE)
indy_nets <- tibble(
  param = str_sub(indy_net_files, start = -14, end = -13) %>% parse_integer(),
  subj = str_sub(indy_net_files, start = -6, end = -5) %>% parse_integer(),
  path = indy_net_files
)

# define analysis function -----------------------------------------------------

analyze_indy_net <- function(input_file, output_folder) {
  try({
    graph <- readRDS(input_file)
    indy_param <- str_sub(input_file, start = -14, end = -13) %>% parse_integer()
    indy_subj <- str_sub(input_file, start = -6, end = -5) %>% parse_integer()
    
    analyis_result <- analyze_network(graph, aspl_sample = FALSE)
    saveRDS(analyis_result, paste0(output_folder, "indy_net_properties_param_", 
                                   sprintf("%02d", indy_param),  "_subj_", 
                                   sprintf("%02d", indy_subj), ".rds"))
  })
}

# run the analysis -------------------------------------------------------------

for(indy_subj in 1:10) {
  
  tictoc::tic()
  cl <- makeForkCluster(nnodes = 25)
  output <- clusterApply(cl = cl,
                         indy_nets %>% filter(subj == indy_subj) %>% pull(path),
                         analyze_indy_net,
                         output_folder = analysis_results_folder)
  stopCluster(cl)
  gc()
  tictoc::toc()
  # takes about 3.5 hours on sciCORE, uses a lot of RAM, use 270GB RAM
  # machine to run 25 cores
  
}

# check files ------------------------------------------------------------------

files <- dir(analysis_results_folder)

for (sub in 1:10) {
  str_detect(files, paste0("_subj_", sprintf("%02d", sub))) %>% sum() %>% print()
}

# combine the results into one tibble ------------------------------------------

result_files <- dir(analysis_results_folder)
param <- integer(length(result_files))
subj <- integer(length(result_files))
n_nodes <- integer(length(result_files))
n_edges <- integer(length(result_files))
n_giant_comp <- integer(length(result_files))
mean_strength <- numeric(length(result_files))
cc <- numeric(length(result_files))
aspl <- numeric(length(result_files))
mod <- numeric(length(result_files))
avg_degr <- numeric(length(result_files))

for (i in 1:length(result_files)) {
  
  result_file <- readRDS(paste0(analysis_results_folder, result_files[i]))
  
  param[i] = str_sub(result_files[i], start = -14, end = -13) %>% parse_integer()
  subj[i] = str_sub(result_files[i], start = -6, end = -5) %>% parse_integer()
  n_nodes[i] = result_file$n_nodes
  n_edges[i] = result_file$n_edges
  n_giant_comp[i] = result_file$n_giant_comp
  mean_strength[i] = result_file$mean_strength
  cc[i] = result_file$cc
  aspl[i] = result_file$aspl
  mod[i] = max(result_file$mod)
  avg_degr[i] = result_file$avg_degr
  
}

indyNet_properties <- tibble(
  param = param,
  subj = subj,
  n_nodes = n_nodes,
  n_edges = n_edges,
  n_giant_comp = n_giant_comp,
  mean_strength = mean_strength,
  cc = cc,
  aspl = aspl,
  mod = mod,
  avg_degr = avg_degr
)

saveRDS(indyNet_properties, "01_Data/05_indyNets/indyNet_properties_full.RDS")



