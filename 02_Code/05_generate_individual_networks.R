# based on 03_generate_individual_networks_NEXT_v5.r

# dependencies -----------------------------------------------------------------

# packages
library(igraph)
library(dplyr)
library(readr)
library(parallel)

# functions
source("02_Code/Functions/Behavioral_Data/make_probability_matrix.R")
source("02_Code/Functions/Helpers/sample_dac.R")

# load data from fs ------------------------------------------------------------

base_graph <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_fully_connected.rds")
triangle_scores <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_fully_connected_triangle_scores.rds")

# generate individual networks -------------------------------------------------

# function definition using parameters to vary clustering and connectedness
generate_individual_network <- function(base_graph,
                                        triangle_scores,
                                        p,
                                        r,
                                        n_touch = 44000000,
                                        ts_cutoff = 23,
                                        edge_cutoff = 0.2) {
  
  e_ids <- 1:length(triangle_scores)
  ts_large <- triangle_scores > ts_cutoff
  
  # get edge samples from large (p) and small (1 - p) triangle_score groups
  edge_sample_ts_large <- sample(x = e_ids[ts_large], size = p * n_touch) 
  edge_sample_ts_small <- sample(x = e_ids[!ts_large], size = (1 - p) * n_touch) 
  edge_sample <- c(edge_sample_ts_large, edge_sample_ts_small)
  
  rm(edge_sample_ts_large, edge_sample_ts_small, ts_large); gc()
  
  # delete edge-weight for (r) and relocate edge-weights for (r - 1) of the edges
  edge_r <- rbinom(length(edge_sample), size = 1, prob = r) == 1
  
  if(sum(!edge_r) > 0) {
    
    # potential targets
    potential_target_ids <- e_ids[-edge_sample]
    potential_target_weights <- edge_attr(base_graph, "weight", potential_target_ids)
    
    # find actual targets (faster): attention: there is no real difference in weight
    # in the sampled vs. not sampled groups
    targets_ids <- sample_dac(xs = potential_target_ids, 
                              n = sum(!edge_r), 
                              ps = potential_target_weights)
    targets_ids <- sample(targets_ids)
    
    rm(potential_target_ids, potential_target_weights); gc()
    
    # get current weights of targets
    targets_weights <- edge_attr(base_graph, "weight", targets_ids)
    
    # get current weights of edges to relocate the weight from
    sources_ids <- edge_sample[!edge_r]
    sources_weights <- edge_attr(base_graph, "weight", edge_sample[!edge_r])
    
    # deal with uneven lengths (minor differences due to sample_dac)
    if(length(targets_weights) > length(sources_weights)) {
      targets_ids <- head(targets_ids, length(sources_weights))
      targets_weights <- head(targets_weights, length(sources_weights))
    } else if (length(targets_weights) < length(sources_weights)) {
      sources_ids <- head(sources_ids, length(targets_weights))
      sources_weights <- head(sources_weights, length(targets_weights))
    }
    
    # determine proportion to relocate
    props <- runif(length(sources_weights), 0, 1)
    
    # add proportion sources_weights to the targets_weights
    targets_weights <- targets_weights + (sources_weights * props)
    
    # remove the relocated weight from the sources_weights
    sources_weights <- sources_weights * (1 - props)
    
    
    # add partial weights from 1-r edges to target edges
    base_graph <- set_edge_attr(base_graph, 
                                "weight", 
                                index = targets_ids, 
                                value = targets_weights)
    # remove partial weight from 1-r edges from source edges
    base_graph <- set_edge_attr(base_graph,
                                "weight",
                                index = sources_ids,
                                value = sources_weights)
    
    rm(targets_ids, targets_weights, sources_ids, sources_weights, props); gc()
  }
  
  # delete edge weights
  # base_graph <- delete_edges(base_graph, edge_sample)
  
  delete_ids <- edge_sample[edge_r]
  delete_weights <- runif(length(delete_ids), 
                          min = 0, 
                          max = 1) * edge_attr(base_graph, "weight", delete_ids)
  
  base_graph <- set_edge_attr(base_graph, 
                              "weight",
                              index = delete_ids,
                              value = delete_weights)
  
  rm(delete_ids, delete_weights, edge_r, edge_sample); gc()
  
  # remove edges below weight cutoff
  ids_below <- (1:length(e_ids))[edge_attr(base_graph, "weight") < edge_cutoff]
  rm(e_ids); gc()
  
  base_graph <- delete_edges(base_graph, ids_below)
  rm(ids_below); gc()
  
  # out
  base_graph
}


# create the generation parameters ---------------------------------------------

# create parameters manually to produce best effort of independent cc and strength
grid <- tibble(ps = c(0.000, 0.000, 0.000, 0.000, 0.045,
                      0.125, 0.125, 0.125, 0.175, 0.225,
                      0.250, 0.250, 0.300, 0.300, 0.375,
                      0.375, 0.375, 0.450, 0.500, 0.550,
                      0.500, 0.550, 0.625, 0.700, 0.750),
               rs = c(1.000, 0.875, 0.750, 0.625, 0.500,
                      0.875, 0.750, 0.625, 0.500, 0.375,
                      0.800, 0.700, 0.550, 0.450, 0.300,
                      0.700, 0.625, 0.450, 0.325, 0.200,
                      0.625, 0.500, 0.325, 0.150, 0.000))

# save paramteter grid for future reference
saveRDS(grid, "00_Cold_Storage/04_Individual_Networks/grid.rds")

# use subtlex data for word frequency based word probabilities
subtlex <- read_table("00_Cold_Storage/01_Source/SUBTLEX-US.txt") %>%
  rename(frequency = FREQcount) %>%
  mutate(word = tolower(Word)) %>%
  filter(word %in% names(V(base_graph))) %>%
  mutate(probability = frequency/sum(frequency)) %>%
  select(word, probability)

# use tuned free association parameters for probability matrix creation
fa_sensitivity_representation <- 10
fa_sensitivity_frequency <- 1

# create individual networks functionally --------------------------------------

create_subject <- function(j) {
  # subject
  for(i in 1:nrow(grid)) {
    try({
      indy_net <- generate_individual_network(base_graph, 
                                              triangle_scores,
                                              p = grid$ps[i], 
                                              r = grid$rs[i])
      saveRDS(indy_net, paste0("00_Cold_Storage/04_Individual_Networks/igraph/indy_net_",
                               "param_", sprintf("%02d", i),
                               "_subj_", sprintf("%02d", j),
                               ".rds"))
      
    })
    prob_mat <- make_probability_matrix(semantic_representation = indy_net,
                                        sensitivity_representation = fa_sensitivity_representation,
                                        word_frequency = subtlex,
                                        sensitivity_frequency = fa_sensitivity_frequency)
    rm(indy_net); gc()
    saveRDS(prob_mat, paste0("00_Cold_Storage/04_Individual_Networks/prob_mat/prob_mat_",
                             "param_", sprintf("%02d", i),
                             "_subj_", sprintf("%02d", j),
                             ".rds"))
    rm(prob_mat); gc()
    print(paste("finished param", i, "of", nrow(grid)))
  }
}

cl <- makeForkCluster(nnodes = 5, outfile = "")
output <- clusterApply(cl = cl,
                       x = 1:10,
                       fun = create_subject)
stopCluster(cl)
saveRDS(output, "00_Cold_Storage/Individual_Networks/cluster_output.rds")

# runs on 5 cores and about 20 GB of RAM for each core in about 5h
