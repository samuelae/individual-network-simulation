# script to analyze the behavioral data and indyNets ---------------------------

library(tidyverse)
library(qgraph)
library(igraph)
library(Matrix)
library(parallel)

source("02_Code/Functions/Network/edge_weight_threshold.R")
source("02_Code/Functions/Network/small_world_index.R")

# define study design parameters -----------------------------------------------

study_parameters <- expand_grid(breadth = c("narrow", "mixed", "broad"),
                                size = c(10, 100, 1000),
                                responses = c(1, 10, 100))

# set up behavioral data simulation --------------------------------------------

# individual networks and prob_mats
indy_net_folder <- "00_Cold_Storage/04_Individual_Networks/igraph/"
behavioral_data_folder <- "00_Cold_Storage/05_Behavioral_Data/"
analysis_results_folder <- "00_Cold_Storage/09_SWI_Analysis_Results/"

# infer and analyze the behavioral data ----------------------------------------

# function to run the analysis
run_parameter_analysis <- function(indy_param, indy_subj) {
  
  # load all necessary functions
  source("02_Code/Functions/Inference/infer_assonet.R")
  source("02_Code/Functions/Inference/infer_rjnet.R")
  source("02_Code/Functions/Network/analyze_network.R")
  
  # load individual network data and behavioral data
  indy_net <- readRDS(paste0(indy_net_folder, "indy_net_", "param_", 
                             sprintf("%02d", indy_param), "_subj_", 
                             sprintf("%02d", indy_subj), ".rds"))
  
  for(study in 1:nrow(study_parameters)) {
    
    behav_data <- readRDS(paste0(behavioral_data_folder, "data_", "param_", 
                                 sprintf("%02d", indy_param), "_subj_", 
                                 sprintf("%02d", indy_subj), "_study_", 
                                 sprintf("%02d", study), ".rds"))
    results_multiset <- list()
    
    for(cue_set in 1:10) {
      
      this_behav_data <- behav_data[[cue_set]]
      
      # store order of cues for later sorting
      order_of_cues <- this_behav_data$fa_data$cue %>% unique()
      
      # generate individual net subgraph and its ordered adjacency matrix
      indy_net_sub <- induced_subgraph(indy_net,
                                       vids = V(indy_net)[names(V(indy_net)) %in% order_of_cues],
                                       impl = "auto")
      indy_adj_mat <- as_adjacency_matrix(indy_net_sub,
                                          type = "both",
                                          attr = "weight") %>% as.matrix()
      indy_adj_mat <- indy_adj_mat[order_of_cues, order_of_cues]
      
      # generate inferred association network and ordered adjacency matrix
      fa_adj_mat <- infer_assonet(this_behav_data$fa_data, ppmi = TRUE, svd = FALSE)
      fa_adj_mat <- fa_adj_mat[order_of_cues, order_of_cues]
      fa_graph <- graph_from_adjacency_matrix(fa_adj_mat, 
                                              weighted = TRUE, 
                                              mode = "undirected",
                                              diag = FALSE)
      
      # generate inferred relatedness judgment network and ordered adjacency matrix
      # uses workaround for imperfect rj_data that does not include all words of the cueset
      rj_adj_mat <- infer_rjnet(this_behav_data$rj_data, cueset = rownames(fa_adj_mat))
      rj_adj_mat <- rj_adj_mat[order_of_cues, order_of_cues]
      rj_graph <- graph_from_adjacency_matrix(rj_adj_mat, 
                                              weighted = TRUE, 
                                              mode = "undirected",
                                              diag = FALSE)
      
      
      
      # assemble list of results
      results <- list(parameters = list(indy_subj = this_behav_data$indy_subj,
                                        indy_param = this_behav_data$indy_param,
                                        breadth = this_behav_data$breadth,
                                        size = this_behav_data$size,
                                        responses = this_behav_data$responses),
                      fa_swi = small_world_index(fa_graph),
                      rj_swi = small_world_index(rj_graph),
                      indy_swi = small_world_index(indy_net_sub))
      
      results_multiset[[cue_set]] <- results
      
    } # end for cue_set
    
    saveRDS(results_multiset, paste0(analysis_results_folder, "swi_param_", 
                                     sprintf("%02d", indy_param),  "_subj_", 
                                     sprintf("%02d", indy_subj), "_study_", 
                                     sprintf("%02d", study), ".rds"))
    
  } # end for study
  
} # end run_parameter_analysis()

# run the analysis for each subject and parameter ------------------------------

for (subject in 1:10) {
  
  tictoc::tic()
  cl <- makeForkCluster(nnodes = 25)
  output <- clusterApply(cl = cl,
                         1:25,
                         run_parameter_analysis,
                         indy_subj = subject)
  stopCluster(cl)
  gc()
  tictoc::toc() # 14 min
  
}


# check completeness -----------------------------------------------------------
# 
files <- dir(path = analysis_results_folder)
# check for subject completeness
for (subj in 1:10) {
  paste(subj, str_detect(files, paste0("_subj_", sprintf("%02d", subj), "_")) %>% sum()) %>% print()
}
# # check for param completeness
# for (param in 1:25) {
#   paste(param, str_detect(files, paste0("_param_", sprintf("%02d", param), "_")) %>% sum()) %>% print()
# }
# # check for study completeness
# for (study in 1:27) {
#   paste(study, str_detect(files, paste0("_study_", sprintf("%02d", study))) %>% sum()) %>% print()
# }
