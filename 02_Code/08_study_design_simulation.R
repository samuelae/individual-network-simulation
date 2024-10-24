# generate behavioral data for study design parameters -------------------------

# packages
library(tidyverse)
library(igraph)
library(Matrix)
library(parallel)

# functions
source("02_Code/Functions/Behavioral_Data/generate_rj_words.R")
source("02_Code/Functions/Behavioral_Data/generate_free_associations.R")
source("02_Code/Functions/Behavioral_Data/generate_relatedness_judgments.R")

# define study design parameters -----------------------------------------------

# parameters
study_parameters <- expand_grid(breadth = c("narrow", "mixed", "broad"),
                                size = c(10, 100, 1000),
                                responses = c(1, 10, 100))

# cue sets
cue_sets <- readRDS("01_Data/06_Cue_Sets/cue_sets.rds")

# set up behavioral data simulation --------------------------------------------

# individual networks and prob_mats
indy_net_folder <- "00_Cold_Storage/04_Individual_Networks/igraph/"
prob_mat_folder <- "00_Cold_Storage/04_Individual_Networks/prob_mat/"
behavioral_data_folder <- "00_Cold_Storage/04_Individual_Networks/behavioral_data/"

# function to run one indyNet subject from one indyNet parameter with one study 
# parameter combination
run_study <- function(study_params,
                      cue_set,
                      indy_net,
                      prob_mat) {
  # study_params (list: $breadth, $size, $responses): parameters for this run
  # indy_net (igraph): individual network
  # prob_mat (Matrix): probability matrix (based on indy_net and frequency)
  # return (list): behavioral data for this subject and parameter combination
  
  # generate free associations
  fa_data <- generate_free_associations(probability_matrix = prob_mat,
                                        cue_set = cue_set,
                                        n_responses = 3,
                                        n_repetitions = study_params$responses,
                                        n_subjects = 1)
  
  # prepare word paris for relatedness judgments
  rj_words <- generate_rj_words(cue_set, 
                                n_trials = study_params$size * 
                                           study_params$responses * 3)
  
  # generate relatedness judgments
  rj_data <- generate_relatedness_judgments(words_1 = rj_words$words_1,
                                            words_2 = rj_words$words_2,
                                            semantic_representation = indy_net)
  
  # combine data and return
  subj_data <- list(study_params = study_params,
                    fa_data = fa_data,
                    rj_data = rj_data)
  
  subj_data
}

# function to run all 27 study parameters for one indyNet subject in one indyNet
# parameter
run_studies <- function(indy_subj,
                        indy_param,
                        study_parameters) {
  # run all study parameters (currently 27) for one indyNet subject of one indyNet
  # parameter
  # indy_subj (int): which indyNet subject to run (currently 1:10)
  # indy_param (int): which indyNet parameter to run (currently 1:25)

  # read indyNet data for this subject from file system
  indy_net <- readRDS(paste0(indy_net_folder, "/indy_net_", "param_", 
                             sprintf("%02d", indy_param), "_subj_", 
                             sprintf("%02d", indy_subj), ".rds"))
  prob_mat <- readRDS(paste0(prob_mat_folder, "/prob_mat_", "param_", 
                             sprintf("%02d", indy_param), "_subj_", 
                             sprintf("%02d", indy_subj), ".rds"))
  
  # iterate through study parameters (27 studies)
  for (study in 1:nrow(study_parameters)) {
    
    # get cue-sets for this study
    study_cue_sets <- cue_sets %>% 
      filter(breadth == study_parameters[study, ]$breadth,
             size == study_parameters[study, ]$size)
    
    study_data_multiset <- list()
    
    # iterate through cue sets (10 cue sets)
    for (set in 1:nrow(study_cue_sets)) {
      
      study_data <- run_study(study_params = study_parameters[study, ],
                              cue_set = study_cue_sets$set[[set]],
                              indy_net = indy_net,
                              prob_mat = prob_mat)
      
      study_data <- list(indy_subj = indy_subj,
                         indy_param = indy_param,
                         study_param = study,
                         breadth = study_data$study_params$breadth,
                         size = study_data$study_params$size,
                         responses = study_data$study_params$responses,
                         cue_set_no = set,
                         fa_data = study_data$fa_data,
                         rj_data = study_data$rj_data)
      
      study_data_multiset[[set]] <- study_data
      
    }

    saveRDS(study_data_multiset, paste0(behavioral_data_folder, "data_", "param_", 
                                        sprintf("%02d", indy_param), "_subj_", 
                                        sprintf("%02d", indy_subj), "_study_", 
                                        sprintf("%02d", study), ".rds"))
    rm(study_data, study_data_multiset); gc()
    
  }
  
}

run_subject <- function(indy_subj,
                        indy_params,
                        study_parameters) {
  # function runs one indyNet subject trough all indyNet parameters and all
  # study parameters
  # indy_subj (int): currently 1:10
  # indy_params (vector of ints): currently 1:25
  # study_parameters = currently 27 study parameter combinations
  
  for (indy_param in indy_params) {
    run_studies(indy_subj = indy_subj,
                indy_param = indy_param,
                study_parameters = study_parameters)
  }
}

run_parameter <- function(indy_parameter,
                          indy_subjects = 1:10,
                          study_parameters) {
  # parallelized over parameters version of run subject
  for (subject in indy_subjects) {
    run_studies(indy_subj = subject,
                indy_param = indy_parameter,
                study_parameters = study_parameters)
  }
  
}

# run the behavioral data generation for all subjects in parallel --------------

# parallel parameter run, 1 subj in 21 min on 25 cores sciCORE (3.5h total)
cl <- makeForkCluster(nnodes = 25)
tictoc::tic()
output <- clusterApply(cl = cl,
                       1:25,
                       run_parameter,
                       indy_subjects = 1:10,
                       study_parameters = study_parameters)
stopCluster(cl)
tictoc::toc()
gc()



