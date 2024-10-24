# dependencies -----------------------------------------------------------------

# packages
library(tidyverse)
library(patchwork)

# define parameters ------------------------------------------------------------

# study parameters
study_parameters <- expand_grid(breadth = c("narrow", "mixed", "broad"),
                                size = c(10, 100, 1000),
                                responses = c(1, 10, 100))

# read in result files ---------------------------------------------------------

source("02_Code/Functions/Data_IO/read_analysis.R")

analysis_results_folder <- "00_Cold_Storage/06_Inference_Analysis_Results/"

combined_data <- read_analysis(folder_path = analysis_results_folder,
                               nw_params = readRDS("00_Cold_Storage/04_Individual_Networks/grid.rds"),
                               study_params = study_parameters)

# set cc = 1 to NA
combined_data <- combined_data %>% 
  mutate(nw_data_fa_cc = na_if(nw_data_fa_cc, 1),
         nw_data_rj_cc = na_if(nw_data_rj_cc, 1))

# add responses variable
combined_data <- combined_data %>% 
  mutate(study_param_responses = study_param_cue_repetition * 3)

combined_data$cue_set <- as_factor(combined_data$cue_set)

saveRDS(combined_data, "01_Data/09_Analysis_Results/analysis_results.rds")

# summarize data within networks -----------------------------------------------

data_strength <- combined_data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, 
           study_param_breadth, cue_set) %>% 
  summarise(cor_sp = mean(importance_cor_indy_fa, na.rm = TRUE),
            comparison = "indy_fa",
            nw_metric = "strength") %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, 
                       study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(importance_cor_indy_rj, na.rm = TRUE),
                        comparison = "indy_rj",
                        nw_metric = "strength")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, 
                       study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(importance_cor_indy_avg, na.rm = TRUE),
                        comparison = "indy_avg",
                        nw_metric = "strength")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, 
                       study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(importance_cor_fa_rj, na.rm = TRUE),
                        comparison = "fa_rj",
                        nw_metric = "strength"))

data_edge_weight <- combined_data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, 
           study_param_breadth, cue_set) %>% 
  summarise(cor_sp = mean(relatedness_cor_indy_fa, na.rm = TRUE),
            comparison = "indy_fa",
            nw_metric = "edge_weight") %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, 
                       study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(relatedness_cor_indy_rj, na.rm = TRUE),
                        comparison = "indy_rj",
                        nw_metric = "edge_weight")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, 
                       study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(relatedness_cor_indy_avg, na.rm = TRUE),
                        comparison = "indy_avg",
                        nw_metric = "edge_weight")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, 
                       study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(relatedness_cor_fa_rj, na.rm = TRUE),
                        comparison = "fa_rj",
                        nw_metric = "edge_weight"))

# summarize data between networks ----------------------------------------------

data_mean_strength <- combined_data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_mean_strength, nw_data_fa_mean_strength, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "mean_strength") %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mean_strength, nw_data_rj_mean_strength, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "mean_strength")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mean_strength, nw_data_avg_mean_strength, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg",
                        nw_metric = "mean_strength")) %>%  
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mean_strength, 
                                     (nw_data_fa_mean_strength + nw_data_rj_mean_strength) / 2, 
                                     method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg_2",
                        nw_metric = "mean_strength")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_fa_mean_strength, nw_data_rj_mean_strength, method = "spearman", use = "na.or.complete"),
                        comparison = "fa_rj",
                        nw_metric = "mean_strength"))

data_cc <- combined_data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_cc, nw_data_fa_cc, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "cc") %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_cc, nw_data_rj_cc, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "cc")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_cc, nw_data_avg_cc, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg",
                        nw_metric = "cc")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_cc, 
                                     (nw_data_fa_cc + nw_data_rj_cc) / 2, 
                                     method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg_2",
                        nw_metric = "cc")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_fa_cc, nw_data_rj_cc, method = "spearman", use = "na.or.complete"),
                        comparison = "fa_rj",
                        nw_metric = "cc"))

data_aspl <- combined_data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_aspl, nw_data_fa_aspl, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "aspl") %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_aspl, nw_data_rj_aspl, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "aspl")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_aspl, nw_data_avg_aspl, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg",
                        nw_metric = "aspl")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_aspl, 
                                     (nw_data_fa_aspl + nw_data_rj_aspl) / 2,
                                     method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg_2",
                        nw_metric = "aspl")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_fa_aspl, nw_data_rj_aspl, method = "spearman", use = "na.or.complete"),
                        comparison = "fa_rj",
                        nw_metric = "aspl"))

data_mod <- combined_data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_mod, nw_data_fa_mod, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "mod") %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mod, nw_data_rj_mod, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "mod")) %>%  
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mod, nw_data_avg_mod, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg",
                        nw_metric = "mod")) %>%   
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mod, 
                                     (nw_data_fa_mod + nw_data_rj_mod) / 2, 
                                     method = "spearman", use = "na.or.complete"),
                        comparison = "indy_avg_2",
                        nw_metric = "mod")) %>% 
  bind_rows(combined_data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_fa_mod, nw_data_rj_mod, method = "spearman", use = "na.or.complete"),
                        comparison = "fa_rj",
                        nw_metric = "mod"))

# combine data -----------------------------------------------------------------

summarized_data <- bind_rows(data_strength, data_edge_weight, data_mean_strength, data_cc, data_aspl, data_mod) %>% 
  mutate(number_of_responses = study_param_cue_repetition * 3)

saveRDS(summarized_data, "01_Data/09_Analysis_Results/analysis_results_summarized.rds")
