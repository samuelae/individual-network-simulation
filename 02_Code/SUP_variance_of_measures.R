# dependencies

library(tidyverse)
library(igraph)
library(patchwork)

# data_summary <- readRDS("01_Data/09_Analysis_Results/analysis_results_summarized.rds") %>% 
#   ungroup() %>%
#   mutate(nw_measure_group = if_else(nw_metric %in% c("edge_weight", "strength"), "within", "between"))

data <- readRDS( "01_Data/09_Analysis_Results/analysis_results.rds")

data %>% 
  names()

data %>% 
  count(study)

data %>% 
  select(param, subj, study, cue_set, 
         cue_set_type = study_param_breadth, 
         cue_set_size = study_param_cue_set_size,
         number_of_responses = study_param_responses,
         starts_with("nw_data_fa_"), starts_with("nw_data_rj_"), starts_with("nw_data_indy_")) %>% 
  group_by(param, subj, cue_set_type, cue_set_size, number_of_responses) %>% 
  summarize(fa_avg_strength_mean = mean(nw_data_fa_mean_strength),
            fa_avg_strength_var = sd(nw_data_fa_mean_strength),
            indy_avg_strength_mean = mean(nw_data_indy_mean_strength),
            indy_avg_strength_var = sd(nw_data_indy_mean_strength)) %>% 
  ungroup() %>% 
  group_by(cue_set_size, cue_set_type) %>% 
  summarize(fa_mean = mean(fa_avg_strength_mean),
            fa_var = mean(fa_avg_strength_var),
            indy_mean = mean(indy_avg_strength_mean),
            indy_var = mean(indy_avg_strength_var)) %>% 
  filter(cue_set_type == "broad") %>% 
  mutate(var_ratio = indy_var / fa_var)
  

0.000144 / 0.000473
0.0515 / 5.10  


rnorm(100, mean = 100, sd = 10) %>% var()
rnorm(100, mean = 100, sd = 10) %>% var()
