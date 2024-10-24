# dependencies

library(tidyverse)
library(igraph)

# data

data <- readRDS( "01_Data/09_Analysis_Results/analysis_results.rds")
data_summary <- readRDS("01_Data/09_Analysis_Results/analysis_results_summarized.rds") %>% 
  ungroup() %>%
  mutate(nw_measure_group = if_else(nw_metric %in% c("edge_weight", "strength"), "within", "between"))

# find relevant comparison data in summary

data_summary %>% 
  group_by(nw_measure_group, study_param_cue_set_size) %>% 
  summarize(var = var(cor_sp, na.rm = TRUE))



data %>% 
  names()


data %>% 
  select(param, subj, study, cue_set, 
         study_param_breadth, study_param_cue_set_size, study_param_responses,
         nw_data_fa_aspl, nw_data_rj_aspl, nw_data_indy_aspl) %>% 
  filter(study_param_breadth == "broad", study_param_responses != 3) %>% 
  group_by(study_param_cue_set_size, study_param_responses) %>% 
  summarize(var_fa = var(nw_data_fa_aspl, na.rm = TRUE),
            var_rj = var(nw_data_rj_aspl, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(var_fa, var_rj), names_to = "resp_type", values_to = "var") %>% 
  ggplot(aes(x = as_factor(study_param_cue_set_size), y = var)) +
  geom_jitter()

data %>% 
  select(param, subj, study, cue_set, 
         study_param_breadth, study_param_cue_set_size, study_param_responses,
         nw_data_fa_aspl, nw_data_rj_aspl, nw_data_indy_aspl) %>% 
  filter(study_param_breadth == "broad") %>% 
  pivot_longer(cols = starts_with("nw_data_"), names_to = "type", values_to = "aspl") %>% 
  filter(type != "nw_data_indy_aspl") %>% 
  ggplot(aes(x = as_factor(study_param_cue_set_size), y = aspl, color = type)) +
  geom_point() +
  facet_grid(rows = vars(-study_param_responses), scales = "free_y")

data %>% 
  select(param, subj, study, cue_set, 
         study_param_breadth, study_param_cue_set_size, study_param_responses,
         nw_data_fa_mean_strength, nw_data_rj_mean_strength, nw_data_indy_mean_strength) %>% 
  filter(study_param_breadth == "broad") %>% 
  pivot_longer(cols = starts_with("nw_data_"), names_to = "type", values_to = "ms") %>% 
  filter(type != "nw_data_indy_mean_strength") %>% 
  ggplot(aes(x = as_factor(study_param_cue_set_size), y = ms, color = type)) +
  geom_point() +
  facet_grid(rows = vars(-study_param_responses), scales = "free_y")

