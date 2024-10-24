# dependencies -----------------------------------------------------------------

# packages
library(tidyverse)

# cue set size -----------------------------------------------------------------

# results <- readRDS("01_Data/09_Analysis_Results/analysis_results.rds")
summarized_data <- readRDS("01_Data/09_Analysis_Results/analysis_results_summarized.rds")

data <- summarized_data %>% 
  group_by(study_param_cue_set_size, number_of_responses, study_param_breadth, 
           comparison, nw_metric) %>% 
  summarise(cor_sp = mean(cor_sp, na.rm = TRUE)) %>% 
  select(cue_set_size = study_param_cue_set_size,
         responses = number_of_responses,
         breadth = study_param_breadth,
         data_type = comparison, 
         nw_param = nw_metric, 
         cor = cor_sp) %>% 
  filter(data_type == "indy_fa" | data_type == "indy_rj") %>% 
  mutate(nw_param_group = if_else((nw_param == "edge_weight" | nw_param == "strength"), "cue-level", "network-level"))

# set to 2 significant digits rounding 
options(pillar.sigfig = 2)

data %>% 
  group_by(cue_set_size) %>% 
  summarize(cor = mean(cor, na.rm = TRUE))

data %>% 
  group_by(nw_param_group, cue_set_size) %>% 
  summarize(cor = mean(cor, na.rm = TRUE))

# cue set type -----------------------------------------------------------------

data %>% 
  group_by(breadth) %>% 
  summarize(cor = mean(cor, na.rm = TRUE))

# local vs. global recovery ----------------------------------------------------

# indyNet properties
indyNet_properties <- readRDS("01_Data/05_indyNets/indyNet_properties_full.RDS")

# inference properties
inference_properties <- readRDS("01_Data/09_Analysis_Results/analysis_results.rds") %>% 
  select(param, subj, cue_set, study_param_cue_set_size, study_param_cue_repetition,
         study_param_breadth, nw_data_indy_n_nodes:study_param_responses) 

comparison_data <- inference_properties %>% 
  left_join(indyNet_properties, by = c("param", "subj"))

comparison_results <- comparison_data %>% 
  group_by(cue_set, study_param_cue_set_size, study_param_cue_repetition,
           study_param_breadth) %>% 
  summarise(global.mean_strength.fa = cor(nw_data_fa_mean_strength, mean_strength, 
                                          use = "na.or.complete",
                                          method = "spearman"),
            global.mean_strength.rj = cor(nw_data_rj_mean_strength, mean_strength, 
                                          use = "na.or.complete",
                                          method = "spearman"),
            local.mean_strength.fa = cor(nw_data_fa_mean_strength, nw_data_indy_mean_strength, 
                                         use = "na.or.complete",
                                         method = "spearman"),
            local.mean_strength.rj = cor(nw_data_rj_mean_strength, nw_data_indy_mean_strength, 
                                         use = "na.or.complete",
                                         method = "spearman"),
            global.aspl.fa = cor(nw_data_fa_aspl, aspl, 
                                 use = "na.or.complete",
                                 method = "spearman"),
            global.aspl.rj = cor(nw_data_rj_aspl, aspl, 
                                 use = "na.or.complete",
                                 method = "spearman"),
            local.aspl.fa = cor(nw_data_fa_aspl, nw_data_indy_aspl, 
                                use = "na.or.complete",
                                method = "spearman"),
            local.aspl.rj = cor(nw_data_rj_aspl, nw_data_indy_aspl, 
                                use = "na.or.complete",
                                method = "spearman"),
            global.cc.fa = cor(nw_data_fa_cc, cc, 
                               use = "na.or.complete",
                               method = "spearman"),
            global.cc.rj = cor(nw_data_rj_cc, cc, 
                               use = "na.or.complete",
                               method = "spearman"),
            local.cc.fa = cor(nw_data_fa_cc, nw_data_indy_cc, 
                              use = "na.or.complete",
                              method = "spearman"),
            local.cc.rj = cor(nw_data_rj_cc, nw_data_indy_cc, 
                              use = "na.or.complete",
                              method = "spearman"),
            global.mod.fa = cor(nw_data_fa_mod, mod, 
                                use = "na.or.complete",
                                method = "spearman"),
            global.mod.rj = cor(nw_data_rj_mod, mod, 
                                use = "na.or.complete",
                                method = "spearman"),
            local.mod.fa = cor(nw_data_fa_mod, nw_data_indy_mod, 
                               use = "na.or.complete",
                               method = "spearman"),
            local.mod.rj = cor(nw_data_rj_mod, nw_data_indy_mod, 
                               use = "na.or.complete",
                               method = "spearman"))

# make data long for ploting and seperate data type and property
comparison_data_long <- comparison_results %>% 
  pivot_longer(cols = global.mean_strength.fa:local.mod.rj, names_to = "property", values_to = "cor") %>% 
  separate_wider_delim(property, delim = ".", 
                       names = c("comparison", "property", "data_type"))

comparison_data_long %>% 
  group_by(comparison)  %>% 
  summarize(cor = mean(cor, na.rm = TRUE))

# marginal recoveries for number of responses ----------------------------------

data %>% 
  group_by(responses) %>% 
  summarize(cor = mean(cor, na.rm = TRUE))

# behavioral data type ---------------------------------------------------------

data %>% 
  group_by(data_type) %>% 
  summarize(cor = mean(cor, na.rm = TRUE))

# fa - rj cor -----------------------------------------------------------

cor_data <- summarized_data %>% 
  group_by(study_param_cue_set_size, number_of_responses, study_param_breadth, 
           comparison, nw_metric) %>% 
  summarise(cor_sp = mean(cor_sp, na.rm = TRUE)) %>% 
  select(cue_set_size = study_param_cue_set_size,
         responses = number_of_responses,
         breadth = study_param_breadth,
         data_type = comparison, 
         nw_param = nw_metric, 
         cor = cor_sp) %>% 
  filter(data_type == "fa_rj") %>% 
  mutate(nw_param_group = if_else((nw_param == "edge_weight" | nw_param == "strength"), "cue-level", "network-level"))

cor_data %>% 
  ungroup() %>% 
  summarize(cor = mean(cor, na.rm = TRUE))

