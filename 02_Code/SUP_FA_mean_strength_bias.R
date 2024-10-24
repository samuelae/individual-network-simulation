# dependencies

library(tidyverse)
# library(igraph)

source("02_Code/Functions/Helpers/bias.R")

# data

data <- readRDS( "01_Data/09_Analysis_Results/analysis_results.rds")



data %>% names()

# filter out the mean strength FA data (and to compare RJ)

subset <- data %>% 
  filter(study_param_breadth == "broad",
         study_param_cue_set_size == 100,
         study_param_responses == 30)

subset %>% 
  select(nw_data_indy_mean_strength,
         nw_data_fa_mean_strength,
         nw_data_rj_mean_strength)

# nw-properties
subset %>% 
  select(nw_data_indy_n_nodes, nw_data_indy_n_edges,
         nw_data_fa_n_nodes, nw_data_fa_n_edges,
         nw_data_rj_n_nodes, nw_data_rj_n_edges) %>% 
  pivot_longer(cols = nw_data_fa_n_nodes:nw_data_rj_n_edges,
               names_to = c("nw_type", ".value"),
               names_pattern = "(.*)_n_(.*)") %>% 
  mutate(nw_type = str_sub(nw_type, 9)) %>% 
  ggplot(aes(x = nw_data_indy_n_edges, y = edges, colour = nw_type)) +
  geom_point()

