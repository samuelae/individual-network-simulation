# dependencies -----------------------------------------------------------------

library(tidyverse)
library(igraph)

# define cue set needs from study parameters -----------------------------------

study_parameters <- expand_grid(breadth = c("narrow", "mixed", "broad"),
                                size = c(10, 100, 1000),
                                responses = c(1, 10, 100))

# extract cue set types 
cue_set_types <- study_parameters %>% 
  select(breadth, size) %>% 
  distinct()

# number of sets for each type
n_sets = 10
cue_sets <- expand_grid(cue_set_types, set_no = 1:n_sets)

# define source for cue sets ---------------------------------------------------

base_graph <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_0.2.rds")

# remove words not available in indyNets
subtlex_words <- read_table("00_Cold_Storage/01_Source/SUBTLEX-US.txt") %>%
  mutate(word = tolower(Word)) %>%
  pull(word)
base_graph <- delete_vertices(base_graph,
                              !vertex_attr(base_graph, "name") %in% subtlex_words)
base_graph_am <- as_adjacency_matrix(base_graph, type = "both", attr = "weight")

# get function to produce cue sets ---------------------------------------------

source("02_Code/Functions/Study_Simulation/generate_cue_set.R")

# generate cue sets (reproducible) ---------------------------------------------

library(furrr)

set.seed(1991)

plan(multisession, workers = 20) # runs in 20 min on 20 cores on sciCORE

result_sets <- future_map2(.y = cue_sets$breadth, 
                           .x = cue_sets$size, 
                           .f = generate_cue_set,
                           graph_am = base_graph_am,
                           .options = furrr_options(seed = TRUE,
                                                    chunk_size = 1))

cue_sets <- cue_sets %>% 
  bind_cols(tibble(set = result_sets))

saveRDS(cue_sets, "01_Data/06_Cue_Sets/cue_sets.rds")

