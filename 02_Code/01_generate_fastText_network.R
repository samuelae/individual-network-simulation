# dependencies -----------------------------------------------------------------

# packages
library(tidyverse)

# functions
source("02_Code/Functions/Data_IO/read_vectors.R")
Rcpp::sourceCpp("02_Code/Functions/Vectors/arma_cosine.cpp")
source("02_Code/Functions/Vectors/cosine.R")
source("02_Code/Functions/Network/edge_weight_threshold.R")

# load and prepare SWOW data ---------------------------------------------------

# SWOW-EN cues
swow_en <- read_csv("00_Cold_Storage/01_Source/SWOW-EN.R100.csv") 

swow_en_cues <- swow_en %>% 
  pull(cue) %>% 
  unique()

# SWOW-EN responses
swow_en_responses <- swow_en %>% 
  pivot_longer(c("R1", "R2", "R3"), 
               names_to = "position", 
               values_to = "response") %>% 
  select(response) %>% 
  count(response)

# find words with min 30 occurrences (to keep overal size of network "reasonable")
swow_words_30 <- unique(c(swow_en_cues, 
                          swow_en_responses %>% filter(n >= 30) %>% pull(response)))

# extract fastText data from source file ---------------------------------------

# fastText
fastText <- read_vectors(file = "00_Cold_Storage/01_Source/crawl-300d-2M.vec",
                         words = swow_words_30)

# extract fasText vectors based on casing
fT_names <- rownames(fastText)
fT_names_lower <- tolower(fT_names)

# vectors for all lowercase forms (min 30 occurrences)
fT_lower_30 <- fastText[which(fT_names == fT_names_lower), ]

# generate fully connected cosine network --------------------------------------

cos_mat <- cosine(fT_lower_30)

graph <- igraph::graph_from_adjacency_matrix(cos_mat,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE,
                                             add.colnames = NULL)

saveRDS(graph, "00_Cold_Storage/02_fastText_Network/base_graph_fully_connected.rds")

# generate network with cosine cut off at 0.2 ----------------------------------

base_graph_0.2 <- edge_weight_threshold(graph, 0.2)

saveRDS(base_graph_0.2, "00_Cold_Storage/02_fastText_Network/base_graph_0.2.rds")



