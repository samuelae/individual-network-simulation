# dependencies -----------------------------------------------------------------

# packages
library(tidyverse)

# define parameters ------------------------------------------------------------

# study parameters
study_parameters <- expand_grid(breadth = c("narrow", "mixed", "broad"),
                                size = c(10, 100, 1000),
                                responses = c(1, 10, 100))

# read in result files ---------------------------------------------------------

source("02_Code/Functions/Data_IO/read_bias_analysis.R")

analysis_results_folder <- "00_Cold_Storage/08_Bias_Analysis_Results/"

combined_data <- read_bias_analysis(folder_path = analysis_results_folder,
                                    nw_params = readRDS("00_Cold_Storage/04_Individual_Networks/grid.rds"),
                                    study_params = study_parameters)

# add responses variable
combined_data <- combined_data %>% 
  mutate(study_param_responses = study_param_cue_repetition * 3)

combined_data$cue_set <- as_factor(combined_data$cue_set)

saveRDS(combined_data, "01_Data/XX_Bias_Analysis_Results/bias_analysis_results.rds")
