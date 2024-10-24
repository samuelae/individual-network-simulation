# dependencies -----------------------------------------------------------------

# packages
library(tidyverse)
library(igraph)
library(parallel)
library(patchwork)

# functions
source("02_Code/Functions/Behavioral_Data/make_probability_matrix.R")
source("02_Code/Functions/Behavioral_Data/generate_free_associations.R")
source("02_Code/Functions/Behavioral_Data/p_r1_c_evaluation.R")
source("02_Code/Functions/Behavioral_Data/get_ranks.R")
source("02_Code/Functions/Network/edge_weight_threshold.R")

# import data from filesystem --------------------------------------------------

base_graph_0.2 <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_0.2.rds")

subtlex <- read_table("00_Cold_Storage/01_Source/SUBTLEX-US.txt") %>% 
  rename(frequency = FREQcount) %>% 
  mutate(word = tolower(Word)) %>% 
  filter(word %in% names(V(base_graph_0.2))) %>% 
  mutate(probability = frequency/sum(frequency)) %>% 
  select(word, probability)

swow <- read_csv("00_Cold_Storage/01_Source/SWOW-EN.R100.csv") %>% 
  select(cue, R1, R2, R3) %>% 
  pivot_longer(cols = c(R1, R2, R3), names_to = "position", values_to = "response") %>% 
  filter(!is.na(response))

# tune free association --------------------------------------------------------

swow_first_response_counts <- swow %>% 
  filter(response %in% names(V(base_graph_0.2))) %>% 
  filter(cue %in% names(V(base_graph_0.2))) %>% 
  filter(position == "R1") %>% 
  group_by(cue) %>% 
  count(response) %>% 
  arrange(cue, desc(n))

swow_cues <- swow_first_response_counts %>% pull(cue) %>% unique()

sensitivities_representation <- c(5, 7.5, 10, 12.5, 15, 17.5)
sensitivities_frequency <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5)
sens_grid <- expand_grid(sensitivities_representation, sensitivities_frequency)

run_free_assoc <- function(sensitivities_representation,
                           sensitivities_frequency) {
  prob_mat <- make_probability_matrix(semantic_representation = base_graph_0.2, 
                                      sensitivity_representation = sensitivities_representation,
                                      word_frequency = subtlex, 
                                      sensitivity_frequency = sensitivities_frequency)
  
  prediction_model <- generate_free_associations(probability_matrix = prob_mat,
                                                 cue_set = swow_cues[swow_cues %in% rownames(prob_mat)], 
                                                 n_responses = 3, 
                                                 n_repetitions = 1,
                                                 n_subjects = 300) 

  saveRDS(prediction_model, 
          paste0("00_Cold_Storage/03_Free_Association/tuning_models/prediction_model_", 
                 sensitivities_representation, 
                 "_",
                 sensitivities_frequency,
                 ".rds"))
  
  model_first_response_counts <- prediction_model %>% 
    pivot_longer(cols = c(resp_1, resp_2, resp_3), 
                 names_to = "position", 
                 values_to = "response") %>% 
    filter(position == "resp_1") %>% 
    group_by(cue) %>% 
    count(response) %>% 
    arrange(cue, desc(n))
  
  cor_pearson <- p_r1_c_evaluation(swow_first_response_counts = swow_first_response_counts,
                                   model_first_response_counts = model_first_response_counts,
                                   cor_method = "pearson",
                                   usable_words = rownames(prob_mat))
  cor_spearman <- p_r1_c_evaluation(swow_first_response_counts = swow_first_response_counts,
                                    model_first_response_counts = model_first_response_counts,
                                    cor_method = "spearman",
                                    usable_words = rownames(prob_mat))
  
  model_cues <- model_first_response_counts %>% 
    pull(cue) %>% 
    unique()
  all_ranks <- sapply(model_cues, get_ranks, swow_first_response_counts, model_first_response_counts)
  median_ranks <- apply(all_ranks, 1, median, na.rm = TRUE)
  
  results <- list(sens_rep = sensitivities_representation,
                  sens_freq = sensitivities_frequency,
                  cor_spearman = cor_spearman,
                  cor_pearson = cor_pearson,
                  median_ranks = median_ranks)
  
  saveRDS(results, 
          paste0("00_Cold_Storage/03_Free_Association/tuning_results/results_", 
                 sensitivities_representation, 
                 "_",
                 sensitivities_frequency,
                 ".rds"))
  
}

# parallel version
run_free_assoc_wrapper <- function(x, sens_grid) {
  run_free_assoc(sensitivities_representation = sens_grid$sensitivities_representation[x], 
                 sensitivities_frequency = sens_grid$sensitivities_frequency[x])
}

cl <- makeForkCluster(nnodes = 9) # 20GB RAM each
output <- clusterApply(cl = cl,
                       1:nrow(sens_grid),
                       run_free_assoc_wrapper,
                       sens_grid = sens_grid)
stopCluster(cl)
gc()

files <- dir(path = "00_Cold_Storage/03_Free_Association/tuning_results", full.names = TRUE)

results <- tibble(sensitivities_representation = rep(NA, times = length(files)),
                  sensitivities_frequency = rep(NA, times = length(files)),
                  cor_pearson = rep(NA, times = length(files)),
                  cor_spearman = rep(NA, times = length(files)),
                  rank_r1 = rep(NA, times = length(files)),
                  rank_r2 = rep(NA, times = length(files)),
                  rank_r3 = rep(NA, times = length(files)))

for(i in 1:length(files)) {
  this_result <- readRDS(files[i])
  results$sensitivities_representation[i] = this_result$sens_rep
  results$sensitivities_frequency[i] = this_result$sens_freq
  results$cor_pearson[i] <- this_result$cor_pearson
  results$cor_spearman[i] <- this_result$cor_spearman
  results$rank_r1[i] <- this_result$median_ranks[1]
  results$rank_r2[i] <- this_result$median_ranks[2]
  results$rank_r3[i] <- this_result$median_ranks[3]
}

saveRDS(results, "01_Data/03_Free_Association/tuning_grid_results.rds")
results <- readRDS("01_Data/03_Free_Association/tuning_grid_results.rds")

# cut offs ---------------------------------------------------------------------

working_graph <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_fully_connected.rds")

# test recovery of SWOW-EN data with different edge weight cut offs
cut_offs <- seq(from = 0.05, to = 0.3, by = 0.05)
results_co <- numeric(length = length(cut_offs))

for (i in 1:length(cut_offs)) {
  tictoc::tic()
  working_graph <- edge_weight_threshold(working_graph, cut_offs[i])
  
  prob_mat <- make_probability_matrix(semantic_representation = working_graph, 
                                      sensitivity_representation = 10,
                                      word_frequency = subtlex, 
                                      sensitivity_frequency = 1)
  
  prediction_model <- generate_free_associations(probability_matrix = prob_mat,
                                                 cue_set = swow_cues[swow_cues %in% rownames(prob_mat)], 
                                                 n_responses = 3, 
                                                 n_repetitions = 1,
                                                 n_subjects = 300) 
  
  model_first_response_counts <- prediction_model %>% 
    pivot_longer(cols = c(resp_1, resp_2, resp_3), 
                 names_to = "position", 
                 values_to = "response") %>% 
    filter(position == "resp_1") %>% 
    group_by(cue) %>% 
    count(response) %>% 
    arrange(cue, desc(n))
  
  cor_pearson <- p_r1_c_evaluation(swow_first_response_counts = swow_first_response_counts,
                                   model_first_response_counts = model_first_response_counts,
                                   cor_method = "pearson",
                                   usable_words = rownames(prob_mat))
  
  results_co[i] <- cor_pearson
  tictoc::toc()
  print(paste("cut off", cut_offs[i], "done"))
  
}

results_co <- c(readRDS("01_Data/03_Free_Association/cut_off_partial/cut_off_0.05.rds"),
                readRDS("01_Data/03_Free_Association/cut_off_partial/cut_off_0.1.rds"),
                readRDS("01_Data/03_Free_Association/cut_off_partial/cut_off_0.15.rds"),
                readRDS("01_Data/03_Free_Association/cut_off_partial/cut_off_0.2.rds"),
                readRDS("01_Data/03_Free_Association/cut_off_partial/cut_off_0.25.rds"),
                readRDS("01_Data/03_Free_Association/cut_off_partial/cut_off_0.3.rds"))

cut_off_correlations <- tibble(cut_off = cut_offs,
                               r = results_co)

write_csv(cut_off_correlations, "01_Data/03_Free_Association/cut_off_correlations.csv")


# plotting ---------------------------------------------------------------------

p_cut_offs <- cut_off_correlations %>% 
  mutate(col = c("0", "0", "0", "1", "0", "0")) %>% 
  ggplot(aes(x = cut_off, y = r, fill = col)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("grey30", "darkred")) +
  geom_text(aes(label = round(r, 3)), nudge_y = -0.03, color = "white") +
  scale_x_continuous(n.breaks = 6) +
  theme_minimal() +
  ylab(expression(r[Pearson])) +
  xlab("Cut off") +
  labs(title = "FA model correlation to SWOW", 
       subtitle = "Varying ground truth network edge weight cut off")

p_pearson <- ggplot(results, 
       aes(x = sensitivities_representation, y = sensitivities_frequency)) +
  geom_tile(aes(fill = cor_pearson)) +
  geom_text(aes(label = round(cor_pearson, 2),
                color = cor_pearson < (min(cor_pearson) + ((max(cor_pearson) - min(cor_pearson)) / 2)))) +
  scale_color_manual(guide = "none", values = c("black", "white")) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "FA model correlation to SWOW", subtitle = "Pearson correlation") +
  xlab(expression(gamma[w])) +
  ylab(expression(gamma[f])) +
  scale_x_continuous(n.breaks = 6, 
                     breaks = c(5, 7.5, 10, 12.5, 15, 17.5),
                     minor_breaks = NULL) +
  scale_y_continuous(n.breaks = 6, 
                     breaks = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     minor_breaks = NULL) +
  theme(legend.position = "none")
p_spearman <- ggplot(results, 
                    aes(x = sensitivities_representation, y = sensitivities_frequency)) +
  geom_tile(aes(fill = cor_spearman)) +
  geom_text(aes(label = round(cor_spearman, 2),
                color = cor_spearman < (min(cor_spearman) + ((max(cor_spearman) - min(cor_spearman)) / 2)))) +
  scale_color_manual(guide = "none", values = c("black", "white")) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "FA model correlation to SWOW", subtitle = "Spearman rank correlation") +
  xlab(expression(gamma[w])) +
  ylab(expression(gamma[f])) +  
  scale_x_continuous(n.breaks = 6, 
                     breaks = c(5, 7.5, 10, 12.5, 15, 17.5),
                     minor_breaks = NULL) +
  scale_y_continuous(n.breaks = 6, 
                     breaks = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     minor_breaks = NULL) +
  theme(legend.position = "none")
p_pearson + p_spearman
ggsave("03_Plots/01_Tuning_Free_Association/free_association_correlations.pdf", width = 32, height = 16, units = "cm")

p_r1 <- ggplot(results, 
               aes(x = sensitivities_representation, y = sensitivities_frequency)) +
  geom_tile(aes(fill = rank_r1)) +
  geom_text(aes(label = rank_r1,
                color = rank_r1 < (min(rank_r1) + ((max(rank_r1) - min(rank_r1)) / 2)))) +
  scale_color_manual(guide = "none", values = c("white", "black")) +
  theme_minimal() +
  scale_fill_viridis_c(direction = -1) +
  labs(title = "Median SWOW rank 1st response") +
  xlab(expression(gamma[w])) +
  ylab(expression(gamma[f])) +  
  scale_x_continuous(n.breaks = 6, 
                     breaks = c(5, 7.5, 10, 12.5, 15, 17.5),
                     minor_breaks = NULL) +
  scale_y_continuous(n.breaks = 6, 
                     breaks = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     minor_breaks = NULL) +
  theme(legend.position = "none")
p_r2 <- ggplot(results, 
               aes(x = sensitivities_representation, y = sensitivities_frequency)) +
  geom_tile(aes(fill = rank_r2)) +
  geom_text(aes(label = rank_r2,
                color = rank_r2 < (min(rank_r2) + ((max(rank_r2) - min(rank_r2)) / 2)))) +
  scale_color_manual(guide = "none", values = c("white", "black")) +
  theme_minimal() +
  scale_fill_viridis_c(direction = -1) +
  labs(title = "Median SWOW rank 2nd response") +
  xlab(expression(gamma[w])) +
  ylab(expression(gamma[f])) +  
  scale_x_continuous(n.breaks = 6, 
                     breaks = c(5, 7.5, 10, 12.5, 15, 17.5),
                     minor_breaks = NULL) +
  scale_y_continuous(n.breaks = 6, 
                     breaks = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     minor_breaks = NULL) +
  theme(legend.position = "none")
p_r3 <- ggplot(results, 
               aes(x = sensitivities_representation, y = sensitivities_frequency)) +
  geom_tile(aes(fill = rank_r3)) +
  geom_text(aes(label = rank_r3,
                color = rank_r3 < (min(rank_r3) + ((max(rank_r3) - min(rank_r3)) / 2)))) +
  scale_color_manual(guide = "none", values = c("white", "black")) +
  theme_minimal() +
  scale_fill_viridis_c(direction = -1) +
  labs(title = "Median SWOW rank 3rd response") +
  xlab(expression(gamma[w])) +
  ylab(expression(gamma[f])) +  
  scale_x_continuous(n.breaks = 6, 
                     breaks = c(5, 7.5, 10, 12.5, 15, 17.5),
                     minor_breaks = NULL) +
  scale_y_continuous(n.breaks = 6, 
                     breaks = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     minor_breaks = NULL) +
  theme(legend.position = "none")
p_r1 + p_r2 + p_r3
ggsave("03_Plots/01_Tuning_Free_Association/free_association_ranks.pdf", width = 34, height = 14, units = "cm")

# overview plot
(p_pearson + p_spearman + p_cut_offs)  /
((p_r1 + theme(plot.margin = unit(c(15, 0, 0, 0), "mm"))) + p_r2 + p_r3) +
  plot_annotation(tag_levels = 'A')
ggsave("03_Plots/01_Tuning_Free_Association/free_association_tuning_overview.pdf", width = 32, height = 24, units = "cm")
