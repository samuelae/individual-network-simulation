# dependencies -----------------------------------------------------------------

library(tidyverse)
library(patchwork)

# data and paths ---------------------------------------------------------------

# data
data <- readRDS("01_Data/09_Analysis_Results/analysis_results.rds")

# set cc = 1 to NA
data <- data %>% 
  mutate(nw_data_fa_cc = na_if(nw_data_fa_cc, 1),
         nw_data_rj_cc = na_if(nw_data_rj_cc, 1))

# add responses variable
data <- data %>% 
  mutate(study_param_responses = study_param_cue_repetition * 3)

data$cue_set <- as_factor(data$cue_set)

# summarise data for tileplots -------------------------------------------------

data_strength <- data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = mean(importance_cor_indy_fa, na.rm = TRUE),
            comparison = "indy_fa",
            nw_metric = "strength") %>% 
  bind_rows(data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(importance_cor_indy_rj, na.rm = TRUE),
                        comparison = "indy_rj",
                        nw_metric = "strength"))

data_edge_weight <- data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = mean(relatedness_cor_indy_fa, na.rm = TRUE),
            comparison = "indy_fa",
            nw_metric = "edge_weight") %>% 
  bind_rows(data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = mean(relatedness_cor_indy_rj, na.rm = TRUE),
                        comparison = "indy_rj",
                        nw_metric = "edge_weight"))

# data between networks --------------------------------------------------------

data_mean_strength <- data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_mean_strength, nw_data_fa_mean_strength, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "mean_strength") %>% 
  bind_rows(data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mean_strength, nw_data_rj_mean_strength, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "mean_strength"))

data_cc <- data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_cc, nw_data_fa_cc, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "cc") %>% 
  bind_rows(data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_cc, nw_data_rj_cc, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "cc"))

data_aspl <- data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_aspl, nw_data_fa_aspl, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "aspl") %>% 
  bind_rows(data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_aspl, nw_data_rj_aspl, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "aspl"))

data_mod <- data %>% 
  group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
  summarise(cor_sp = cor(nw_data_indy_mod, nw_data_fa_mod, method = "spearman", use = "na.or.complete"),
            comparison = "indy_fa",
            nw_metric = "mod") %>% 
  bind_rows(data %>% 
              group_by(study_param_cue_set_size, study_param_cue_repetition, study_param_breadth, cue_set) %>% 
              summarise(cor_sp = cor(nw_data_indy_mod, nw_data_rj_mod, method = "spearman", use = "na.or.complete"),
                        comparison = "indy_rj",
                        nw_metric = "mod"))

# combine data -----------------------------------------------------------------

plot_data <- bind_rows(data_strength, data_edge_weight, data_mean_strength, data_cc, data_aspl, data_mod) %>% 
  mutate(number_of_responses = study_param_cue_repetition * 3)

# summarise over all 10 cue sets
plot_data <- plot_data %>% 
  group_by(study_param_cue_set_size, number_of_responses, study_param_breadth, comparison, nw_metric) %>% 
  summarise(cor_sp = mean(cor_sp, na.rm = TRUE))

# improve casing for figure
plot_data <- plot_data %>% 
  mutate(study_param_breadth = case_when(
    study_param_breadth == "broad" ~ "Broad",
    study_param_breadth == "mixed" ~ "Mixed",
    study_param_breadth == "narrow" ~ "Narrow"
  ))

# plotting set up --------------------------------------------------------------

# continuous colors
make_tileplot <- function(data, 
                          metric = c("strength", "edge_weight", "mean_strength",
                                     "cc", "aspl", "mod"), 
                          type = c("indy_fa", "indy_rj"),
                          limits = c(-0.8, 1)) {
  
  plot <- data %>% 
    filter(nw_metric == metric) %>% 
    filter(comparison == type) %>% 
    ggplot(aes(x = as_factor(study_param_cue_set_size),
               y = as_factor(number_of_responses))) +
    geom_tile(aes(fill = cor_sp)) +
    geom_text(aes(label = round(cor_sp, 2),
                  color = cor_sp < (limits[1] + ((limits[2] - limits[1]) / 2))),
              size = 6) +
    scale_color_manual(guide = "none", values = c("FALSE" = "black", "TRUE" = "white")) +
    theme_minimal() +
    scale_fill_viridis_c(limits = limits) +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", 
                                          color = NA),
          axis.title = element_blank()) +
    facet_wrap(~study_param_breadth)
  
  plot
  
}

# binned colors
make_tileplot_d <- function(data, 
                            metric = c("strength", "edge_weight", "mean_strength",
                                       "cc", "aspl", "mod"), 
                            type = c("indy_fa", "indy_rj"),
                            limits = c(-0.01, 0.5)) {
  
  plot <- data %>% 
    filter(nw_metric == metric) %>% 
    filter(comparison == type) %>% 
    mutate(color_value = case_when(cor_sp < limits[1] ~ "negative",
                                   cor_sp >= limits[1] & cor_sp < limits[2] ~ "positive",
                                   cor_sp >= limits[2] ~ "good")) %>% 
    ggplot(aes(x = as_factor(study_param_cue_set_size),
               y = as_factor(number_of_responses))) +
    geom_tile(aes(fill = color_value)) +
    geom_text(aes(label = round(cor_sp, 2),
                  color = (cor_sp <= limits[2])),
              size = 6) +
    scale_color_manual(guide = "none", values = c("FALSE" = "black", "TRUE" = "white")) +
    scale_fill_manual(values = c("negative" = "#440154FF", "positive" = "#21908CFF", "good" = "#FDE725FF")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", 
                                          color = NA),
          axis.title = element_blank()) +
    facet_wrap(~study_param_breadth)
  
  plot
  
}

# make detailed plot -----------------------------------------------------------

# within network
importance_fa <- make_tileplot(plot_data, "strength", "indy_fa")
importance_rj <- make_tileplot(plot_data, "strength", "indy_rj")
relatedness_fa <- make_tileplot(plot_data, "edge_weight", "indy_fa")
relatedness_rj <- make_tileplot(plot_data, "edge_weight", "indy_rj")

within_nw <-  (relatedness_fa + relatedness_rj) /
  (importance_fa + importance_rj)

ggsave("03_Plots/Resolution/resolution_detailed_within.pdf", within_nw,
       width = 20, height = 8, units = "cm", scale = 2)

# between network
ms_fa <- make_tileplot(plot_data, "mean_strength", "indy_fa")
ms_rj <- make_tileplot(plot_data, "mean_strength", "indy_rj")
aspl_fa <- make_tileplot(plot_data, "aspl", "indy_fa")
aspl_rj <- make_tileplot(plot_data, "aspl", "indy_rj")
cc_fa <- make_tileplot(plot_data, "cc", "indy_fa")
cc_rj <- make_tileplot(plot_data, "cc", "indy_rj")
mod_fa <- make_tileplot(plot_data, "mod", "indy_fa")
mod_rj <- make_tileplot(plot_data, "mod", "indy_rj")

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj) 

ggsave("03_Plots/Resolution/resolution_detailed_between.pdf", between_nw,
       width = 20, height = 16, units = "cm", scale = 2)

# make binned plot -----------------------------------------------------------

# within network
importance_fa <- make_tileplot_d(plot_data, "strength", "indy_fa")
importance_rj <- make_tileplot_d(plot_data, "strength", "indy_rj")
relatedness_fa <- make_tileplot_d(plot_data, "edge_weight", "indy_fa")
relatedness_rj <- make_tileplot_d(plot_data, "edge_weight", "indy_rj")

within_nw <-  (relatedness_fa + relatedness_rj) /
  (importance_fa + importance_rj)

ggsave("03_Plots/Resolution/resolution_binned_within.pdf", within_nw,
       width = 20, height = 8, units = "cm", scale = 2)

# between network
ms_fa <- make_tileplot_d(plot_data, "mean_strength", "indy_fa")
ms_rj <- make_tileplot_d(plot_data, "mean_strength", "indy_rj")
aspl_fa <- make_tileplot_d(plot_data, "aspl", "indy_fa")
aspl_rj <- make_tileplot_d(plot_data, "aspl", "indy_rj")
cc_fa <- make_tileplot_d(plot_data, "cc", "indy_fa")
cc_rj <- make_tileplot_d(plot_data, "cc", "indy_rj")
mod_fa <- make_tileplot_d(plot_data, "mod", "indy_fa")
mod_rj <- make_tileplot_d(plot_data, "mod", "indy_rj")

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj) 

ggsave("03_Plots/Resolution/resolution_binned_between.pdf", between_nw,
       width = 20, height = 16, units = "cm", scale = 2)

# EXTRACT NUMBERS FOR RESULTS SECTION ------------------------------------------

options(pillar.sigfig = 5)

results_data <- plot_data %>% 
  ungroup() %>% 
  mutate(resolution_level = case_when(cor_sp >= 0.5 ~ "good",
                                      cor_sp < 0.5 & cor_sp > -0.01 ~ "poor",
                                      cor_sp <= -0.01 ~ "negative"))

# overall levels of resolution
results_data %>% 
  group_by(resolution_level) %>% 
  summarize(n = n(),
            percent = n() / 324 * 100)
  
# proportion of good resolution per measure
results_data %>% 
  group_by(resolution_level, nw_metric) %>% 
  summarize(n = n(),
            percent = n() / 54 * 100) %>% 
  filter(resolution_level == "good") %>% 
  arrange(desc(percent))

# proportion of good resolution per design factor: response type
results_data %>% 
  group_by(resolution_level, comparison) %>% 
  summarize(n = n(),
            percent = n() / 162 * 100) %>% 
  filter(resolution_level == "good") %>% 
  arrange(desc(percent))

# proportion of good resolution per design factor: number of responses
results_data %>% 
  group_by(resolution_level, number_of_responses) %>% 
  summarize(n = n(),
            percent = n() / 108 * 100) %>% 
  filter(resolution_level == "good") %>% 
  arrange(desc(percent))

# proportion of good resolution per design factor: cue set size
results_data %>% 
  group_by(resolution_level, study_param_cue_set_size) %>% 
  summarize(n = n(),
            percent = n() / 108 * 100) %>% 
  filter(resolution_level == "good") %>% 
  arrange(desc(percent))

# proportion of good resolution per design factor: cue set type
results_data %>% 
  group_by(resolution_level, study_param_breadth) %>% 
  summarize(n = n(),
            percent = n() / 108 * 100) %>% 
  filter(resolution_level == "good") %>% 
  arrange(desc(percent))

# EXTRACT NUMBERS FOR DISCUSSION -----------------------------------------------

plot_data %>%
  filter(!(nw_metric %in% c("edge_weight", "strength"))) %>% 
  group_by(study_param_cue_set_size, number_of_responses) %>%
  summarize(r = mean(cor_sp))



# # rel by type
# results_data %>% group_by(comparison) %>% summarize(mean(cor_sp, na.rm = T))
# 
# # rel by measure
# results_data %>% group_by(nw_metric) %>% summarize(m = mean(cor_sp, na.rm = T)) %>% arrange(desc(m))
# 
# # variance by design factor
# results_data %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(m = mean(cor_sp, na.rm = T)) %>% 
#   summarize(sd = sd(m))
# 
# results_data %>% 
#   group_by(number_of_responses) %>% 
#   summarize(m = mean(cor_sp, na.rm = T)) %>% 
#   summarize(sd = sd(m))
# 
# results_data %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(m = mean(cor_sp, na.rm = T)) %>% 
#   summarize(sd = sd(m))
# 
# results_data %>% 
#   group_by(comparison) %>% 
#   summarize(m = mean(cor_sp, na.rm = T)) %>% 
#   summarize(sd = sd(m))
# 
# # number of responses
# results_data %>% 
#   group_by(number_of_responses) %>% 
#   summarize(m = mean(cor_sp, na.rm = T))
# 
# # cue set size
# results_data %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(m = mean(cor_sp, na.rm = T))
# 
# # cue set type
# results_data %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(m = mean(cor_sp, na.rm = T))
# 
# # within-nw behavioral data vs. between-nw behavioral data
# results_data %>% 
#   filter(nw_metric %in% c("strength", "edge_weight")) %>% 
#   group_by(comparison) %>% 
#   summarize(m = mean(cor_sp, na.rm = T))
# 
# results_data %>% 
#   filter(nw_metric %in% c("mean_strength","cc", "aspl", "mod")) %>% 
#   group_by(comparison) %>% 
#   summarize(m = mean(cor_sp, na.rm = T))
# 
# # EXTRACT NUMBERS FOR DISCUSSION -----------------------------------------------
# 
# corrs <- plot_data %>% 
#   group_by(study_param_cue_set_size, number_of_responses) %>% 
#   summarize(cor = mean(cor_sp)) %>% 
#   pull(cor)
# corrs[5]/corrs[9]