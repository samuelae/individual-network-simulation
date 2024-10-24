# dependencies -----------------------------------------------------------------

library(tidyverse)
library(patchwork)

source("02_Code/Functions/Helpers/bias.R")

# data -------------------------------------------------------------------------

# between network raw results
inference_properties <- readRDS("01_Data/09_Analysis_Results/analysis_results.rds") %>% 
  mutate(nw_data_fa_cc = na_if(nw_data_fa_cc, 1),
         nw_data_rj_cc = na_if(nw_data_rj_cc, 1)) %>% 
  select(param, subj, cue_set, study_param_cue_set_size, study_param_responses, study_param_breadth, 
         nw_data_indy_mean_strength, nw_data_indy_cc, nw_data_indy_aspl, nw_data_indy_mod,
         nw_data_fa_mean_strength, nw_data_fa_cc, nw_data_fa_aspl, nw_data_fa_mod,
         nw_data_rj_mean_strength, nw_data_rj_cc, nw_data_rj_aspl, nw_data_rj_mod)

# bias -------------------------------------------------------------------------

# calculate between-nw bias
bias_between <- inference_properties %>%
  group_by(study_param_cue_set_size, study_param_responses, study_param_breadth) %>% 
  summarize(bias.fa.ms = bias(nw_data_fa_mean_strength, nw_data_indy_mean_strength, na.rm = T),
            bias.rj.ms = bias(nw_data_rj_mean_strength, nw_data_indy_mean_strength, na.rm = T),
            bias.fa.cc = bias(nw_data_fa_cc, nw_data_indy_cc, na.rm = T),
            bias.rj.cc = bias(nw_data_rj_cc, nw_data_indy_cc, na.rm = T),
            bias.fa.aspl = bias(nw_data_fa_aspl, nw_data_indy_aspl, na.rm = T),
            bias.rj.aspl = bias(nw_data_rj_aspl, nw_data_indy_aspl, na.rm = T),
            bias.fa.mod = bias(nw_data_fa_mod, nw_data_indy_mod, na.rm = T),
            bias.rj.mod = bias(nw_data_rj_mod, nw_data_indy_mod, na.rm = T))

# pivot into long format and separate information into columns

plot_data_between <- bias_between %>%
  pivot_longer(cols = bias.fa.ms:bias.rj.mod) %>%
  separate_wider_delim(cols = name, delim = ".", names = c("summary_metric", "type", "metric"))


# improve casing for figure
plot_data <- plot_data_between %>% 
  mutate(study_param_breadth = case_when(
    study_param_breadth == "broad" ~ "Broad",
    study_param_breadth == "mixed" ~ "Mixed",
    study_param_breadth == "narrow" ~ "Narrow"
  ))

# plotting ---------------------------------------------------------------------

# custom plotting function
make_tileplot <- function(data, 
                          p_metric = c("strength", "edge_weight", "ms",
                                     "cc", "aspl", "mod"), 
                          p_type = c("fa", "rj"),
                          p_summary_metric = c("bias"),
                          p_limits = c(-1, 1)) {
  
  # log(bias+1) for color
  p_limits_color <- log(p_limits + 1.01)
  
  plot <- data %>% 
    filter(metric == p_metric) %>% 
    filter(type == p_type) %>% 
    filter(summary_metric == p_summary_metric) %>% 
    mutate(color_value = log(value + 1.01),
           label_value = if_else(abs(value) >= 1, signif(value, 3), round(value, 2))) %>% 
    ggplot(aes(x = as_factor(study_param_cue_set_size),
               y = as_factor(study_param_responses))) +
    geom_tile(aes(fill = color_value)) +
    geom_text(aes(label = label_value,
                  color = (color_value > (p_limits_color[1] / 2)) & (color_value < (p_limits_color[2] / 2))),
              size = 6) +
    scale_color_manual(guide = "none", values = c("FALSE" = "white", "TRUE" = "black")) +
    theme_minimal() +
    scale_fill_gradientn(colours = c("firebrick4", "grey95", "dodgerblue4"), 
                         values = scales::rescale(c(p_limits_color[1], 0, p_limits_color[2])),
                         guide = "colorbar", limits = p_limits_color) +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", 
                                          color = NA),
          axis.title = element_blank()) +
    facet_wrap(~study_param_breadth)
  
  plot
  
}

make_tileplot_d <- function(data, 
                            p_metric = c("strength", "edge_weight", "ms",
                                         "cc", "aspl", "mod"), 
                            p_type = c("fa", "rj"),
                            p_summary_metric = c("bias"),
                            p_limits = c(-0.3, 0.3)) {
  
  plot <- data %>% 
    filter(metric == p_metric) %>% 
    filter(type == p_type) %>% 
    filter(summary_metric == p_summary_metric) %>% 
    mutate(color_value = case_when(value < p_limits[1] ~ "negative",
                                   value >= p_limits[1] & value <= p_limits[2] ~ "good",
                                   value > p_limits[2] ~ "positive"),
           label_value = if_else(abs(value) >= 1, signif(value, 3), round(value, 2))) %>% 
    ggplot(aes(x = as_factor(study_param_cue_set_size),
               y = as_factor(study_param_responses))) +
    geom_tile(aes(fill = color_value)) +
    geom_text(aes(label = label_value,
                  color = ((value >= p_limits[1]) & (value <= p_limits[2]))),
              size = 6) +
    scale_color_manual(guide = "none", values = c("FALSE" = "white", "TRUE" = "black")) +
    theme_minimal() +
    scale_fill_manual(values = c("negative" = "firebrick4", "good" = "grey95", "positive" = "dodgerblue4")) +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", 
                                          color = NA),
          axis.title = element_blank()) +
    facet_wrap(~study_param_breadth)
  
  plot
  
}


# bias based plot --------------------------------------------------------------

summary_metric = "bias"

# detailed color
limits = c(-1, 0.87)
ms_fa <- make_tileplot(plot_data, "ms", "fa", summary_metric, limits)
ms_rj <- make_tileplot(plot_data, "ms", "rj", summary_metric, limits)
limits = c(-1, 77.5)
aspl_fa <- make_tileplot(plot_data, "aspl", "fa", summary_metric, limits)
aspl_rj <- make_tileplot(plot_data, "aspl", "rj", summary_metric, limits)
limits = c(-1, 1.3)
cc_fa <- make_tileplot(plot_data, "cc", "fa", summary_metric, limits)
cc_rj <- make_tileplot(plot_data, "cc", "rj", summary_metric, limits)
limits = c(-1, 9.55)
mod_fa <- make_tileplot(plot_data, "mod", "fa", summary_metric, limits)
mod_rj <- make_tileplot(plot_data, "mod", "rj", summary_metric, limits)

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj)

ggsave("03_Plots/Bias/bias_detailed.pdf", between_nw,
       width = 20, height = 16, units = "cm", scale = 2)

# binned color

ms_fa <- make_tileplot_d(plot_data, "ms", "fa", summary_metric)
ms_rj <- make_tileplot_d(plot_data, "ms", "rj", summary_metric)
aspl_fa <- make_tileplot_d(plot_data, "aspl", "fa", summary_metric)
aspl_rj <- make_tileplot_d(plot_data, "aspl", "rj", summary_metric)
cc_fa <- make_tileplot_d(plot_data, "cc", "fa", summary_metric)
cc_rj <- make_tileplot_d(plot_data, "cc", "rj", summary_metric)
mod_fa <- make_tileplot_d(plot_data, "mod", "fa", summary_metric)
mod_rj <- make_tileplot_d(plot_data, "mod", "rj", summary_metric)

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj)

ggsave("03_Plots/Bias/bias_binned.pdf", between_nw,
       width = 20, height = 16, units = "cm", scale = 2)

# EXTRACT VALUES FOR RESULTS SECTION -------------------------------------------

options(pillar.sigfig = 5)

results_data <- inference_properties %>% 
  pivot_longer(cols = starts_with("nw_data_"), names_to = "name", values_to = "values") %>% 
  separate_wider_delim(cols = name, delim = "_", names = c(NA, NA, "type", "metric"), too_many = "merge") %>% 
  pivot_wider(names_from = type, values_from = values)

results_data_plot <- results_data %>% 
  pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
  rename(cue_set_size = study_param_cue_set_size, 
           number_of_responses = study_param_responses, 
           cue_set_type = study_param_breadth, 
           network_measure = metric, 
           response_type = type) %>% 
  group_by(cue_set_size, number_of_responses, cue_set_type, response_type, network_measure) %>% 
  summarize(b = bias(indy, inference)) %>% 
  mutate(bias_label = case_when(b < -0.3 ~ "negative",
                                b >= -0.3 & b <= 0.3 ~ "good",
                                b > 0.3 ~ "positive")) %>% 
  ungroup()

# overall labels
results_data_plot %>% 
  group_by(bias_label) %>% 
  summarize(n = n(),
            percent = n() / nrow(results_data_plot) * 100)
perc <- results_data_plot %>% 
  group_by(bias_label) %>% 
  summarize(n = n(),
            percent = n() / nrow(results_data_plot) * 100) %>% 
  pull(percent)
perc
perc[2:3] %>% sum()

# per measure
results_data_plot %>% 
  group_by(network_measure, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 54 * 100) %>%
  filter(bias_label == "good") %>% 
  arrange(desc(percent))

# per design factor: response type
results_data_plot %>% 
  group_by(response_type, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 108 * 100) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent))

# per design factor: cue set type
results_data_plot %>% 
  group_by(cue_set_type, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent))

# per design factor: cue set size
results_data_plot %>% 
  group_by(cue_set_size, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent))

# per design factor: number of responses
results_data_plot %>% 
  group_by(number_of_responses, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent))



# # percentage of edges in FA inferences
# n_edges_data <- readRDS("01_Data/09_Analysis_Results/analysis_results.rds") %>% 
#   select(param, subj, cue_set, study_param_cue_set_size, study_param_responses, study_param_breadth, 
#          nw_data_indy_n_edges, nw_data_fa_n_edges)
# n_edges_data %>% 
#   mutate(n_edges_prop = nw_data_fa_n_edges / nw_data_indy_n_edges) %>% 
#   # group_by(study_param_responses, study_param_cue_set_size) %>% 
#   summarize(mean_n_edges_prop = mean(n_edges_prop))
# 
# # data for the rest of analyses
# results_data <- inference_properties %>% 
#   pivot_longer(cols = starts_with("nw_data_"), names_to = "name", values_to = "values") %>% 
#   separate_wider_delim(cols = name, delim = "_", names = c(NA, NA, "type", "metric"), too_many = "merge") %>% 
#   pivot_wider(names_from = type, values_from = values)
# 
# # overall bias
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   summarize(b = bias(inference, indy, na.rm = T))
#   
# # overall bias by measure
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(metric) %>% 
#   summarize(b = bias(inference, indy, na.rm = T)) %>% 
#   mutate(b_dev = abs(b) - 0) %>% 
#   arrange(b_dev)
# 
# # bias b and sd by study design dimension
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(b = bias(inference, indy, na.rm = T)) %>% 
#   summarize(cue_set_size_sd = sd(b))
# 
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_responses) %>% 
#   summarize(b = bias(inference, indy, na.rm = T)) %>% 
#   summarize(responses_sd = sd(b))
# 
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(b = bias(inference, indy, na.rm = T)) %>% 
#   summarize(set_type_sd = sd(b))
# 
# results_data %>% 
#   summarize(b_fa = bias(fa, indy, na.rm = T),
#             b_rj = bias(rj, indy, na.rm = T)) %>% 
#   pivot_longer(cols = 1:2) %>% 
#   summarize(data_type_sd = sd(value))
# 
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   filter(!(metric == "mean_strength" & type == "fa")) %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(b = bias(inference, indy, na.rm = T))
# 
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   filter(!(metric == "mean_strength" & type == "fa")) %>% 
#   group_by(study_param_responses) %>% 
#   summarize(b = bias(inference, indy, na.rm = T))
# 
# results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   filter(!(metric == "mean_strength" & type == "fa")) %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(b = bias(inference, indy, na.rm = T))
# 
# results_data %>% 
#   summarize(b_fa = bias(fa, indy, na.rm = T),
#             b_rj = bias(rj, indy, na.rm = T))
# 
# # fa ms only
# results_data %>% 
#   filter(metric == "mean_strength") %>% 
#   summarize(b = bias(fa, indy))


