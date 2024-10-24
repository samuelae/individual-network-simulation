# dependencies -----------------------------------------------------------------

library(tidyverse)
library(patchwork)

source("02_Code/Functions/Helpers/bias.R")

# data -------------------------------------------------------------------------

# indyNet properties
indyNet_properties <- readRDS("01_Data/05_indyNets/indyNet_properties_full.RDS")

# between network raw results
inference_properties <- readRDS("01_Data/09_Analysis_Results/analysis_results.rds") %>% 
  mutate(nw_data_fa_cc = na_if(nw_data_fa_cc, 1),
         nw_data_rj_cc = na_if(nw_data_rj_cc, 1)) %>% 
  select(param, subj, cue_set, study_param_cue_set_size, study_param_responses, study_param_breadth, 
         nw_data_indy_mean_strength, nw_data_indy_cc, nw_data_indy_aspl, nw_data_indy_mod,
         nw_data_fa_mean_strength, nw_data_fa_cc, nw_data_fa_aspl, nw_data_fa_mod,
         nw_data_rj_mean_strength, nw_data_rj_cc, nw_data_rj_aspl, nw_data_rj_mod)

# add indyNet properties based on individual "subject" and "parameter"
data <- inference_properties %>% 
  left_join(indyNet_properties, by = c("param", "subj"))

# biases
data_bias <- data %>%
  group_by(study_param_cue_set_size, study_param_responses, study_param_breadth) %>% 
  summarize(l.bias.fa.ms = bias(nw_data_fa_mean_strength, nw_data_indy_mean_strength, na.rm = T),
            l.bias.rj.ms = bias(nw_data_rj_mean_strength, nw_data_indy_mean_strength, na.rm = T),
            l.bias.fa.cc = bias(nw_data_fa_cc, nw_data_indy_cc, na.rm = T),
            l.bias.rj.cc = bias(nw_data_rj_cc, nw_data_indy_cc, na.rm = T),
            l.bias.fa.aspl = bias(nw_data_fa_aspl, nw_data_indy_aspl, na.rm = T),
            l.bias.rj.aspl = bias(nw_data_rj_aspl, nw_data_indy_aspl, na.rm = T),
            l.bias.fa.mod = bias(nw_data_fa_mod, nw_data_indy_mod, na.rm = T),
            l.bias.rj.mod = bias(nw_data_rj_mod, nw_data_indy_mod, na.rm = T),
            g.bias.fa.ms = bias(nw_data_fa_mean_strength, mean_strength, na.rm = T),
            g.bias.rj.ms = bias(nw_data_rj_mean_strength, mean_strength, na.rm = T),
            g.bias.fa.cc = bias(nw_data_fa_cc, cc, na.rm = T),
            g.bias.rj.cc = bias(nw_data_rj_cc, cc, na.rm = T),
            g.bias.fa.aspl = bias(nw_data_fa_aspl, aspl, na.rm = T),
            g.bias.rj.aspl = bias(nw_data_rj_aspl, aspl, na.rm = T),
            g.bias.fa.mod = bias(nw_data_fa_mod, mod, na.rm = T),
            g.bias.rj.mod = bias(nw_data_rj_mod, mod, na.rm = T)) %>% 
  select(study_param_cue_set_size, study_param_responses, 
         study_param_breadth, l.bias.fa.ms:g.bias.rj.mod)

plot_data_bias <- data_bias %>%
  pivot_longer(cols = l.bias.fa.ms:g.bias.rj.mod) %>%
  separate_wider_delim(cols = name, delim = ".", names = c("comparison", "delete", "type", "metric")) %>% 
  select(-delete)

# calculate correlations
corrs <- data %>% 
  group_by(cue_set, study_param_cue_set_size, study_param_responses,
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

plot_data_corrs <- corrs %>%
  ungroup() %>% 
  pivot_longer(global.mean_strength.fa:local.mod.rj) %>% 
  separate_wider_delim(cols = name, delim = ".", names = c("comparison", "metric", "type")) %>% 
  mutate(comparison = case_match(comparison, 
                                 "local" ~ "l", 
                                 "global" ~ "g"),
         metric = if_else(metric == "mean_strength", "ms", metric)) %>% 
  group_by(study_param_cue_set_size, 
           study_param_responses, 
           study_param_breadth,
           comparison,
           type,
           metric) %>% 
  summarize(value = mean(value, na.rm = TRUE))


# # improve casing for figure
plot_data_bias <- plot_data_bias %>%
  mutate(study_param_breadth = case_when(
    study_param_breadth == "broad" ~ "Broad",
    study_param_breadth == "mixed" ~ "Mixed",
    study_param_breadth == "narrow" ~ "Narrow"
  ))
plot_data_corrs <- plot_data_corrs %>% 
  mutate(study_param_breadth = case_when(
    study_param_breadth == "broad" ~ "Broad",
    study_param_breadth == "mixed" ~ "Mixed",
    study_param_breadth == "narrow" ~ "Narrow"
  ))

# plotting ---------------------------------------------------------------------

# custom plotting function (continuous)
make_tileplot <- function(data, 
                          p_metric = c("strength", "edge_weight", "ms",
                                       "cc", "aspl", "mod"), 
                          p_comparison = c("l", "g", "d"),
                          p_type = c("fa", "rj"),
                          p_add_lcomparison = FALSE,
                          p_limits) {
  
  plot_data  <- data %>% 
    filter(metric == p_metric) %>% 
    filter(comparison == p_comparison) %>% 
    filter(type == p_type) %>% 
    mutate(color_value = log(value + 1.01),
           label_value = if_else(abs(value) >= 1, signif(value, 3), round(value, 2))) 
  
  p_limits_color <- log(p_limits + 1.01)
  
  plot_data <- plot_data %>% 
    mutate(color = (color_value > (p_limits_color[1] / 2)) & (color_value < (p_limits_color[2] / 2)))
  
  if(p_add_lcomparison) {
    plot_data_local <- data %>% 
      filter(metric == p_metric) %>% 
      filter(comparison == "l") %>% 
      filter(type == p_type) %>% 
      mutate(color_value = log(value + 1.01),
             label_value = if_else(abs(value) >= 1, signif(value, 3), round(value, 2))) %>% 
      bind_cols(color = plot_data$color)
    
  }
  
  plot <- ggplot(plot_data, aes(x = as_factor(study_param_cue_set_size),
                                y = as_factor(study_param_responses))) +
    geom_tile(aes(fill = color_value)) +
    geom_text(aes(label = label_value, color = color), nudge_y = 0.1, size = 6) +
    geom_text(data = plot_data_local, 
              aes(label = label_value,
                  color = color),
              nudge_y = -0.25,
              size = 4) +
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


# custom plotting function (binned)
make_tileplot_d <- function(data, 
                            p_metric = c("strength", "edge_weight", "ms",
                                         "cc", "aspl", "mod"), 
                            p_comparison = c("l", "g", "d"),
                            p_type = c("fa", "rj"),
                            p_add_lcomparison = FALSE,
                            p_limits = c(-0.3, 0.3)) {
  
  plot_data  <- data %>% 
    filter(metric == p_metric) %>% 
    filter(comparison == p_comparison) %>% 
    filter(type == p_type) %>% 
    mutate(color_value = case_when(value < p_limits[1] ~ "negative",
                                   value >= p_limits[1] & value <= p_limits[2] ~ "good",
                                   value > p_limits[2] ~ "positive"),
           label_value = if_else(abs(value) >= 1, signif(value, 3), round(value, 2))) 
  
  plot_data <- plot_data %>% 
    mutate(color = (value >= p_limits[1]) & (value <= p_limits[2]))
  
  if(p_add_lcomparison) {
    plot_data_local <- data %>% 
      filter(metric == p_metric) %>% 
      filter(comparison == "l") %>% 
      filter(type == p_type) %>% 
      mutate(color_value = case_when(value < p_limits[1] ~ "negative",
                                     value >= p_limits[1] & value <= p_limits[2] ~ "good",
                                     value > p_limits[2] ~ "positive"),
             label_value = if_else(abs(value) >= 1, signif(value, 3), round(value, 2))) %>% 
      bind_cols(color = plot_data$color)
    
  }
  
  plot <- ggplot(plot_data, aes(x = as_factor(study_param_cue_set_size),
                                y = as_factor(study_param_responses))) +
    geom_tile(aes(fill = color_value)) +
    geom_text(aes(label = label_value, color = color), nudge_y = 0.1, size = 6) +
    geom_text(data = plot_data_local, 
              aes(label = label_value,
                  color = color),
              nudge_y = -0.25,
              size = 4) +
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

# gdiffs plot (detailed) ------------------------------------------------------------------

limits = c(-1, 1)

ms_fa <- make_tileplot(plot_data_bias, "ms", "g", "fa", TRUE, limits)
ms_rj <- make_tileplot(plot_data_bias, "ms", "g", "rj", TRUE, limits)
limits = c(-0.56, 56.9)
aspl_fa <- make_tileplot(plot_data_bias, "aspl", "g", "fa", TRUE, limits)
aspl_rj <- make_tileplot(plot_data_bias, "aspl", "g", "rj", TRUE, limits)
limits = c(-0.98, 1.6)
cc_fa <- make_tileplot(plot_data_bias, "cc", "g", "fa", TRUE, limits)
cc_rj <- make_tileplot(plot_data_bias, "cc", "g", "rj", TRUE, limits)
limits = c(-0.93, 3.84)
mod_fa <- make_tileplot(plot_data_bias, "mod", "g", "fa", TRUE, limits)
mod_rj <- make_tileplot(plot_data_bias, "mod", "g", "rj", TRUE, limits)

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj) 

# ggsave("03_Plots/xx_Global_Tileplots/gbias_square.pdf", between_nw,
#        width = 20, height = 16, units = "cm", scale = 2)

ggsave("03_Plots/Generalizability/generalizability_bias_detailed.pdf", between_nw,
       width = 20, height = 13, units = "cm", scale = 2)


# gdiffs plot (binned) ------------------------------------------------------------------

ms_fa <- make_tileplot_d(plot_data_bias, "ms", "g", "fa", TRUE)
ms_rj <- make_tileplot_d(plot_data_bias, "ms", "g", "rj", TRUE)
aspl_fa <- make_tileplot_d(plot_data_bias, "aspl", "g", "fa", TRUE)
aspl_rj <- make_tileplot_d(plot_data_bias, "aspl", "g", "rj", TRUE)
cc_fa <- make_tileplot_d(plot_data_bias, "cc", "g", "fa", TRUE)
cc_rj <- make_tileplot_d(plot_data_bias, "cc", "g", "rj", TRUE)
mod_fa <- make_tileplot_d(plot_data_bias, "mod", "g", "fa", TRUE)
mod_rj <- make_tileplot_d(plot_data_bias, "mod", "g", "rj", TRUE)

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj) 

# ggsave("03_Plots/xx_Global_Tileplots/gbias_square.pdf", between_nw,
#        width = 20, height = 16, units = "cm", scale = 2)

ggsave("03_Plots/Generalizability/generalizability_bias_binned.pdf", between_nw,
       width = 20, height = 13, units = "cm", scale = 2)


# gcorrs plot ------------------------------------------------------------------

# custom plotting function (detailed)
make_tileplot_corr <- function(data, 
                               p_metric = c("strength", "edge_weight", "ms",
                                            "cc", "aspl", "mod"), 
                               p_comparison = c("l", "g", "d"),
                               p_type = c("fa", "rj"),
                               p_add_lcomparison = FALSE,
                               p_limits = NULL) {
  
  plot_data  <- data %>% 
    filter(metric == p_metric) %>% 
    filter(comparison == p_comparison) %>% 
    filter(type == p_type) %>% 
    mutate(color_value = value,
           label_value = round(value, 2))
  
  if(is.null(p_limits)) {
    p_limits = c(min(plot_data$value), max(plot_data$value))
  }
  
  plot_data <- plot_data %>% 
    mutate(color = value < (p_limits[1] + ((p_limits[2] - p_limits[1]) / 2)))
  
  if(p_add_lcomparison) {
    plot_data_local <- data %>% 
      filter(metric == p_metric) %>% 
      filter(comparison == "l") %>% 
      filter(type == p_type) %>% 
      mutate(color_value = value,
             label_value = round(value, 2)) %>% 
      bind_cols(color = plot_data$color)
  }
  
  plot <- ggplot(plot_data, aes(x = as_factor(study_param_cue_set_size),
                                y = as_factor(study_param_responses))) +
    geom_tile(aes(fill = color_value)) +
    geom_text(aes(label = label_value, color = color), nudge_y = 0.1, size = 6) +
    geom_text(data = plot_data_local, 
              aes(label = label_value,
                  color = color),
              nudge_y = -0.25,
              size = 4) +
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

# custom plotting function (binned)
make_tileplot_corr_d <- function(data, 
                                 p_metric = c("strength", "edge_weight", "ms",
                                              "cc", "aspl", "mod"), 
                                 p_comparison = c("l", "g", "d"),
                                 p_type = c("fa", "rj"),
                                 p_add_lcomparison = FALSE,
                                 p_limits = c(-0.01, 0.5)) {
  
  plot_data  <- data %>% 
    filter(metric == p_metric) %>% 
    filter(comparison == p_comparison) %>% 
    filter(type == p_type) %>% 
    mutate(color_value = case_when(value < p_limits[1] ~ "negative",
                                   value >= p_limits[1] & value < 0.5 ~ "positive",
                                   value >= p_limits[2] ~ "good")) %>% 
    mutate(label_value = round(value, 2))
  
  plot_data <- plot_data %>% 
    mutate(color = (value <= p_limits[2]))
  
  if(p_add_lcomparison) {
    plot_data_local <- data %>% 
      filter(metric == p_metric) %>% 
      filter(comparison == "l") %>% 
      filter(type == p_type) %>% 
      mutate(color_value = case_when(value < p_limits[1] ~ "negative",
                                     value >= p_limits[1] & value < p_limits[2] ~ "positive",
                                     value >= p_limits[2] ~ "good"),
             label_value = round(value, 2)) %>% 
      bind_cols(color = plot_data$color)
  }
  
  plot <- ggplot(plot_data, aes(x = as_factor(study_param_cue_set_size),
                                y = as_factor(study_param_responses))) +
    geom_tile(aes(fill = color_value)) +
    geom_text(aes(label = label_value, color = color), nudge_y = 0.1, size = 6) +
    geom_text(data = plot_data_local, 
              aes(label = label_value,
                  color = color),
              nudge_y = -0.25,
              size = 4) +
    scale_color_manual(guide = "none", values = c("FALSE" = "black", "TRUE" = "white")) +
    theme_minimal() +
    scale_fill_manual(values = c("negative" = "#440154FF", "positive" = "#21908CFF", "good" = "#FDE725FF")) +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "grey90", 
                                          color = NA),
          axis.title = element_blank()) +
    facet_wrap(~study_param_breadth)
  
  plot
  
}

# plot resolution detailed

limits = c(-1, 1)

ms_fa <- make_tileplot_corr(plot_data_corrs, "ms", "g", "fa", TRUE, limits)
ms_rj <- make_tileplot_corr(plot_data_corrs, "ms", "g", "rj", TRUE, limits)

aspl_fa <- make_tileplot_corr(plot_data_corrs, "aspl", "g", "fa", TRUE, limits)
aspl_rj <- make_tileplot_corr(plot_data_corrs, "aspl", "g", "rj", TRUE, limits)

cc_fa <- make_tileplot_corr(plot_data_corrs, "cc", "g", "fa", TRUE, limits)
cc_rj <- make_tileplot_corr(plot_data_corrs, "cc", "g", "rj", TRUE, limits)

mod_fa <- make_tileplot_corr(plot_data_corrs, "mod", "g", "fa", TRUE, limits)
mod_rj <- make_tileplot_corr(plot_data_corrs, "mod", "g", "rj", TRUE, limits)

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj) 

# ggsave("03_Plots/xx_Global_Tileplots/gcorrs.pdf", between_nw,
#        width = 20, height = 16, units = "cm", scale = 2)

ggsave("03_Plots/Generalizability/greliability_resolution_detailed.pdf", between_nw,
       width = 20, height = 13, units = "cm", scale = 2)

# plot resolution binned


ms_fa <- make_tileplot_corr_d(plot_data_corrs, "ms", "g", "fa", TRUE)
ms_rj <- make_tileplot_corr_d(plot_data_corrs, "ms", "g", "rj", TRUE)

aspl_fa <- make_tileplot_corr_d(plot_data_corrs, "aspl", "g", "fa", TRUE)
aspl_rj <- make_tileplot_corr_d(plot_data_corrs, "aspl", "g", "rj", TRUE)

cc_fa <- make_tileplot_corr_d(plot_data_corrs, "cc", "g", "fa", TRUE)
cc_rj <- make_tileplot_corr_d(plot_data_corrs, "cc", "g", "rj", TRUE)

mod_fa <- make_tileplot_corr_d(plot_data_corrs, "mod", "g", "fa", TRUE)
mod_rj <- make_tileplot_corr_d(plot_data_corrs, "mod", "g", "rj", TRUE)

between_nw <- (ms_fa + ms_rj) /
  (aspl_fa + aspl_rj) /
  (cc_fa + cc_rj) /
  (mod_fa + mod_rj) 

# ggsave("03_Plots/xx_Global_Tileplots/gcorrs.pdf", between_nw,
#        width = 20, height = 16, units = "cm", scale = 2)

ggsave("03_Plots/Generalizability/greliability_resolution_binned.pdf", between_nw,
       width = 20, height = 13, units = "cm", scale = 2)


# EXTRACT NUMBERS FOR RESULTS SECTION ------------------------------------------

# Prepare data -----------------------------------------------------------------

bias_results_data <- data %>%
  rename(nw_data_full_mean_strength = mean_strength,
         nw_data_full_cc = cc,
         nw_data_full_aspl = aspl,
         nw_data_full_mod = mod) %>%
  pivot_longer(cols = starts_with("nw_data_"), names_to = "name", values_to = "values") %>%
  select(-c(n_nodes:avg_degr)) %>%
  separate_wider_delim(cols = name, delim = "_", names = c(NA, NA, "type", "metric"), too_many = "merge") %>%
  pivot_wider(names_from = type, values_from = values)

bias_results_data <- bias_results_data %>% 
  pivot_longer(cols = c(fa, rj), names_to = "response_type", values_to = "inference") %>% 
  pivot_longer(cols = c(indy, full), names_to = "comparison", values_to = "ground_truth") %>% 
  mutate(comparison = if_else(comparison == "indy", "l", "g")) %>% 
  rename(cue_set_size = study_param_cue_set_size, 
         number_of_responses = study_param_responses, 
         cue_set_type = study_param_breadth, 
         network_measure = metric) %>% 
  group_by(cue_set_size, number_of_responses, cue_set_type, response_type, network_measure, comparison) %>% 
  summarize(b = bias(ground_truth, inference)) %>% 
  mutate(bias_label = case_when(b < -0.3 ~ "negative",
                                b >= -0.3 & b <= 0.3 ~ "good",
                                b > 0.3 ~ "positive")) %>% 
  ungroup()

corr_results_data <- plot_data_corrs %>% 
  ungroup() %>% 
  rename(cue_set_size = study_param_cue_set_size, 
         number_of_responses = study_param_responses, 
         cue_set_type = study_param_breadth, 
         response_type = type,
         network_measure = metric) %>% 
  mutate(resolution_label = case_when(value >= 0.5 ~ "good",
                                      value < 0.5 & value > -0.01 ~ "poor",
                                      value <= -0.01 ~ "negative"))

# Extract numbers --------------------------------------------------------------

options(pillar.sigfig = 5)

# Overall
bias_results_data %>% 
  filter(comparison == "g") %>% 
  group_by(bias_label) %>% 
  summarize(n = n(),
            percent = n() / 216 * 100)
corr_results_data %>% 
  filter(comparison == "g") %>% 
  group_by(resolution_label) %>% 
  summarize(n = n(),
            percent = n() / 216 * 100)
bias_results_data %>% 
  filter(comparison == "l") %>% 
  group_by(bias_label) %>% 
  summarize(n = n(),
            percent = n() / 216 * 100)
corr_results_data %>% 
  filter(comparison == "l") %>% 
  group_by(resolution_label) %>% 
  summarize(n = n(),
            percent = n() / 216 * 100)

# Per measure
bias_results_data %>% 
  group_by(comparison, network_measure, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 54 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(n_g = replace_na(n_g, 0),
         percent_g = replace_na(percent_g, 0)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent_g))

corr_results_data %>% 
  group_by(comparison, network_measure, resolution_label) %>% 
  summarize(n = n(),
            percent = n() / 54 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(n_g = replace_na(n_g, 0),
         percent_g = replace_na(percent_g, 0)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(resolution_label == "good") %>% 
  arrange(desc(percent_g))

# Per design factor: response type
bias_results_data %>% 
  group_by(comparison, response_type, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 108 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent_g))
corr_results_data %>% 
  group_by(comparison, response_type, resolution_label) %>% 
  summarize(n = n(),
            percent = n() / 108 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(resolution_label == "good") %>% 
  arrange(desc(percent_g))

# Per design factor: number of responses
bias_results_data %>% 
  group_by(comparison, number_of_responses, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent_g))
corr_results_data %>% 
  group_by(comparison, number_of_responses, resolution_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(resolution_label == "good") %>% 
  arrange(desc(percent_g))

# Per design factor: cue set size
bias_results_data %>% 
  group_by(comparison, cue_set_size, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent_g))
corr_results_data %>% 
  group_by(comparison, cue_set_size, resolution_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(resolution_label == "good") %>% 
  arrange(desc(percent_g))

# Per design factor: cue set type
bias_results_data %>% 
  group_by(comparison, cue_set_type, bias_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(bias_label == "good") %>% 
  arrange(desc(percent_g))
corr_results_data %>% 
  group_by(comparison, cue_set_type, resolution_label) %>% 
  summarize(n = n(),
            percent = n() / 72 * 100) %>% 
  pivot_wider(names_from = comparison, values_from = c(n, percent)) %>% 
  mutate(percent_point_diff = percent_g - percent_l) %>% 
  filter(resolution_label == "good") %>% 
  arrange(desc(percent_g))

# EXTRACT NUMBERS FOR DISCUSSION -----------------------------------------------

plot_data_corrs %>%
  filter(comparison == "g") %>%
  group_by(study_param_cue_set_size, study_param_responses) %>%
  summarize(r = mean(value))



# 
# # bias ---
# 
# bias_results_data <- data %>% 
#   rename(nw_data_full_mean_strength = mean_strength, 
#          nw_data_full_cc = cc,
#          nw_data_full_aspl = aspl,
#          nw_data_full_mod = mod) %>%
#   pivot_longer(cols = starts_with("nw_data_"), names_to = "name", values_to = "values") %>% 
#   select(-c(n_nodes:avg_degr)) %>% 
#   separate_wider_delim(cols = name, delim = "_", names = c(NA, NA, "type", "metric"), too_many = "merge") %>% 
#   pivot_wider(names_from = type, values_from = values)
# 
# # overall bias
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   summarize(a_full = bias(inference, full, na.rm = T))
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   summarize(a_indy = bias(inference, indy, na.rm = T))
# 
# # by measure
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(metric) %>% 
#   summarize(a = bias(inference, full, na.rm = T)) %>% 
#   mutate(a_dev = abs(a) - 0) %>% 
#   arrange(a_dev)
# 
# # by design factor
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(a = bias(inference, full, na.rm = T)) %>% 
#   summarize(cue_set_size_sd = sd(a)) %>% 
#   pull()
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_responses) %>% 
#   summarize(a = bias(inference, full, na.rm = T)) %>% 
#   summarize(responses_sd = sd(a))
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(a = bias(inference, full, na.rm = T)) %>% 
#   summarize(set_type_sd = sd(a))
# bias_results_data %>% 
#   summarize(a_fa = bias(fa, full, na.rm = T),
#             a_rj = bias(rj, full, na.rm = T)) %>% 
#   pivot_longer(cols = 1:2) %>% 
#   summarize(data_type_sd = sd(value)) %>% 
#   pull()
# 
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(a = bias(inference, full, na.rm = T))
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_responses) %>% 
#   summarize(a = bias(inference, full, na.rm = T))
# bias_results_data %>% 
#   pivot_longer(cols = c(fa, rj), names_to = "type", values_to = "inference") %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(a = bias(inference, full, na.rm = T))
# bias_results_data %>% 
#   summarize(a_fa = bias(fa, full, na.rm = T),
#             a_rj = bias(rj, full, na.rm = T))
# 
# # CORRS ---
# corr_results_data <- plot_data_corrs %>% ungroup()
# 
# # overall corr
# corr_results_data %>% group_by(comparison) %>% summarize(mean(value, na.rm = T))
# 
# # by measure
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(metric) %>% summarize(r = mean(value, na.rm = T)) %>% arrange(desc(r))
# 
# # by design factor
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(m = mean(value, na.rm = T)) %>% 
#   summarize(sd = sd(m))
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(study_param_cue_set_size) %>% 
#   summarize(m = mean(value, na.rm = T)) 
# 
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(study_param_responses) %>% 
#   summarize(m = mean(value, na.rm = T)) %>% 
#   summarize(sd = sd(m))
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(study_param_responses) %>% 
#   summarize(m = mean(value, na.rm = T))
# 
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(m = mean(value, na.rm = T)) %>% 
#   summarize(sd = sd(m))
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(study_param_breadth) %>% 
#   summarize(m = mean(value, na.rm = T))
# 
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(type) %>% 
#   summarize(m = mean(value, na.rm = T)) %>% 
#   summarize(sd = sd(m, na.rm = T))
# corr_results_data %>% 
#   filter(comparison == "g") %>% 
#   group_by(type) %>% 
#   summarize(m = mean(value, na.rm = T))
# 



# 
# old <- options(pillar.sigfig = 5)
# 
# results_diffs <- plot_data_diffs %>% ungroup()
# results_corrs <- plot_data_corrs %>% ungroup()
# 
# # ([mean abs diff global], [mean abs diff local]). 
# results_diffs %>% group_by(comparison) %>% summarize(mean(abs(value)))
# 
# # ([mean correlation global], [mean correlation local]).
# results_corrs %>% group_by(comparison) %>% summarize(mean(value, na.rm = TRUE))
# 
# 
# # ([mean gms 10], [mean gms 100], [mean gms 1000]), while in local comparisons, 
# results_diffs %>% filter(metric == "ms", comparison == "g") %>% group_by(study_param_cue_set_size) %>% summarize(mean(value))
# 
# # smaller cue set sizes have less of a bias ([mean lms 10], [mean lms 100], [mean lms 1000]).
# results_diffs %>% filter(metric == "ms", comparison == "l") %>% group_by(study_param_cue_set_size) %>% summarize(mean(value))
# 
# #  cue sets ([mean gcorr broad], [mean gcorr mixed], [mean gcorr narrow]). 
# results_corrs %>% filter(comparison == "g") %>% group_by(study_param_breadth) %>% summarize(mean(value, na.rm = TRUE))
# 
# 
# options(old)

# EXTRACT NUMBERS FOR DISCUSSION -----------------------------------------------
# 
# corrs <- plot_data_corrs %>% 
#   filter(comparison == "g") %>% 
#   group_by(study_param_cue_set_size, study_param_responses) %>% 
#   summarize(value = mean(value)) %>% 
#   pull(value)
# corrs[5]/corrs[9]
