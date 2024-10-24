# dependencies

library(tidyverse)
library(igraph)
library(patchwork)

data_summary <- readRDS("01_Data/09_Analysis_Results/analysis_results_summarized.rds") %>% 
  ungroup() %>%
  mutate(nw_measure_group = if_else(nw_metric %in% c("edge_weight", "strength"), "within", "between"))

# calculate variances of the 10 random cue sets

variances <- data_summary %>% 
  group_by(study_param_cue_set_size, number_of_responses, study_param_breadth, 
           comparison, nw_metric) %>% 
  summarize(metric_mean = mean(cor_sp, na.rm = TRUE), 
            metric_median = median(cor_sp, na.rm = TRUE),
            metric_var = var(cor_sp, na.rm = TRUE))

variances <- variances %>% 
  ungroup() %>% 
  mutate(cue_set_size = as_factor(study_param_cue_set_size),
         number_of_responses = as_factor(number_of_responses),
         cue_set_type = as_factor(study_param_breadth)) %>% 
  select(cue_set_size, number_of_responses, cue_set_type, comparison, nw_metric,
         metric_mean, metric_var) %>% 
  filter(comparison %in% c("indy_fa", "indy_rj"))


plot_mean_var <- function(data, 
                          metric, 
                          response_type = c("indy_fa", "indy_rj")) {
  mean <- data %>% 
    filter(comparison %in% response_type,
           nw_metric %in% metric) %>% 
    ggplot(aes(x = cue_set_size, y = number_of_responses)) +
    geom_tile(aes(fill = metric_mean)) +
    geom_label(aes(label = sprintf("%.2f", metric_mean))) +
    scale_fill_viridis_c(name = "Mean") +
    facet_grid(cols = vars(cue_set_type)) +
    labs(title = "Resolution Mean", x = "Cue set size", y = "Number of responses") +
    theme_minimal()
  
  var <- data %>% 
    filter(comparison %in% response_type,
           nw_metric %in% metric) %>% 
    ggplot(aes(x = cue_set_size, y = number_of_responses)) +
    geom_tile(aes(fill = metric_var)) +
    geom_label(aes(label = sprintf("%.2f", metric_var))) +
    scale_fill_viridis_c(option = "B", name = "Variance") +
    facet_grid(cols = vars(cue_set_type)) +
    labs(title = "Resolution Variance", x = "Cue set size", y = "Number of responses") +
    theme_minimal()
  
  
  plt <- (mean / var) +
    plot_annotation(title = str_to_upper(metric))
  
  plt
  
}

# within-network
edge_weight_fa <- plot_mean_var(variances, "edge_weight", "indy_fa")
edge_weight_rj <- plot_mean_var(variances, "edge_weight", "indy_rj")
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/within_edge_weight_fa.pdf", edge_weight_fa, width = 9, height = 7)
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/within_edge_weight_rj.pdf", edge_weight_rj, width = 9, height = 7)

strength_fa <- plot_mean_var(variances, "strength", "indy_fa")
strength_rj <- plot_mean_var(variances, "strength", "indy_rj")
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/within_node_strength_fa.pdf", strength_fa, width = 9, height = 7)
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/within_node_strength_rj.pdf", strength_rj, width = 9, height = 7)


# between-network
avg_strength_fa <- plot_mean_var(variances, "mean_strength", "indy_fa")
avg_strength_rj <- plot_mean_var(variances, "mean_strength", "indy_rj")
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_avg_strength_fa.pdf", avg_strength_fa, width = 9, height = 7)
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_avg_strength_rj.pdf", avg_strength_rj, width = 9, height = 7)

aspl_fa <- plot_mean_var(variances, "aspl", "indy_fa")
aspl_rj <- plot_mean_var(variances, "aspl", "indy_rj")
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_aspl_fa.pdf", aspl_fa, width = 9, height = 7)
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_aspl_rj.pdf", aspl_rj, width = 9, height = 7)

cc_fa <- plot_mean_var(variances, "cc", "indy_fa")
cc_rj <- plot_mean_var(variances, "cc", "indy_rj")
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_cc_fa.pdf", cc_fa, width = 9, height = 7)
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_cc_rj.pdf", cc_rj, width = 9, height = 7)

mod_fa <- plot_mean_var(variances, "mod", "indy_fa")
mod_rj <- plot_mean_var(variances, "mod", "indy_rj")
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_mod_fa.pdf", mod_fa, width = 9, height = 7)
ggsave("03_Plots/SUP_Variance_Of_Cue_Sets/between_mod_rj.pdf", mod_rj, width = 9, height = 7)
