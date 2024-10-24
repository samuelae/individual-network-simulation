# dependencies -----------------------------------------------------------------

# packages
library(tidyverse)
library(igraph)
library(patchwork)

# functions
source("02_Code/Functions/Behavioral_Data/generate_relatedness_judgments.R")
source("02_Code/Functions/Network/edge_weight_threshold.R")

# import and prepare data from filesystem --------------------------------------

base_graph <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_fully_connected.rds")

men <- read_delim("00_Cold_Storage/01_Source/MEN_dataset_natural_form_full",
                  col_names = c("word_1", "word_2", "relatedness"))

# get MEN ratings with only words in base_graph
words <- V(base_graph)$name %>% as.character()
men_limited <- men %>% 
  filter(word_1 %in% words & word_2 %in% words)

# predict similarity for MEN with different cosine cut offs --------------------
# show that 0.2 cut off (to reduce computational complexity) is a good choice

cut_offs <- seq(from = 0.05, to = 0.4, by = 0.05)

working_graph <- base_graph
men_predictions <- tibble(word_1 = character(0),
                          word_2 = character(0),
                          rel_jud = numeric(0),
                          cut_off = numeric(0),
                          relatedness = numeric(0))

for(cut_off in cut_offs) {
  working_graph <- edge_weight_threshold(working_graph, cut_off)
  predictions <- generate_relatedness_judgments(men_limited$word_1,
                                                men_limited$word_2,
                                                working_graph,
                                                sensitize = FALSE,
                                                lower_bound = 0,
                                                upper_bound = 1,
                                                integers = FALSE,
                                                sd_noise = 0)
  men_predictions <- men_predictions %>% 
    bind_rows(predictions %>% 
                bind_cols(cut_off = rep(cut_off, times = nrow(men_limited)), 
                          relatedness = men_limited$relatedness))
}

# write to fs
write_csv(men_predictions, 
          "01_Data/04_Relatedness_Judgment/men_predictions_cut_off.csv")
men_predictions <- read_csv("01_Data/04_Relatedness_Judgment/men_predictions_cut_off.csv")

# analyze men predictions for cut offs -----------------------------------------

cut_off_plot <- men_predictions %>%
  group_by(cut_off) %>%
  mutate(cor = cor(relatedness, rel_jud, method = "pearson")) %>%
  distinct(cor) %>%
  ungroup() %>% 
  filter(cut_off <= 0.3) %>% 
  mutate(col = c("0", "0", "0", "1", "0", "0")) %>% 
  ggplot(aes(x = cut_off, y = cor, fill = col)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("grey30", "darkred")) +
  geom_text(aes(label = round(cor, 3)), nudge_y = -0.05, color = "white") +
  scale_x_continuous(n.breaks = 8) +
  theme_minimal() +
  ylab(expression(r[Pearson])) +
  xlab("cut off") +
  labs(title = "RJ model correlation to MEN ratings", 
       subtitle = "Varying ground truth network edge weight cut off")

ggsave(plot = cut_off_plot, 
       file = "03_Plots/02_Tuning_Relatedness_Judgment/relatedness_judgment_cut_offs.pdf",
       scale = 0.5)

# fit representation sensitivity -----------------------------------------------

graph_0.20 <- readRDS("00_Cold_Storage/02_fastText_Network/base_graph_0.2.rds")

men_predictions_sensitivity <- men_limited %>% 
  bind_cols(`0.5` = generate_relatedness_judgments(men_limited$word_1,
                                                   men_limited$word_2,
                                                   graph_0.20,
                                                   sensitize = TRUE,
                                                   sensitivity_representation = 0.5,
                                                   lower_bound = 0,
                                                   upper_bound = 1,
                                                   integers = FALSE,
                                                   sd_noise = 0) %>% pull(rel_jud))

men_predictions_sensitivity <- men_predictions_sensitivity %>% 
  bind_cols("1" = generate_relatedness_judgments(men_limited$word_1,
                                                 men_limited$word_2,
                                                 graph_0.20,
                                                 sensitize = TRUE,
                                                 sensitivity_representation = 1,
                                                 lower_bound = 0,
                                                 upper_bound = 1,
                                                 integers = FALSE,
                                                 sd_noise = 0) %>% pull(rel_jud))

men_predictions_sensitivity <- men_predictions_sensitivity %>% 
  bind_cols("2" = generate_relatedness_judgments(men_limited$word_1,
                                                 men_limited$word_2,
                                                 graph_0.20,
                                                 sensitize = TRUE,
                                                 sensitivity_representation = 2,
                                                 lower_bound = 0,
                                                 upper_bound = 1,
                                                 integers = FALSE,
                                                 sd_noise = 0) %>% pull(rel_jud))

men_predictions_sensitivity <- men_predictions_sensitivity %>% 
  bind_cols("10" = generate_relatedness_judgments(men_limited$word_1,
                                                  men_limited$word_2,
                                                  graph_0.20,
                                                  sensitize = TRUE,
                                                  sensitivity_representation = 10,
                                                  lower_bound = 0,
                                                  upper_bound = 1,
                                                  integers = FALSE,
                                                  sd_noise = 0) %>% pull(rel_jud))

write_csv(men_predictions_sensitivity, 
          "01_Data/04_Relatedness_Judgment/men_predictions_sensitivity.csv")
men_predictions_sensitivity <- read_csv("01_Data/04_Relatedness_Judgment/men_predictions_sensitivity.csv")

# analyze representations sensitivity results
predictions_long <- men_predictions_sensitivity %>% 
  pivot_longer(cols = c(`0.5`, `1`, `2`, `10`), 
               names_to = "sensitivity", 
               values_to = "prediction") %>% 
  mutate(sensitivity = as.numeric(sensitivity))

cor_sp <- predictions_long %>% 
  group_by(sensitivity) %>% 
  mutate(cor = cor(relatedness, prediction, method = "spearman")) %>% 
  distinct(cor) %>% 
  ggplot(aes(x = as_factor(sensitivity), y = cor)) +
  geom_col() +
  geom_text(aes(label = round(cor, 3)), nudge_y = -0.05, color = "white") +
  theme_minimal() +
  ylab("r") +
  xlab(expression(gamma)) +
  labs(title = "Spearman correlation")

cor_pe <- predictions_long %>% 
  group_by(sensitivity) %>% 
  mutate(cor = cor(relatedness, prediction, method = "pearson")) %>% 
  distinct(cor) %>% 
  ungroup() %>% 
  mutate(col = c("0", "1", "0", "0")) %>% 
  ggplot(aes(x = as_factor(sensitivity), y = cor, fill = col)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("grey30", "darkred")) +
  geom_text(aes(label = round(cor, 3)), nudge_y = -0.05, color = "white") +
  theme_minimal() +
  ylab(expression(r[Spearman])) +
  xlab(expression(gamma)) +
  labs(title = "Sensitivity parameter correlation")

gamma <- cor_sp + cor_pe + plot_annotation(tag_levels = "A")
ggsave("03_Plots/02_Tuning_Relatedness_Judgment/relatedness_judgment_sensitivity.pdf", 
       plot = gamma, width = 19, height = 9, units = "cm", scale = 1)
ggsave("03_Plots/02_Tuning_Relatedness_Judgment/relatedness_judgment_sensitivity.png", 
       plot = gamma, width = 19, height = 9, units = "cm", scale = 1, dpi = 300)

# add interrater reliability and discrete outcomes -----------------------------

# target interrater reliability on likert scale 1-7 from MEN paper r[spearman] = 0.68

men_predictions_noise_1 <- tibble(word_1 = character(0),
                                  word_2 = character(0),
                                  rel_jud = numeric(0),
                                  sd_noise = numeric(0),
                                  relatedness = numeric(0))
men_predictions_noise_2 <- men_predictions_noise_1

noise_levels <- seq(0.10, 0.30, 0.025)
set.seed(1337)

# simulate raters 1 and 2
for(noise_level in noise_levels) {
  
  rj_1 <- generate_relatedness_judgments(men_limited$word_1,
                                         men_limited$word_2,
                                         graph_0.20,
                                         sensitivity_representation = 1,
                                         lower_bound = 1,
                                         upper_bound = 7,
                                         integers = TRUE,
                                         sd_noise = noise_level)
  
  rj_2 <- generate_relatedness_judgments(men_limited$word_1,
                                         men_limited$word_2,
                                         graph_0.20,
                                         sensitivity_representation = 1,
                                         lower_bound = 1,
                                         upper_bound = 7,
                                         integers = TRUE,
                                         sd_noise = noise_level)
  
  men_predictions_noise_1 <- men_predictions_noise_1 %>% 
    bind_rows(rj_1 %>% 
                bind_cols(sd_noise = rep(noise_level, times = nrow(men_limited)), 
                          relatedness = men_limited$relatedness))
  
  men_predictions_noise_2 <- men_predictions_noise_2 %>% 
    bind_rows(rj_2 %>% 
                bind_cols(sd_noise = rep(noise_level, times = nrow(men_limited)), 
                          relatedness = men_limited$relatedness))
  
}

# calculate interrater reliability for each sd_noise level

men_predictions_noise_1 <- men_predictions_noise_1 %>% 
  mutate(rater = "A")
men_predictions_noise_2 <- men_predictions_noise_2 %>% 
  mutate(rater = "B")

noise_data_long <- men_predictions_noise_1 %>% 
  bind_rows(men_predictions_noise_2)

# interrater reliablity (coarse)

interrater_reliability <- noise_data_long %>% 
  pivot_wider(names_from = rater, values_from = rel_jud) %>% 
  group_by(sd_noise) %>% 
  summarise(cor_sp = cor(A, B, method = "spearman")) %>% 
  mutate(favorite = c("0", "0", "1", "0", "0", "0", "0", "0", "0"))

interrater_reliability <- ggplot(interrater_reliability, aes(x = sd_noise, y = cor_sp)) +
  geom_col(aes(fill = favorite), show.legend = FALSE) +
  scale_fill_manual(values = c("grey30", "darkred")) +
  geom_text(aes(label = round(cor_sp, 3)), nudge_y = -0.05, color = "white") +
  scale_x_continuous(n.breaks = 9) +
  theme_minimal() +
  ylab(expression(r[Spearman])) +
  xlab(expression(sigma)) +
  labs(title = "Correlation between raters A and B",
       subtitle = expression(paste(sigma, " before scaling to MEN data Likert scale from 1 to 7")))
ggsave("03_Plots/02_Tuning_Relatedness_Judgment/relatedness_judgment_interrater_reliability.pdf",
       interrater_reliability, width = 25, height = 12, units = "cm")

# result of tuning: cos cut off = 0.2, sensitivity = 1, sd_noise = 0.15

(cut_off_plot + cor_pe) /
  (interrater_reliability) + plot_annotation(tag_levels = "A")
ggsave("03_Plots/02_Tuning_Relatedness_Judgment/relatedness_judgment_tuning.pdf",
       width = 25, height = 25, units = "cm")




