library(tidyverse)

randp <- readRDS("00_Cold_Storage/04_Individual_Networks/grid.rds")
data <- readRDS("01_Data/05_indyNets/indyNet_properties_full.RDS")

# add "param" index
randp <- randp %>% 
  bind_cols(param = 1:25) %>% 
  select(p = ps, r = rs, param)

# join r and p values to individual netwok's metrics
combined_data <- data %>% 
  left_join(randp, by = "param")

# calculate correlation of avg strength and CC for text
cor.test(combined_data$mean_strength, combined_data$cc, method = "pearson")

mean <- combined_data %>% 
  select(p, r, n_edges, mean_strength, cc, aspl, mod) %>% 
  group_by(p, r) %>% 
  summarize_all("mean") %>% 
  mutate(measure = "mean")

sd <- combined_data %>% 
  select(p, r, n_edges, mean_strength, cc, aspl, mod) %>% 
  group_by(p, r) %>% 
  summarize_all("sd") %>% 
  mutate(measure = "sd")

table_data_sd <- mean %>% 
  bind_rows(sd)

sup_table_s1 <- table_data_sd %>% 
  pivot_wider(names_from = measure, values_from = c(n_edges, mean_strength, cc, aspl, mod)) %>%
  left_join(randp, by = c("p", "r")) %>%
  arrange(param) %>%
  select(-param)

round_custom <- function(x) {
  ifelse(abs(x) >= 0.01, round(x, 2), signif(x, 2))
}

sup_table_s1_sig2 <- sup_table_s1 %>%
  ungroup() %>%
  mutate(across(-c("r", "p"), \(x) round_custom(x)))

write_csv(sup_table_s1, "01_Data/XX_Tables/s1_table.csv")
write_csv(sup_table_s1_sig2, "01_Data/XX_Tables/s1_table_sig2.csv")


# ci_lower <- plot_data %>% 
#   select(p, r, n_edges, mean_strength, cc, aspl, mod) %>% 
#   group_by(p, r) %>% 
#   summarize_all(\(x){quantile(x, 0.05)}) %>% 
#   mutate(measure = "ci_90_lower")
# ci_upper <- plot_data %>% 
#   select(p, r, n_edges, mean_strength, cc, aspl, mod) %>% 
#   group_by(p, r) %>% 
#   summarize_all(\(x){quantile(x, 0.95)}) %>% 
#   mutate(measure = "ci_90_upper")
# 
# table_data_ci <- mean %>% 
#   bind_rows(ci_lower, ci_upper)



# Additional plot

plot_data %>% 
  ggplot(aes(x = r, y = p)) +
  geom_tile(aes(fill = mean_strength))
plot_data %>% 
  ggplot(aes(x = r, y = p)) +
  geom_tile(aes(fill = cc))

plot_data %>% 
  ggplot(aes(x = (param - 1) %% 5 , y = param / 5)) +
  geom_tile(aes(fill = mean_strength))
