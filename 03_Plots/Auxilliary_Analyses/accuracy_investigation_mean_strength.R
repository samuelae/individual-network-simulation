library(patchwork)

inference_properties <- readRDS("01_Data/09_Analysis_Results/analysis_results.rds") %>% 
  mutate(nw_data_fa_cc = na_if(nw_data_fa_cc, 1),
         nw_data_rj_cc = na_if(nw_data_rj_cc, 1)) %>% 
  select(param, subj, cue_set, study_param_cue_set_size, study_param_responses, study_param_breadth, 
         nw_data_indy_mean_strength, nw_data_indy_cc, nw_data_indy_aspl, nw_data_indy_mod,
         nw_data_fa_mean_strength, nw_data_fa_cc, nw_data_fa_aspl, nw_data_fa_mod,
         nw_data_rj_mean_strength, nw_data_rj_cc, nw_data_rj_aspl, nw_data_rj_mod)


d <- inference_properties %>%
  group_by(study_param_cue_set_size, 
           study_param_responses, 
           study_param_breadth) %>% 
  select(nw_data_indy_mean_strength, nw_data_fa_mean_strength) %>% 
  mutate(prop_mean_strength = nw_data_fa_mean_strength/nw_data_indy_mean_strength)

fa_broad <- d %>% 
  filter(study_param_breadth == "broad") %>% 
  ggplot(aes(x = prop_mean_strength)) +
  geom_histogram(bins = 100) +
  facet_grid(rows = vars(-study_param_responses),
             cols = vars(study_param_cue_set_size)) +
  labs(title = "broad")
fa_mixed <- d %>% 
  filter(study_param_breadth == "mixed") %>% 
  ggplot(aes(x = prop_mean_strength)) +
  geom_histogram(bins = 100) +
  facet_grid(rows = vars(-study_param_responses),
             cols = vars(study_param_cue_set_size)) +
  labs(title = "mixed")
fa_narrow <- d %>% 
  filter(study_param_breadth == "narrow") %>% 
  ggplot(aes(x = prop_mean_strength)) +
  geom_histogram(bins = 100) +
  facet_grid(rows = vars(-study_param_responses),
             cols = vars(study_param_cue_set_size)) +
  labs(title = "narrow")

fa <- (fa_broad + fa_mixed + fa_narrow) + 
  plot_annotation(title = "Free association") &
  theme_light()

d <- inference_properties %>%
  group_by(study_param_cue_set_size, 
           study_param_responses, 
           study_param_breadth) %>% 
  select(nw_data_indy_mean_strength, nw_data_rj_mean_strength) %>% 
  mutate(prop_mean_strength = nw_data_rj_mean_strength/nw_data_indy_mean_strength)

rj_broad <- d %>% 
  filter(study_param_breadth == "broad") %>% 
  ggplot(aes(x = prop_mean_strength)) +
  geom_histogram(bins = 100) +
  facet_grid(rows = vars(-study_param_responses),
             cols = vars(study_param_cue_set_size)) +
  labs(title = "broad")
rj_mixed <- d %>% 
  filter(study_param_breadth == "mixed") %>% 
  ggplot(aes(x = prop_mean_strength)) +
  geom_histogram(bins = 100) +
  facet_grid(rows = vars(-study_param_responses),
             cols = vars(study_param_cue_set_size)) +
  labs(title = "mixed")
rj_narrow <- d %>% 
  filter(study_param_breadth == "narrow") %>% 
  ggplot(aes(x = prop_mean_strength)) +
  geom_histogram(bins = 100) +
  facet_grid(rows = vars(-study_param_responses),
             cols = vars(study_param_cue_set_size)) +
  labs(title = "narrow")

rj <- (rj_broad + rj_mixed + rj_narrow) + 
  plot_annotation(title = "Relatedness Judgments") &
  theme_light()


ggsave(filename = "03_Plots/Auxilliary_Analyses/Mean_strength_fa.pdf", plot = fa, width = 18, height = 7)
ggsave(filename = "03_Plots/Auxilliary_Analyses/Mean_strength_rj.pdf", plot = rj, width = 18, height = 7)






