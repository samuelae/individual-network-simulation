library(tidyverse)

swi_sub <- readRDS("01_Data/XX_SWI_Analysis_Results/swi_analysis_results.rds")
swi_indy <- read_csv("01_Data/05_indyNets/small_world_index.csv")

swi <- swi_sub %>% 
  left_join(swi_indy %>% select(param, subj, swi.full_indy = index), by = c("param", "subj"))

swi <- swi %>% 
  select(param:cue_set, study_param_breadth, study_param_cue_set_size, 
         study_param_responses, swi.full_indy, swi.sub_indy = indy.swi,
         swi.fa = fa.swi, swi.rj = rj.swi)

swi %>% 
  ggplot(aes(x = swi.sub_indy, y = swi.fa, color = study_param_breadth)) +
  geom_point(alpha = 0.2, size = 3) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, 5))

