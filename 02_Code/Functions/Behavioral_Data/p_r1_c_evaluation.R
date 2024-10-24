p_r1_c_evaluation <- function(swow_first_response_counts,
                              model_first_response_counts,
                              cor_method = "pearson",
                              usable_words) {
  
  swow_r1 <- swow_first_response_counts %>% 
    filter(cue %in% usable_words) %>% 
    group_by(cue) %>% 
    mutate(p_resp = n / sum(n))
  
  model_r1 <- model_first_response_counts %>% 
    group_by(cue) %>% 
    mutate(p_resp = n / sum(n))
  
  # join response probabilities
  joined_response_probabilities <- swow_r1 %>% 
    left_join(model_r1, by = c("cue", "response"), suffix = c("_swow", "_model"))
  
  # calculate correlation coefficient for complete observations
  correlation <- cor(joined_response_probabilities$p_resp_swow, 
                     joined_response_probabilities$p_resp_model, 
                     use = "complete.obs",
                     method = cor_method)
  
  return(correlation)
}