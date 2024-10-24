get_ranks <- function(for_cue, 
                      swow_first_response_counts, 
                      model_first_response_counts) {
  top_3_model <- model_first_response_counts %>% 
    filter(cue == for_cue) %>% 
    arrange(desc(n)) %>% 
    slice_head(n = 3) %>% 
    pull(response)
  swow_responses <- swow_first_response_counts %>% 
    filter(cue == for_cue) %>% 
    arrange(desc(n)) %>%
    pull(n, response)
  swow_ranks <- rank(-swow_responses)
  
  ranks <- c(ifelse(any(names(swow_responses) == top_3_model[1]), 
                    swow_ranks[names(swow_ranks) == top_3_model[1]] %>% as.numeric(), 
                    NA),
             ifelse(any(names(swow_responses) == top_3_model[2]), 
                    swow_ranks[names(swow_ranks) == top_3_model[2]] %>% as.numeric(), 
                    NA),
             ifelse(any(names(swow_responses) == top_3_model[3]), 
                    swow_ranks[names(swow_ranks) == top_3_model[3]] %>% as.numeric(), 
                    NA))
  
  return(ranks)
}