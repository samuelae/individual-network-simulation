generate_free_associations <- function(probability_matrix,
                                       cue_set, 
                                       n_responses, 
                                       n_repetitions,
                                       n_subjects) {
  # function to generate free association responses for the cue by using a
  # semantic representation and frequency to output n_responses
  # cue (string): cue that is part of the semantic representation
  # n_responses (integer): number of responses to generate for the cue
  # semantic_representation (igraph): semantic network to use as source of 
  #                                   relatedness
  # word_frequency (named float vector): word frequency for all words in 
  #                                      semantic_representation
  # return (string vector): response words generated to the cue
  
  Rcpp::sourceCpp("02_Code/Functions/Behavioral_Data/gen_associations_cpp.cpp")
  
  # get indices of cue set for cpp function
  inds <- 0:(nrow(probability_matrix) - 1)
  names(inds) <- rownames(probability_matrix)
  cue_inds <- inds[cue_set]
  
  # convert matrix to full matrix
  prob_mat <- as.matrix(probability_matrix)
  
  # generate associations
  asso <- gen_associations_cpp(prob_mat, 
                               cue_inds, 
                               n_responses,
                               n_repetitions,
                               n_subjects)
  
  # collect responses
  out <- cbind(asso[, 1:2],
               matrix(rownames(probability_matrix)[asso[, -c(1:2)] + 1], 
                      ncol = n_responses + 1))
  colnames(out) <- c('subject', 
                     'repetition', 
                     'cue', 
                     paste0('resp_', 1:n_responses))
  responses <- as_tibble(out) %>% readr::type_convert()
  
  # return  propperly formated responses
  return(responses)
}
