generate_relatedness_judgments <- function(words_1,
                                           words_2,
                                           semantic_representation,
                                           sensitize = FALSE,
                                           sensitivity_representation = 1,
                                           lower_bound = 1,
                                           upper_bound = 20,
                                           integers = TRUE,
                                           sd_noise = 0.15) {
  # This function generates simulated relatedness judgments for each pair of 
  # words (same position) in words_1 and words_2. 
  # words_1, words_2 (chr): vectors of words to judge the relatedness of
  # semantic_representation (igraph): indyNet of the participant as an igraph
  # returns (list): the words and a relatedness judgment on a scale from 1 to 20
  
  # convert graph to cosine matrix
  cosine_matrix <- igraph::as_adjacency_matrix(semantic_representation,
                                               type = "both",
                                               attr = "weight",
                                               names = TRUE,
                                               sparse = TRUE) 
  # extract cosines of the word pairs
  cosines <- as.matrix(cosine_matrix)[cbind(words_1, words_2)]
  
  # sensitize cosines
  if(sensitize) {
    cosines <- cosines ** sensitivity_representation
  }
  
  # predict relatedness with rnorm noise (based on human data)
  prediction <- rnorm(length(cosines), cosines, sd_noise)
  
  # map values below 0 and above 1 to 0 and 1 respectively
  prediction <- pmax(pmin(prediction, 1), 0)

  # rescale
  prediction <- scales::rescale(prediction, c(lower_bound, upper_bound), c(0, 1))

  if(integers) {
    # discretize
    prediction <- round(prediction, 0)
  }

  
  prediction <- tibble::tibble(word_1 = words_1,
                               word_2 = words_2,
                               rel_jud = prediction)
  # return prediction tibble
  prediction
}

