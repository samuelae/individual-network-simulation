make_probability_matrix <- function(semantic_representation, 
                                    sensitivity_representation,
                                    word_frequency, 
                                    sensitivity_frequency) {
  
  common_words <- intersect(names(V(semantic_representation)), 
                            word_frequency %>% pull(word))
  
  # representation
  vectorspace <- as_adjacency_matrix(semantic_representation,
                                     type = "both",
                                     attr = "weight",
                                     names = TRUE,
                                     sparse = TRUE)
  vectorspace <- vectorspace[common_words, common_words]
  vectorspace <- (vectorspace - min(vectorspace)) / 
    (max(vectorspace) - min(vectorspace))
  vectorspace <- vectorspace ** sensitivity_representation
  vectorspace <- vectorspace / Matrix::rowSums(vectorspace)
  
  # frequency
  freq_prob <- word_frequency %>% pull(probability, word)
  freq_prob <- freq_prob ** sensitivity_frequency
  freq_prob <- freq_prob[common_words]
  freq_prob <- freq_prob[rownames(vectorspace)]
  word_frequency <- matrix(rep(freq_prob, times = length(freq_prob)), 
                           byrow = TRUE, 
                           ncol = length(freq_prob))
  rownames(word_frequency) <- colnames(word_frequency) <- rownames(vectorspace)
  diag(word_frequency) <- 0
  word_frequency <- word_frequency / rowSums(word_frequency)
  
  # probability matrix
  probability_matrix <- vectorspace * word_frequency
  
  return(probability_matrix)
}