infer_rjnet <- function(rj_data, cueset = NULL) {
  
  mean_data <- rj_data %>% 
    mutate(id = if_else(word_1 < word_2, 
                        paste(word_1, word_2, sep = "_"), 
                        paste(word_2, word_1, sep = "_"))) %>% 
    group_by(id) %>% 
    summarise(mean_rj = mean(rel_jud))
  
  mean_data <- mean_data %>% 
    bind_cols(str_split(mean_data$id, "_", simplify = TRUE)[, 1],
              str_split(mean_data$id, "_", simplify = TRUE)[, 2],
              scales::rescale(mean_data$mean_rj, c(0, 1), c(1, 20))) %>% 
    rename(word_1 = "...3",
           word_2 = "...4",
           weight = "...5")
  
  # this is a workaround for rj_data, that does not include all cues
  if(!is.null(cueset)) {
    nodes <- cueset
  } else {
    nodes <- unique(c(mean_data$word_1, mean_data$word_2))
  }
  
  adj_mat <- matrix(0, nrow = length(nodes), ncol = length(nodes))
  rownames(adj_mat) <- colnames(adj_mat) <- nodes
  
  adj_mat[cbind(mean_data$word_1, mean_data$word_2)]<- mean_data$weight
  adj_mat[cbind(mean_data$word_2, mean_data$word_1)]<- mean_data$weight
  
  return(adj_mat)
}
# by Samuel