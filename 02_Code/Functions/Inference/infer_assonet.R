infer_assonet = function(asso, ppmi = TRUE, svd = FALSE){
  # create count matrix
  cues = asso$cue %>% unique()
  resp = asso %>% select(starts_with("resp")) %>% unlist() %>% unique()
  counts = matrix(0,
                  nrow = length(cues),
                  ncol = length(resp),
                  dimnames = list(cues, resp))
  # fill count matrix
  for(i in 1:3){
    tmp = asso %>%
      group_by(cue, across(all_of(paste0("resp_",i)))) %>%
      summarize(n = n()) %>%
      as.matrix() %>%
      suppressMessages()
    counts[tmp[,1:2]] = counts[tmp[,1:2]] + as.numeric(tmp[,3])
  }
  # transform counts to ppmi
  if(ppmi){
    counts = counts / sum(counts)
    counts = counts / (rowSums(counts) %*% t(colSums(counts)))
    counts[counts<0] = 0
  }
  # svd if specified (10% left)
  if(svd) {
    counts <- svd(counts)
    counts <- diag(counts$d) %*% counts$u
    n_cols <- ceiling(ncol(counts) / 10)
    counts <- subset(counts, select = 1:n_cols, drop = FALSE)
    rownames(counts) <- cues
  }
  # do cosine
  norm = sqrt(rowSums(counts ** 2))
  cos = counts %*% t(counts) / (norm %*% t(norm))
  
  # remove negative and NaN values
  cos[(cos < 0) | is.nan(cos)] <- 0
  
  cos
}
# by Dirk (modified by Samuel)