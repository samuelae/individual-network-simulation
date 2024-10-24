generate_rj_words <- function(cue_set, n_trials) {
  # cue_set (chr): vector of cues to use
  # n_trials (int): scalar of number of trials (based on set size and responses)
  # return (list: $words_1, $words_2): list of two character vectors of words to 
  # compare in the relatedness judgments
  
  # helper shifter function to cyclicallly shift words
  shifter <- function(x, n = 1) {
    if (n == 0) x else c(tail(x, -n), head(x, n))
  }
  
  words_1 <- character()
  words_2 <- character()
  cues <- sample(cue_set, length(cue_set))
  iterations <- ceiling(n_trials / length(cue_set))
  shifts <- rep_len(1:(length(cue_set) - 1), length.out = n_trials)
  
  for(iter in 1:iterations) {
    words_1 <- c(words_1, cues)
    words_2 <- c(words_2, shifter(cues, shifts[iter]))
  }
  
  list(words_1 = head(words_1, n_trials),
       words_2 = head(words_2, n_trials))
  
}