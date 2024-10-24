generate_cue_set <- function(graph_am, 
                             set_size, 
                             breadth = c("broad", "mixed", "narrow")) {
  # generates a cue set of size set_size from the words in the graph
  # graph:    graph adjacency matrix, must be a connected graph!
  # set_size: int for cue set size
  # breadth:  "broad": random sample from nodes in graph
  #           "narrow": random sample from neighbors of a random origin word
  #           "mixed": multiple smaller "narrow" sets combined
  # return:   character vector of cues
  
  make_narrow_set <- function(origin, graph_am, size) {
    cue_set <- origin
    for(i in 2:size) {
      sub <- graph_am[cue_set, !colnames(graph_am) %in% cue_set, drop = FALSE] %>% 
        as.matrix()
      sub <- t(t(sub) * 1/(1:length(cue_set)))
      new <- sort(colMeans(sub), decreasing = TRUE) %>% `[`(1)
      cue_set <- c(cue_set, names(new))
    }
    cue_set
  }
  
  make_broad_set <- function(graph_am, size) {
    cue_set <- sample(colnames(graph_am), size = 1)
    for (i in 2:size) {
      sub <- graph_am[cue_set[i - 1], ]
      options <- sub[sub != 0][!(names(sub[sub != 0]) %in% cue_set)]
      new <- sample(names(options), size = 1)
      cue_set <- c(cue_set, new)
    }
    cue_set
  }
  
  cue_set <- character(length = 0L)
  
  switch (breadth,
    "broad" = {
      cue_set <- make_broad_set(graph_am, set_size)
    },
    "narrow" = {
      origin <- sample(colnames(graph_am), 1)
      cue_set <- make_narrow_set(origin, graph_am, set_size)
    },
    "mixed" = {
      n_origins <- floor(sqrt(set_size))
      origins <- make_broad_set(graph_am, n_origins)
      # generate balanced lengths for sub-cue sets adding up to set_size
      subset_sizes <- unname(unlist(lapply(split(rep_len(1, set_size), 
                                                 rep_len(1:n_origins, set_size)), 
                                           length)))
      # create sub-cue sets and combine into one cue set
      cue_set <- unlist(purrr::map2(origins, subset_sizes, 
                                    make_narrow_set, graph_am = graph_am))
    }
  )
  # output cue set
  cue_set
}
