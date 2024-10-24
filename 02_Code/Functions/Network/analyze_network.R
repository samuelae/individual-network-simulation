analyze_network <- function(graph, aspl_sample = FALSE, cc_prop = 0) {
  # graph (igraph): igraph object to analyze
  # returns: list with results (tbd)
  
  # CC
  if (cc_prop == 0) {
    cc <- transitivity(graph,
                       type = c("weighted"),
                       isolates = c("NaN"))
  } else {
    threshold <- quantile(edge_attr(graph, "weight"), probs = cc_prop)
    thr_graph <- edge_weight_threshold(graph, threshold)
    cc <- transitivity(thr_graph,
                       type = c("weighted"),
                       isolates = c("NaN"))
    rm(thr_graph); gc();
  }
  
  # average weighted degree
  mean_strength <- mean(strength(graph), na.rm = TRUE)
  
  # modularity
  modularity <- cluster_louvain(graph)$modularity
  
  # aspl
  edge_attr(graph, "weight") <- pmax(1 - edge_attr(graph, "weight"), 0)
  if (aspl_sample == TRUE) {
    nnodes <- vcount(graph)
    sample_a <- sample(1:nnodes, 100)
    sample_b <- sample(1:nnodes, 100)
    dist_sample <- distances(
      graph,
      v = sample_a,
      to = sample_b
    )
    aspl <- mean(dist_sample)
  } else {
    aspl <- mean_distance(graph, directed = FALSE, unconnected = TRUE)
  }

  result <- list(n_nodes = vcount(graph),
                 n_edges = ecount(graph),
                 n_giant_comp = max(components(graph)$csize),
                 mean_strength = mean_strength,
                 cc = mean(cc, na.rm = TRUE),
                 aspl = aspl,
                 mod = modularity,
                 avg_degr = (2 * ecount(graph)) / vcount(graph))
  
  return(result)    
}