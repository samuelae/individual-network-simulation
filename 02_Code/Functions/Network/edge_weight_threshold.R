edge_weight_threshold <- function(graph, threshold) {
  ids_below <- (1:igraph::ecount(graph))[igraph::edge_attr(graph, "weight") < threshold]
  graph <- igraph::delete_edges(graph, ids_below)
  graph
}
