small_world_index <- function(graph, prop = 0.5) {
  
  if ("weight" %in% edge_attr_names(graph)) {
    graph <- delete_edge_attr(edge_weight_threshold(graph, quantile(E(graph)$weight, prop)), "weight")
  }
  smallworldIndex(graph)
  
}