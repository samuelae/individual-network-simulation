triangle_score <- function(graph, graph_adj, edges) {
  # calculates triangle scores for all edges in graph
  # for each edge in edges, returns triangle score (sum over all triangles of 
  # the product of all edge weights in each triangle)
  
  tiangle_scores <- numeric(length(edges)) 
  
  for(i in 1:length(edges)){
    
    # get vertices for this edge and respective numeric ids
    ends <- igraph::ends(graph, edges[i], names = FALSE)
    
    # find vertices that form triangles with edge
    triangle_vertex_ids <- intersect(igraph::neighbors(graph, ends[1]), 
                                     igraph::neighbors(graph, ends[2]))
    
    # get triangle scores
    scores <- graph_adj[ends[1], triangle_vertex_ids] * 
      graph_adj[ends[2], triangle_vertex_ids] * 
      graph_adj[ends[1], ends[2]]
    
    # out
    tiangle_scores[i] <- sum(scores)
    
  }
  
  # out
  tiangle_scores
}
