cosine = function(P){
  nam = rownames(P)
  C = arma_cosine(P)
  rownames(C) = colnames(C) = nam
  C
}