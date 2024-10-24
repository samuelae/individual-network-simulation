sample_dac = function(xs, ps, n, gran = 1000) {
  # settings
  gran = min(n, gran)
  # cut index
  ind = 1:length(ps)
  gr = cut(ind, gran)
  # split indices
  is = split(ind, gr)
  # determine group proportions
  props = tapply(ps, gr, sum) / sum(ps)
  props[is.na(props)] = 0
  #sample
  inds = list()
  diffs = c()
  for(i in 1:gran){
    n_target = round(props[i] * n)
    if(props[i] > 0) n_draw = min(rbinom(1,n,props[i]),length(is[[i]])) else n_draw = 0
    diff = n_draw/n - props[i]
    props[i] = props[i] + diff
    if(i < gran) props[i+1] = props[i+1] - diff
    if(n_draw > 0){
      inds[[i]] = sample(is[[i]], n_draw, replace=FALSE)
    }
  }
  xs[unlist(inds)]
}
# made by Dirk