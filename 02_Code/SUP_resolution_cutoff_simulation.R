# Simulation to determine resolution cut-off

# medium effect
effect_size <- .5

# sample_size <- 100
sizes <- seq(100, 1000, 50)

# simulation parameters
nrep <- 1000


# simulate
out = list()

for(j in 1:length(sizes)) {
  
  sample_size = sizes[j]
  corrs = seq(-1, 1, .01) ; corrs = corrs[corrs != 0]
  res = array(dim = c(nrep, 2, length(corrs)), dimnames = list(NULL, c("diff", "t"), corrs))
  
  for(c in corrs) {
    
    corr = c
    
    for(i in 1:nrep){
      
      true_a = rnorm(sample_size, 0, 1)
      true_b = rnorm(sample_size, effect_size, 1)
      
      sample_a = sign(corr) * true_a + rnorm(sample_size, 0, sqrt(1 / corr**2 - 1))
      sample_b = sign(corr) * true_b + rnorm(sample_size, 0, sqrt(1 / corr**2 - 1))
      
      res[i, 1, as.character(c)] = mean(sample_b) - mean(sample_a)
      res[i, 2, as.character(c)] = t.test(sample_b, sample_a)$statistic
    
    }
  
  }
  
  out[[j]] = res
  print(paste(sizes[j], "complete"))
  
}

# calculate power
powers <- sapply(out, function(x) colMeans(x[, 2, ] > qt(.95, 2 * sample_size - 2)))

# power > 0.8 (true positives)
pos <- apply(powers, 1, function(x) min(which(x > .8)))
pos[pos > 100] <- NA

plot(corrs, sizes[pos], 
     ylab = "Sample size", ylim = c(0, 1000), 
     xlab = "Correlation", xlim = c(0, 1))
rug(x = seq(from = 0, to = 1, by = 0.01), side = 1, ticksize = -0.02)
rug(x = seq(from = 0, to = 1, by = 0.05), side = 1, ticksize = -0.03)
rug(x = seq(from = 0, to = 1000, by = 50), side = 2, ticksize = -0.02)

# abline(v = seq(0, .7, .1))
abline(v = 0.5, col = "red")
# abline(h = seq(100, 600, 100))
abline(h = 200, col = "red")

# export as A5 landscape pdf: SUP_power_cut_off.pdf


# Cutoffs
# Bias: good = +- .3; under < -.3; over > +.3
# Resolution: really bad = < 0; okish = > 0 & < .5; good = >.5


