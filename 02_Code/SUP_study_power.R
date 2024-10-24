# Simulation to determine resolution cut-off

# medium effect
effect_size <- .5

# sample_size
sizes <- seq(80, 400, 10)

# simulation parameters
nrep <- 5000


# simulate
out = list()

for(j in 1:length(sizes)) {

  tictoc::tic()  
  sample_size = sizes[j]
  corrs = seq(0.3, 0.8, .01) ; corrs = corrs[corrs != 0]
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
  tictoc::toc()
  
}

# calculate power
powers <- sapply(out, function(x) colMeans(x[, 2, ] > qt(.95, 2 * sample_size - 2)))

# power > 0.8 (true positives)
pos <- apply(powers, 1, function(x) min(which(x > .8)))
pos[pos > 100] <- NA

# plot
plot(corrs, sizes[pos], 
     ylab = "Sample size", ylim = c(0, 400), 
     xlab = "Resolution", xlim = c(0.3, 0.8),
     main = expression("Minimal sample sizes per group to detect d = 0.5 effect with" ~ 1 - beta >= .8))
rug(x = seq(from = 0.3, to = 0.8, by = 0.01), side = 1, ticksize = -0.02)
rug(x = seq(from = 0, to = 400, by = 10), side = 2, ticksize = -0.02)
abline(v = 0.71, col = "red")
abline(v = 0.40, col = "blue")
abline(h = 110, col = "red", lty = 2)
abline(h = 320, col = "blue", lty = 2)

# exported as A5 landscape





