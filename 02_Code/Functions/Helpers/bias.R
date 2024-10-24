# works for positive values only (also no 0s!)
bias <- function(estimated, true,
                 na.rm = TRUE) {
  
  if(na.rm) {
    cc <- complete.cases(estimated, true)
    estimated <- estimated[cc]
    true <- true[cc]
  }
  
  estimated[estimated < 0.01] <- 0.01
  true[true < 0.01] <- 0.01
  
  quotient <-  estimated / true
  
  # Step-by-step calculation with high precision
  log_quotient <- log(quotient)
  mean_log <- mean(log_quotient)
  geometric_mean <- exp(mean_log)
  
  # Output the result
  means <- geometric_mean
  means - 1
  
}