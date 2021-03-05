# Simple function to estimate bootsrap confidence interval for a single estimate, e.g., mean using precentile method:
bs.precentile <- function(x, statistic, B, conf.level) {
  
  n <- length(x)
  b <- replicate(B,statistic(sample(x,n,T)))
  s <- statistic(x)
  q <- quantile(b, probs = c(1 - ((1 - conf.level) / 2),(1 - conf.level) / 2))
  bs.ci <- c(s,q)
  names(bs.ci)[1] <- 'estimate'
  lab <- gsub(x=match.call()[3],'\\[a-z]','')
  lab <- paste0('confidence_interval_',lab)
  out <- list(bs.ci, bs_estimates = b)
  names(out)[1] <- lab
  
  
  
  return(out)
  
}

# Example
bs.precentile(x = iris$Sepal.Length / iris$Sepal.Width,
   statistic = IQR,
   B = 1000,
   conf = 0.95)
