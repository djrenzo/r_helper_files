quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

# Computing the mean, standard deviation, kurtosis and skewness of the data.
desc_stats <- function(data){
  data.frame(mean=mean(data), sd.dev = sd(data), min= min(data), 
  max = max(data), skewness = skewness(data), kurtosis = kurtosis(data))}
