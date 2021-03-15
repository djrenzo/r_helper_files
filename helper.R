quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

# Computing the mean, standard deviation, kurtosis and skewness of the data.
desc_stats <- function(data){
  data.frame(mean=mean(data), sd.dev = sd(data), min= min(data), 
  max = max(data), skewness = skewness(data), kurtosis = kurtosis(data))
}

to_markdown <- function(m, cust_row, cust_names, dig){
  m = matrixreg(m,
          custom.gof.rows = cust_row,
          custom.model.names = cust_names, 
          digits = dig)
  colnames(m) = m[1,]
  rownames(m) = m[,1]
  return(m[-1,-1])
}

to_markdown_k <- function(m, cust_row, cust_names, dig){
  m = matrixreg(m,
          custom.gof.rows = cust_row,
          custom.model.names = cust_names, 
          digits = dig)
  colnames(m) = m[1,]
  rownames(m) = m[,1]
  return(knitr::kable(m[-1,-1]))
}
