embed <- function(series, dim, lag = 1){
  
  series <- as.data.frame(series)
  len <- nrow(series)
  tmp <- seq(1, len, lag)
  tmp <- tmp[tmp < (len - dim + 2)]
  
  print(paste("Length of series:", len))
  print(paste("Length of tmp:", length(tmp)))
  
  out <- NULL  # Initialize out
  
  for(i in tmp){
    mtx <- series[i:(dim + (i - 1)), 1]
    
    if(is.null(out)){
      out <- mtx
    } else {
      out <- rbind(out, mtx)
    }
  }
  
  if(is.null(out)){
    stop("The 'out' matrix was not populated. Check the input series and parameters.")
  }
  
  out <- t(out) %*% out
  
  return(out)
}
