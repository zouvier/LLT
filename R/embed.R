embed <- function(series, dim, lag = 1){
  
  series <- as.data.frame(series)
  len <- nrow(series)
  tmp <- seq(1, len, lag)
  tmp <- tmp[tmp < (len - dim + 2)]
  
  out <- NULL  # Initialize out
  
  for(i in tmp){
    mtx <- series[i:(dim + (i - 1)), 1]
    
    if(is.null(out)){
      out <- mtx
    } else if (!exists("out")) {
       out <- mtx
    }
     else {
      out <- rbind(out, mtx)
    }
  }
  
  out <- t(out) %*% out
  
  return(out)
}
