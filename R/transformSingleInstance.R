transformSingleSeries <- function(single_series, train_law, select = "var", lag = 11){
  
  # Selection function based on the 'select' parameter
  if(select == "mean"){
    sfun <- function(x) {
      x <- as.data.frame(x[,which.min(abs(sapply(as.data.frame(x),FUN = mean)))])
      return(x)
    }
  }else if(select == "var"){
    sfun <- function(x) {
      x <- as.data.frame(x[,which.min(sapply(as.data.frame(x),FUN = var))])
      return(x)
    }
  }else{
    sfun <- function(x){
      a <- rank(abs(colSums((x))),ties.method = "min")
      b <- rank(sapply(as.data.frame(x),FUN = var),ties.method = "min")
      x <- as.data.frame(x[,which.min(a + b)])
      return(x)
    }
  }

  dim <- nrow(train_law)
  
  # Embed the single series
  emb <- LLT::embed(single_series, dim, lag)
  
  # Initialize an empty matrix for the transformed series
  transformed_series <- matrix(0, nrow = nrow(emb), ncol = ncol(train_law))
  
  # Transform the single series based on the linear laws
  for(l in 1:ncol(train_law)){
    tmp2 <- emb %*% matrix(train_law[,l], ncol = 1)
    transformed_series[,l] <- tmp2[,1]
  }
  
  # Apply the selection function
  transformed_series <- sfun(transformed_series)
  
  return(transformed_series)
}
