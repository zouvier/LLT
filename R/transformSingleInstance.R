transformSingleInstance <- function(real_data_file, train_law, select = "var", lag = 11){
  
  sfun <- switch(select,
                 mean = function(x) x[, which.min(abs(colMeans(x)))],
                 var = function(x) x[, which.min(apply(x, 2, var))],
                 {
                   a <- rank(abs(colSums(x)), ties.method = "min")
                   b <- rank(apply(x, 2, var), ties.method = "min")
                   x[, which.min(a + b)]
                 })

  dim_law <- nrow(train_law)
  
  dt <- read.csv(real_data_file, sep="\t", header = TRUE)
  out_matrix <- matrix(0, nrow = dim_law, ncol = ncol(dt) * ncol(train_law))

  for(k in 1:ncol(dt)){
    cn <- colnames(dt)[k]
    idx <- sub("\\#.*", "", colnames(train_law)) == cn
    tmp <- as.matrix(train_law[, idx])
    emb <- LLT::embed(dt[, k], dim_law, lag)

    for(l in 1:ncol(train_law)){
      tmp2 <- as.matrix(tmp[, l])
      tmp2_transformed <- emb %*% tmp2
      out_matrix[, (k-1)*ncol(train_law) + l] <- sfun(tmp2_transformed)
    }
  }

  out_df <- as.data.frame(out_matrix)
  return(out_df)
}
