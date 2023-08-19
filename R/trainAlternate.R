transformRealData <- function(real_data_files, train_law, select = "rank", lag = 1){
  
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
  s <- 0
   
  for (file in real_data_files) {
    dt <- read.csv(file, sep="\t", header = TRUE)

    for(k in 1:ncol(dt)){
      cn <- colnames(dt)[k]
      idx <- sub("\\#.*","",colnames(train_law)) == cn
      tmp <- as.matrix(train_law[,idx])
      emb <- LLT::embed(dt[,k],dim,lag)

      # Assuming you want to apply the transformation for each column in train_law
      for(l in 1:ncol(train_law)){
        tmp2 <- as.matrix(tmp[,l])
        tmp2 <- emb %*% tmp2
        tmp2 <- sfun(tmp2)
        colnames(tmp2) <- paste(cn, "_", l, sep="")

        if(!exists("tmp3")){
          tmp3 <- tmp2
        }else{
          tmp3 <- cbind(tmp3, tmp2)
        }
      }
    }

    if(!exists("out")){
      out <- tmp3
      rm(tmp3)
    }else{
      out <- rbind(tmp3, out)
      rm(tmp3)
    }
    
    s <- s + 1
  }

  out <- as.data.frame(out)
  colnames(out) <- make.unique(colnames(out), sep="_")
  rownames(out) <- 1:nrow(out)

  return(out)
}
