ACP <- function(x, AvgLength=0, ...) {
  filt <- superSmoother(highPassFilter(x))
  corList <- list()
  length(corList) <- 49
  if(AvgLength==0) {
    for(i in 0:48) {
      if(i %in% c(0, 1, 2)) {
        tmpCor <- runCor(filt, lag(filt,i), 2)
      } else {
        tmpCor <- runCor(filt, lag(filt, i), i)
      }
      corList[[i+1]] <- as.numeric(tmpCor)
    }
  } else {
    for(i in 0:48) {
      tmpCor <- runCor(filt, lag(filt, i), AvgLength)
      corList[[i+1]] <- as.numeric(tmpCor)
    }
  }
  corList <- do.call(cbind, corList)
  corList <- xts(corList, order.by=index(filt))
  
}

