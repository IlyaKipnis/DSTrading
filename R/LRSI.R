#'Laguerre RSI
#'@param HLC -- an xts object containing High, Low, and Close price data
#'@param gamma -- a dampening factor
#'@param priceMethod -- use "Close" for the close, any other string will result in Ehlers's (H+L)/2 method.
#'@return an xts object
#'@references
#'\cr \url{http://www.mesasoftware.com/Papers/TIME\%20WARP.pdf}\cr
#'@export
"LRSI" <- function(HLC, gamma=.5, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  L0 <- xts(stats::filter(x=(1-gamma)*price,filter=gamma, method="recursive"), order.by=index(price))
  lagL0 <- lag(L0); lagL0[1] <- 0;
  L1 <- xts(stats::filter(x=-gamma*L0+lagL0,filter=gamma, method="recursive"), order.by=index(price))
  lagL1 <- lag(L1); lagL1[1] <- 0;
  L2 <- xts(stats::filter(x=-gamma*L1+lagL1,filter=gamma, method="recursive"), order.by=index(price))
  lagL2 <- lag(L2); lagL2[1] <- 0;
  L3 <- xts(stats::filter(x=-gamma*L2+lagL2,filter=gamma, method="recursive"), order.by=index(price))
  CU <- CD <- xts(rep(0, length(price)), order.by=index(price))
  L01diff <- L0-L1
  L12diff <- L1-L2
  L23diff <- L2-L3
  ups <- downs <- cbind(L01diff,L12diff,L23diff)
  ups[ups < 0] <- 0
  downs[downs > 0] <- 0
  CU <- rowSums(ups)
  CD <- rowSums(downs)*-1
  out <- xts(CU/(CU+CD)*100,order.by=index(price))
  return(out) 
}


#Generalized Laguerre RSI
#Modified version of Laguerre RSI to control n
#Use at own risk
#'Generalized Laguerre RSI
#'@param HLC -- an xts object containing High, Low, and Close price data
#'@param n -- the period for computation
#'@param gamma -- a dampening factor
#'@param priceMethod -- use "Close" for the close, any other string will result in Ehlers's (H+L)/2 method.
#'@return an xts object
#'@references
#'\cr \url{http://www.mesasoftware.com/Papers/TIME\%20WARP.pdf}\cr
#'@export
"GLRSI" <- function(HLC, n=4, gamma=.5, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  tmp <- NULL
  for(i in 1:n) {
    if (i==1) {
      tmp2 <- xts(stats::filter(x=(1-gamma)*price, filter=gamma, method="recursive"), order.by=index(price)) 
    } else {
      tmp2 <- xts(stats::filter(x=-gamma*tmp2+lagTmp2, filter=gamma, method="recursive"), order.by=index(price)) 
    }
    lagTmp2 <- lag(tmp2); lagTmp2[1] <- 0;
    tmp <- cbind(tmp, tmp2)
  }
  diff <- tmp[,-ncol(tmp)]-tmp[,-1]
  CU <- diff; CU[CU<0] <- 0; CU <- rowSums(CU);
  CD <- diff; CD[CD>0] <- 0; CD <- rowSums(CD)*-1;
  out <- xts(CU/(CU+CD)*100, order.by=index(price))
  return(out)
}