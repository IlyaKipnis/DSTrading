
#Ehlers Price Method
ehlersPriceMethod <- function(HLC, method="Close") {
  if(dim(HLC)[2]==1 & !"Close" %in% colnames(HLC)) {
    return(HLC)
  }
  if (method=="Close") {
    return (Cl(HLC))
  } else {
    return((Hi(HLC)+Lo(HLC))/2)
  }
}

highPassFilter <- function(x) {
  alpha1 <- (cos(.707*2*pi/48)+sin(.707*2*pi/48)-1)/cos(.707*2*pi/48)
  HP <- (1-alpha1/2)*(1-alpha1/2)*(x-2*lag(x)+lag(x,2))
  HP <- HP[-c(1,2)]
  HP <- filter(HP, c(2*(1-alpha1), -1*(1-alpha1)*(1-alpha1)), method="recursive")
  HP <- c(NA, NA, HP)
  HP <- xts(HP, order.by=index(x))
  return(HP)
}

superSmoother <- function(x) {
  a1 <- exp(-1.414*pi/10)
  b1 <- 2*a1*cos(1.414*pi/10)
  c2 <- b1
  c3 <- -a1*a1
  c1 <- 1-c2-c3
  filt <- c1*(x+lag(x))/2
  leadNAs <- sum(is.na(filt))
  filt <- filt[-c(1:leadNAs)]
  filt <- filter(filt, c(c2, c3), method="recursive")
  filt <- c(rep(NA,leadNAs), filt)
  filt <- xts(filt, order.by=index(x))
}
