
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
