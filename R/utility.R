
#Ehlers Price Method
ehlersPriceMethod <- function(HLC, method="Close") {
  if (method=="Close") {
    return (Cl(HLC))
  } else {
    return((Hi(HLC)+Lo(HLC))/2)
  }
}
