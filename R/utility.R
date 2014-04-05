
#Ehlers Price Method
ehlersPriceMethod <- function(HLC, method="Ehlers") {
  if (method=="Close") {
    return (Cl(HLC))
  } else {
    return((Hi(HLC)+Lo(HLC))/2)
  }
}
