#'Fisher Transform
#'@param x an xts or stream of numeric values
#'@return an object the same type as above
#'@references
#'\cr \url{http://www.mesasoftware.com/Papers/THE\%20INVERSE\%20FISHER\%20TRANSFORM.pdf}\cr
#'@export
"fish" <- function(x) {
  return(.5*log((1+x)/(1-x)))
}

#'Inverse Fisher Transform
#'@param x an xts or stream of numeric values
#'@return an object the same type as above
#'@references
#'\cr \url{http://www.mesasoftware.com/Papers/THE\%20INVERSE\%20FISHER\%20TRANSFORM.pdf}\cr
#'@export
"iFish" <- function(x) {
  return((exp(2*x)-1)/(exp(2*x)+1))
}
