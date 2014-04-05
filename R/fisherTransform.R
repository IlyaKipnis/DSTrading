#fisher transform
#http://www.mesasoftware.com/Papers/THE%20INVERSE%20FISHER%20TRANSFORM.pdf
#'Fisher Transform
#'@param x -- an xts or stream of numeric values
#'@return -- an object the same type as above
#'@export
"fish" <- function(x) {
  return(.5*log((1+x)/(1-x)))
}

#inverse fisher transform
#http://www.mesasoftware.com/Papers/THE%20INVERSE%20FISHER%20TRANSFORM.pdf
#'Inverse Fisher Transform
#'@param x -- an xts or stream of numeric values
#'@return -- an object the same type as above
#'@export
"iFish" <- function(x) {
  return((exp(2*x)-1)/(exp(2*x)+1))
}
