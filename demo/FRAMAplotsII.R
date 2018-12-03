chart_Series(XLF)
tmp <- FRAMA(HLC(XLF), FC=4, SC=300, n=126)
add_TA(tmp$FRAMA, col="blue", lwd=3, on=1)

#medians
tmp2 <- runMedian(Cl(XLF), n=126)
tmp3 <- runMedian(Cl(XLF), n=63)
tmp4 <- runMedian(Cl(XLF), n=200)
add_TA(tmp2, col="orange", lwd=2, on=1)
add_TA(tmp3, col="green", lwd=2, on=1)
add_TA(tmp4, col="red", lwd=2, on=1)

#SMAs
tmp5 <- SMA(Cl(XLF), n=126)
tmp6 <- SMA(Cl(XLF), n=63)
tmp7 <- SMA(Cl(XLF), n=200)
add_TA(tmp5, col="orange", lwd=2, on=1)
add_TA(tmp6, col="green", lwd=2, on=1)
add_TA(tmp7, col="red", lwd=2, on=1)

HiLo <- function(HLC, n=20) {
  runHi <- runMax(Hi(HLC), n=n)
  runLo <- runMin(Lo(HLC), n=n)
  out <- (runHi+runLo)/2
  return(out)
}

#HiLo aka Ichimoku
tmp8 <- HiLo(HLC(XLF), n=126)
tmp9 <- HiLo(HLC(XLF), n=63)
tmp10 <- HiLo(HLC(XLF), n=200)
add_TA(tmp8, col="orange", lwd=2, on=1)
add_TA(tmp9, col="green", lwd=2, on=1)
add_TA(tmp10, col="red", lwd=2, on=1)


