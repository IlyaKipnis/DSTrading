require(rugarch)
require(quantmod)
require(TTR)
require(PerformanceAnalytics)

# get SPY data from Yahoo 
getSymbols("SPY", from = '1990-01-01')

spyRets = na.omit(Return.calculate(Ad(SPY)))

# GJR garch with AR1 innovations under a skewed student T distribution for returns
gjrSpec = ugarchspec(mean.model = list(armaOrder = c(1,0)),
                      variance.model = list(model = "gjrGARCH",
                                            variance.targeting = TRUE),
                      distribution.model = "sstd")

# Use rolling window of 504 days, refitting the model every 22 trading days
t1 = Sys.time()
garchroll = ugarchroll(gjrSpec, data = spyRets, 
                        n.start = 504, refit.window = "moving",  refit.every = 22)
t2 = Sys.time()
print(t2-t1)

# convert predictions to data frame
garchroll = as.data.frame(garchroll)

getSymbols('^VIX', from = '1990-01-01')

# convert GARCH sigma predictions to same scale as the VIX by annualizing, multiplying by 100
garchPreds = xts(garchroll$Sigma * sqrt(252) * 100, order.by=as.Date(rownames(garchroll)))
diff = garchPreds - Ad(VIX)

require(downloader)

download("https://www.dropbox.com/s/y3cg6d3vwtkwtqx/VXZlong.TXT?raw=1", destfile="VXZlong.txt")
download("https://www.dropbox.com/s/jk3ortdyru4sg4n/ZIVlong.TXT?raw=1", destfile="ZIVlong.txt")

ziv = xts(read.zoo("ZIVlong.txt", format='%Y-%m-%d', sep = ',', header=TRUE))
vxz = xts(read.zoo("VXZlong.txt", format = '%Y-%m-%d', sep = ',', header = TRUE))

zivRets = na.omit(Return.calculate(Cl(ziv)))
vxzRets = na.omit(Return.calculate(Cl(vxz)))
vxzRets['2014-08-05'] = .045


zivSig = diff < 0
vxzSig = diff > 0

garchOut = lag(zivSig, 2) * zivRets + lag(vxzSig, 2) * vxzRets

histSpy = runSD(spyRets, n = 21, sample = FALSE) * sqrt(252) * 100
spyDiff = histSpy - Ad(VIX)

zivSig = spyDiff < 0
vxzSig = spyDiff > 0

spyOut = lag(zivSig, 2) * zivRets + lag(vxzSig, 2) * vxzRets

avg = (garchOut + spyOut)/2
compare = na.omit(cbind(garchOut, spyOut, avg))
colnames(compare) = c("gjrGARCH", "histVol", "avg")




stratStats <- function(rets) {
  stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
  stats[5,] <- stats[1,]/stats[4,]
  stats[6,] <- stats[1,]/UlcerIndex(rets)
  rownames(stats)[4] <- "Worst Drawdown"
  rownames(stats)[5] <- "Calmar Ratio"
  rownames(stats)[6] <- "Ulcer Performance Index"
  return(stats)
}

charts.PerformanceSummary(compare)
stratStats(compare)
