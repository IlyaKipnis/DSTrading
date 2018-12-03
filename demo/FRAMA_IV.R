require(DSTrading)
require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)

initDate="1990-01-01"
from="2010-03-01"
to=as.character(Sys.Date())
options(width=70)

triggerSeries <- function(FUN="SMA", x, n=10, triggerLag=1) {
  fun <- match.fun(FUN)
  tmp <- fun(x, n=n)
  trigger <- lag(tmp, triggerLag)
  out <- cbind(series=tmp, trigger=trigger)
  colnames(out) <- c("series", "trigger")
  return(out)
}

#to rerun the strategy, rerun everything below this line
source("demoData.R") #contains all of the data-related boilerplate.

#trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "FRAMA_IV"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters

FC=4
SC=300
n=126
triggerLag=1
nConfirm=200

period=10
pctATR=.02

#indicators 

add.indicator(strategy.st, name="FRAMA",
              arguments=list(HLC=quote(HLC(mktdata)), n=n, 
                             SC=SC, FC=FC, triggerLag=triggerLag),
              label="primary")

add.indicator(strategy.st, name="triggerSeries",
              arguments=list(FUN="runMedian", x=quote(Cl(mktdata)), 
                             n=nConfirm, triggerLag=triggerLag),
              label="confirmatory")

add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), n=period), 
              label="atrX")

#long signals

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("FRAMA.primary", "series.confirmatory"), 
                          relationship="gt"),
           label="FRAMAgtConfirm")

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("FRAMA.primary", "trigger.primary"), 
                          relationship="gt"),
           label="FRAMArising")

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("series.confirmatory", "trigger.confirmatory"),
                          relationship="gt"),
           label="confirmRising")
 
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("Close", "FRAMA.primary"), 
                          relationship="gt"),
           label="ClGtFRAMA")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("FRAMAgtConfirm", "FRAMArising", 
                                    "confirmRising", "ClGtFRAMA"), 
                          cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "FRAMA.primary"), 
                          relationship="lt"),
           label="longExit")

#long rules

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open", osFUN=osDollarATR,
                        tradeSize=tradeSize, pctATR=pctATR, atrMod="X"), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

#short signals

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("FRAMA.primary", "series.confirmatory"), 
                          relationship="lt"),
           label="FRAMAltConfirm")

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("FRAMA.primary", "trigger.primary"), 
                          relationship="lt"),
           label="FRAMAfalling")

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("series.confirmatory", "trigger.confirmatory"),
                          relationship="lt"),
           label="confirmFalling")

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("Close", "FRAMA.primary"), 
                          relationship="lt"),
           label="ClLtFRAMA")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("FRAMAltConfirm", "FRAMAfalling",
                                    "confirmFalling", "ClLtFRAMA"), 
                          cross=TRUE),
           label="shortEntry")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "FRAMA.primary"), 
                          relationship="gt"),
           label="shortExit")

#short rules

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="shortEntry", sigval=TRUE, ordertype="market", 
                        orderside="short", replace=FALSE, prefer="Open", osFUN=osDollarATR,
                        tradeSize=-tradeSize, pctATR=pctATR, atrMod="X"), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="shortExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="short", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

#apply strategy
t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)


#set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)


#trade statistics
tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Percent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm=TRUE))

#daily and duration statistics
dStats <- dailyStats(Portfolios = portfolio.st, use="Equity")
rownames(dStats) <- gsub(".DailyEndEq","", rownames(dStats))
print(data.frame(t(dStats)))
durStats <- durationStatistics(Portfolio=portfolio.st, Symbols=sort(symbols))
print(t(durStats))

#market exposure
tmp <- list()
length(tmp) <- length(symbols)
for(i in 1:nrow(dStats)) {
  totalDays <- nrow(get(rownames(dStats)[i]))
  mktExposure <- dStats$Total.Days[i]/totalDays
  tmp[[i]] <- c(rownames(dStats)[i], round(mktExposure, 3))
}
mktExposure <- data.frame(do.call(rbind, tmp))
colnames(mktExposure) <- c("Symbol","MktExposure")
print(mktExposure)
print(mean(as.numeric(as.character(mktExposure$MktExposure))))

#portfolio cash PL
portString <- paste0("portfolio.", portfolio.st)
portPL <- .blotter[[portString]]$summary$Net.Trading.PL

#Cash Sharpe
(SharpeRatio.annualized(portPL, geometric=FALSE))

#Portfolio comparisons to SPY
instRets <- PortfReturns(account.st)

#Correlations
instCors <- cor(instRets)
diag(instRets) <- NA
corMeans <- rowMeans(instCors, na.rm=TRUE)
names(corMeans) <- gsub(".DailyEndEq", "", names(corMeans))
print(round(corMeans,3))
mean(corMeans)

portfRets <- xts(rowMeans(instRets)*ncol(instRets), order.by=index(instRets))
portfRets <- portfRets[!is.na(portfRets)]
cumPortfRets <- cumprod(1+portfRets)
firstNonZeroDay <- as.character(index(portfRets)[min(which(portfRets!=0))])
getSymbols("SPY", from=firstNonZeroDay, to=to)
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1+SPYrets)
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison)  <- c("strategy", "SPY")
chart.TimeSeries(comparison, legend.loc = "topleft",
                 colors=c("green","red"))
chart.RelativePerformance(portfRets,SPYrets)

SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)


myTheme<-chart_theme()
myTheme$col$dn.col<-'blue'
myTheme$col$dn.border <-'blue'
myTheme$col$up.border <-'blue'

chart.Posn(portfolio.st, "EWJ", theme=myTheme)
tmp <- FRAMA(HLC(EWJ), n=n, FC=FC, SC=SC, triggerLag=triggerLag)
add_TA(tmp$FRAMA, on=1, col="purple", lwd=3)
add_TA(tmp$trigger, on=1, col="red", lwd=0.5)
tmp2 <- triggerSeries(FUN="runMedian", x=Cl(EWJ), n=nConfirm)
add_TA(tmp2$series, on=1, col="orange", lwd=3)
tmp3 <- lagATR(HLC=HLC(EWJ), n=period)
add_TA(tmp3$atr, col="purple", lwd=2)
