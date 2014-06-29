require(DSTrading)
require(IKTrading)
require(quantstrat)

initDate="1990-01-01"
from="2003-01-01"
to="2010-12-31"
options(width=70)

#to rerun the strategy, rerun everything below this line
source("demoData.R") #contains all of the data-related boilerplate.

#trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "FRAMA_I"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#Parameters

FC=4
SC=300
n=126
triggerLag=1
pctATR=.02
period=10

#indicators

add.indicator(strategy.st, name="FRAMA", 
              arguments=list(HLC=quote(HLC(mktdata)),n=n, 
                             FC=FC, SC=SC, triggerLag=triggerLag),
              label="frama")

add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), n=period), 
              label="atrX")


#signals

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "FRAMA.frama"), relationship="gte"),
           label="longEntry")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "FRAMA.frama"), relationship="lt"),
           label="longExit")

#rules

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open", osFUN=osDollarATR,
                        tradeSize=tradeSize, pctATR=pctATR, atrMod="X"), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
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
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio))

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
                 main=paste0("FRAMA FC=", FC, ", SC=", SC, ", n=", n, ", ATR period=", period),
                 colors=c("green","red"))

SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

chart.Posn(portfolio.st, "XLF")
tmp <- FRAMA(HLC(XLF), n=n, FC=FC, SC=SC, triggerLag=triggerLag)
add_TA(tmp$FRAMA, on=1, col="blue", lwd=2)
add_TA(tmp$trigger, on=1, col="red", lwd=1)
tmp2 <- lagATR(HLC=HLC(XLF), n=period)
add_TA(tmp2$atr, col="purple", lwd=2)


