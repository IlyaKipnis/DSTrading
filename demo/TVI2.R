require(DSTrading)
require(IKTrading)
require(quantstrat)

initDate="1990-01-01"
from="2003-01-01"
to="2010-12-31"

#to rerun the strategy, rerun everything below this line
source("demoData.R") #contains all of the data-related boilerplate.

#trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "TVI_TF_2"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, 
         currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters (trigger lag unchanged, defaulted at 1)
delta=0
period=20

#indicators
add.indicator(strategy.st, name="TVI", arguments=list(x=quote(Cl(mktdata)), 
                                                      period=period, delta=delta), label="TVI")

#signals
add.signal(strategy.st, name="sigThreshold", 
           arguments=list(threshold=1, column="vigor.TVI", 
                          relationship="gte", cross=FALSE),
           label="TVIgtThresh")
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("vigor.TVI","trigger.TVI"), 
                          relationship="gt"),
           label="TVIgtLag")
add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("TVIgtThresh","TVIgtLag"), 
                          cross=TRUE),
           label="longEntry")
add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("vigor.TVI","trigger.TVI"), 
                          relationship="lt"),
           label="longExit")

#rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, orderqty=100, 
                        ordertype="market", orderside="long", 
                        replace=FALSE, prefer="Open", osFUN=osMaxDollar,
                        tradeSize=10000, maxSize=10000), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="long", replace=FALSE, 
                        prefer="Open"), 
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

#tradeStats
tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Percent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio))

summary(tStats$Gross.Profit)

#dailyStats
dStats <- dailyStats(Portfolios = portfolio.st, use="Equity")
rownames(dStats) <- gsub(".DailyEndEq","", rownames(dStats))
print(data.frame(t(dStats)))
mean(tStats$Percent.Positive)
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Percent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio))

#portfolio cash PL
portPL <- .blotter$portfolio.TVI_TF_2$summary$Net.Trading.PL

#Cash Sharpe
(SharpeRatio.annualized(portPL, geometric=FALSE))

instRets <- PortfReturns(account.st)
portfRets <- xts(rowMeans(instRets)*ncol(instRets), order.by=index(instRets))
cumPortfRets <- cumprod(1+portfRets)-1
firstNonZeroDay <- index(portfRets)[min(which(portfRets!=0))]
getSymbols("SPY", from=firstNonZeroDay, to="2010-12-31")
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1+SPYrets)-1
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison)  <- c("strategy", "SPY")
chart.TimeSeries(comparison, legend.loc = "topleft", 
                 main=paste0("Period=", period, ", Delta=",delta))

SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

chart.Posn(portfolio.st, "XLB")
tmp <- TVI(Cl(XLB), period=period, delta=delta)
add_TA(tmp$vigor)
add_TA(tmp$trigger, on=5, col="red")