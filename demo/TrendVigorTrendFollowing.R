#A demo testing out the Trend Vigor indicator (its vigor computation).
#Theoretically, values between 1 and -1 are prime conditions for oscillator type strategies.
#Thus, values above an absolute value of 1 should be grounds for trying to trend follow.
#It seems that the most critical parameter in this demo is the period parameter in the indicator.

require(DSTrading)
require(quantstrat)

rm(list=ls(.blotter), envir=.blotter)
initDate='1990-12-31'
#initEq=10000

currency('USD')
Sys.setenv(TZ="UTC")

symbols <- c("SPY")

stock(symbols, currency="USD", multiplier=1)
getSymbols(symbols, src='yahoo', 
           index.class=c("POSIXt","POSIXct"),
           from = "1993-01-01", to="2013-12-31", adjust=TRUE)

strategy.st <- portfolio.st <- account.st <- "TVItrendFollowingLong"

rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

######### indicator

add.indicator(strategy.st, name="TVI", arguments=list(x=quote(Cl(mktdata)), period=60, delta=0.2), label="TVI")

########## signals

add.signal(strategy.st, name="sigThreshold", 
           arguments=list(threshold=1, column="vigor.TVI", relationship="gte", cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(threshold=1.4, column="vigor.TVI", relationship="lt", cross=TRUE),
           label="longExit")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(threshold=0.9, column="vigor.TVI", relationship="lt", cross=TRUE),
           label="wrongExit")

########## rules

add.rule(strategy.st, name="ruleSignal", arguments=list(sigcol="longEntry", sigval=TRUE, orderqty=100, ordertype="market", orderside="long", replace=FALSE, prefer="Open"), type="enter", path.dep=TRUE)
add.rule(strategy.st, name="ruleSignal", arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", orderside="long", replace=FALSE, prefer="Open"), type="exit", path.dep=TRUE)
add.rule(strategy.st, name="ruleSignal", arguments=list(sigcol="wrongExit", sigval=TRUE, orderqty="all", ordertype="market", orderside="long", replace=FALSE, prefer="Open"), type="exit", path.dep=TRUE)

########## apply strategy

t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)

########## tradeStats

updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)

(tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=TRUE))
chart.Posn(portfolio.st)
zoom_Chart("1993-06-01::")
