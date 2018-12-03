#A demo testing out the Trend Vigor indicator (its vigor computation).
#Theoretically, values between 1 and -1 are prime conditions for oscillator type strategies.
#Thus, values above an absolute value of 1 should be grounds for trying to trend follow.
#It seems that the most critical parameter in this demo is the period parameter in the indicator.
#From empirical observation, I have found that a period of 60 to 120 days gets the best results.
#At a 200 day period, there aren't enough trades, and the analytics fall out due to lack of trades.

#Only run these lines once to load the data/environment, and the rest of the boilerplate that blotter needs.

require(DSTrading)
require(quantstrat)


initDate="1990-01-01"
from="2003-01-01"
to="2010-12-31"

source("demoData.R") #contains all of the data-related boilerplate.

## To rerun the strategy, re-run everything from this line down.

strategy.st <- portfolio.st <- account.st <- "TVItrendFollowingLong"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

######### indicator

add.indicator(strategy.st, name="TVI", arguments=list(x=quote(Cl(mktdata)), period=100, delta=0.2), label="TVI")

########## signals

add.signal(strategy.st, name="sigThreshold", 
           arguments=list(threshold=1, column="vigor.TVI", relationship="gte", cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(threshold=1.4, column="vigor.TVI", relationship="lt", cross=TRUE),
           label="longExit")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(threshold=1, column="vigor.TVI", relationship="lt", cross=TRUE),
           label="wrongExit")

########## rules

add.rule(strategy.st, name="ruleSignal", arguments=list(sigcol="longEntry", sigval=TRUE, orderqty=100, ordertype="market", orderside="long", replace=FALSE, prefer="Open"), type="enter", path.dep=TRUE)
add.rule(strategy.st, name="ruleSignal", arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", orderside="long", replace=FALSE, prefer="Open"), type="exit", path.dep=TRUE)
add.rule(strategy.st, name="ruleSignal", arguments=list(sigcol="wrongExit", sigval=TRUE, orderqty="all", ordertype="market", orderside="long", replace=FALSE, prefer="Open"), type="exit", path.dep=TRUE)

########## apply strategy

t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st, verbose=FALSE)
t2 <- Sys.time()
print(t2-t1)

########## tradeStats

updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)

(tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=TRUE))
(dStats <- dailyStats(Portfolios = portfolio.st, use="Equity"))

chart.Posn(portfolio.st, "SPY")
