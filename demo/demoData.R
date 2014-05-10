rm(list=ls(.blotter), envir=.blotter)
initDate='1990-12-31'

currency('USD')
Sys.setenv(TZ="UTC")

symbols <- c("SPY", #SPDR S&P 500
             "MDY", #SPDR S&P midcap 400 
             "DIA", #SPDR Dow Jones Industrial Avg
             "XLB", #SPDR Materials sector
             "XLE", #SPDR Energy sector
             "XLF", #SPDR Financial sector
             "XLP", #SPDR Consumer staples sector
             "XLI", #SPDR Industrial sector
             "XLU", #SPDR Utilities sector
             "XLV", #SPDR Healthcare sector
             "XLK", #SPDR Tech sector
             "XLY", #SPDR Consumer discretionary sector
             "SPYV",#SPDR S&P 500 value
             "SLYV",#SPDR S&P 600 smallcap value
             "SPYG",#SPDR S&P 500 growth
             "DGT", #SPDR Dow Global
             "SLYG",#SPDR S&P 600 smallcap growth
             "THRK",#SPDR Russell 3k
             "RWR", #SPDR Dow Jones REIT ETF
             
             "EWJ", #iShares Japan
             "EWG", #iShares Germany
             "EWU", #iShares UK
             "EWC", #iShares Canada
             "EWY", #iShares South Korea
             "EWA", #iShares Australia
             "EWH", #iShares Hong Kong
             "EWS", #iShares Singapore
             "IWB", #iShares Russell 1k
             "IWM", #iShares Russell 2k
             "IYZ", #iShares U.S. Telecom
             "EZU", #iShares MSCI EMU ETF
             "IWN", #iShares Russell 2k value
             "IWO", #iShares Russell 2k Growth
             "IYR", #iShares U.S. Real Estate
             "IWR", #iShares Russell midcap
             "IWS", #iShares Rusell midcap value
             "IWP", #iShares Rusell midcap growth
             "EWT", #iShares Taiwan
             "EWZ", #iShares Brazil
             "EFA", #iShares EAFE
             "IBB", #iShares Nasdaq Biotech
             "IGE", #iShares North American Natural Resources
             "EPP", #iShares Pacific Ex Japan
             "LQD", #iShares Investment Grade Corporate Bonds
             "SHY", #iShares 1-3 year TBonds
             "IEF", #iShares 3-7 year TBonds
             "TLT" #iShares 20+ year Bonds
)

#SPDR ETFs first, iShares ETFs afterwards
if(!"SPY" %in% ls()) { 
  getSymbols(symbols, from="2003-01-01", to="2010-12-31", src="yahoo", adjust=TRUE)  
}

stock(symbols, currency="USD", multiplier=1)