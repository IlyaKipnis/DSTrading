require(Quandl)
Quandl.auth("tMqBC2HG1qczGTFxsHyN")
require(IKTrading)


currency('USD')
Sys.setenv(TZ="UTC")


t1 <- Sys.time()
if(!"CME_CL" %in% ls()) {
  #Energies
  CME_CL <- quandClean("CHRIS/CME_CL", start_date=from, end_date=to, verbose=verbose) #Crude
  CME_NG <- quandClean("CHRIS/CME_NG", start_date=from, end_date=to, verbose=verbose) #NatGas
  CME_HO <- quandClean("CHRIS/CME_HO", start_date=from, end_date=to, verbose=verbose) #HeatingOil
  CME_RB <- quandClean("CHRIS/CME_RB", start_date=from, end_date=to, verbose=verbose) #Gasoline
  ICE_B <- quandClean("CHRIS/ICE_B", start_date=from, end_date=to, verbose=verbose) #Brent
  ICE_G <- quandClean("CHRIS/ICE_G", start_date=from, end_date=to, verbose=verbose) #Gasoil
  
  #Grains
  CME_C <- quandClean("CHRIS/CME_C", start_date=from, end_date=to, verbose=verbose) #Chicago Corn
  CME_S <- quandClean("CHRIS/CME_S", start_date=from, end_date=to, verbose=verbose) #Chicago Soybeans
  CME_W <- quandClean("CHRIS/CME_W", start_date=from, end_date=to, verbose=verbose) #Chicago Wheat
  CME_SM <- quandClean("CHRIS/CME_SM", start_date=from, end_date=to, verbose=verbose) #Chicago Soybean Meal
  CME_KW <- quandClean("CHRIS/CME_KW", start_date=from, end_date=to, verbose=verbose) #Kansas City Wheat
  CME_BO <- quandClean("CHRIS/CME_BO", start_date=from, end_date=to, verbose=verbose) #Chicago Soybean Oil
  
  #Softs
  #ICE_SB <- quandClean("CHRIS/ICE_SB", start_date=from, end_date=to, verbose=verbose) #Sugar
  #Sugar 2007-03-26 is wrong
  ICE_KC <- quandClean("CHRIS/ICE_KC", start_date=from, end_date=to, verbose=verbose) #Coffee
  #Coffee January of 08 is FUBAR'd
  ICE_CC <- quandClean("CHRIS/ICE_CC", start_date=from, end_date=to, verbose=verbose) #Cocoa
  ICE_CT <- quandClean("CHRIS/ICE_CT", start_date=from, end_date=to, verbose=verbose) #Cotton
  
  #Other Ags
  CME_LC <- quandClean("CHRIS/CME_LC", start_date=from, end_date=to, verbose=verbose) #Live Cattle
  CME_LN <- quandClean("CHRIS/CME_LN", start_date=from, end_date=to, verbose=verbose) #Lean Hogs
  
  #Precious Metals
  CME_GC <- quandClean("CHRIS/CME_GC", start_date=from, end_date=to, verbose=verbose) #Gold
  CME_SI <- quandClean("CHRIS/CME_SI", start_date=from, end_date=to, verbose=verbose) #Silver
  CME_PL <- quandClean("CHRIS/CME_PL", start_date=from, end_date=to, verbose=verbose) #Platinum
  CME_PA <- quandClean("CHRIS/CME_PA", start_date=from, end_date=to, verbose=verbose) #Palladium
  
  #Base
  CME_HG <- quandClean("CHRIS/CME_HG", start_date=from, end_date=to, verbose=verbose) #Copper
  
  #Currencies
  CME_AD <- quandClean("CHRIS/CME_AD", start_date=from, end_date=to, verbose=verbose) #Ozzie
  CME_CD <- quandClean("CHRIS/CME_CD", start_date=from, end_date=to, verbose=verbose) #Loonie
  CME_SF <- quandClean("CHRIS/CME_SF", start_date=from, end_date=to, verbose=verbose) #Franc
  CME_EC <- quandClean("CHRIS/CME_EC", start_date=from, end_date=to, verbose=verbose) #Euro
  CME_BP <- quandClean("CHRIS/CME_BP", start_date=from, end_date=to, verbose=verbose) #Cable
  CME_JY <- quandClean("CHRIS/CME_JY", start_date=from, end_date=to, verbose=verbose) #Yen
  CME_NE <- quandClean("CHRIS/CME_NE", start_date=from, end_date=to, verbose=verbose) #Kiwi
  
  #Equities
  CME_ES <- quandClean("CHRIS/CME_ES", start_date=from, end_date=to, verbose=verbose) #Emini
  CME_MD <- quandClean("CHRIS/CME_MD", start_date=from, end_date=to, verbose=verbose) #Midcap 400
  CME_NQ <- quandClean("CHRIS/CME_NQ", start_date=from, end_date=to, verbose=verbose) #Nasdaq 100
  CME_TF <- quandClean("CHRIS/CME_TF", start_date=from, end_date=to, verbose=verbose) #Russell Smallcap
  CME_NK <- quandClean("CHRIS/CME_NK", start_date=from, end_date=to, verbose=verbose) #Nikkei
  
  #Dollar Index and Bonds/Rates
  ICE_DX  <- quandClean("CHRIS/CME_DX", start_date=from, end_date=to, verbose=verbose) #Dixie
  #CME_FF  <- quandClean("CHRIS/CME_FF", start_date=from, end_date=to, verbose=verbose) #30-day fed funds
  CME_ED  <- quandClean("CHRIS/CME_ED", start_date=from, end_date=to, verbose=verbose) #3 Mo. Eurodollar/TED Spread
  CME_FV  <- quandClean("CHRIS/CME_FV", start_date=from, end_date=to, verbose=verbose) #Five Year TNote
  CME_TY  <- quandClean("CHRIS/CME_TY", start_date=from, end_date=to, verbose=verbose) #Ten Year Note
  CME_US  <- quandClean("CHRIS/CME_US", start_date=from, end_date=to, verbose=verbose) #30 year bond
}

CMEinsts <- c("CL", "NG", "HO", "RB", "C", "S", "W", "SM", "KW", "BO", "LC", "LN", "GC", "SI", "PL", 
              "PA", "HG", "AD", "CD", "SF", "EC", "BP", "JY", "NE", "ES", "MD", "NQ", "TF", "NK", #"FF",
              "ED", "FV", "TY", "US")

ICEinsts <- c("B", "G", #"SB", "KC", 
              "CC", "CT", "DX")
CME <- paste("CME", CMEinsts, sep="_")
ICE <- paste("ICE", ICEinsts, sep="_")
symbols <- c(CME, ICE)
stock(symbols, currency="USD", multiplier=1)
t2 <- Sys.time()
print(t2-t1)