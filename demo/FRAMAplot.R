from="2003-01-01"
to="2010-12-31"
source("demoData.R")

tmp <- FRAMA(HLC(XLB))
tmp2 <- FRAMA(HLC(XLB), FC=4, n=126, SC=300)
tmp3 <- FRAMA(HLC(XLB), FC=40, n=252, SC=252)
ema <- EMA(x=Cl(XLB),n=126)

myTheme<-chart_theme()
myTheme$col$dn.col<-'gray2'
myTheme$col$dn.border <-'gray2'
myTheme$col$up.border <-'gray2'

chart_Series(XLB,
             TA="add_TA(tmp$FRAMA, col='blue', on=1, lwd=3);
              add_TA(tmp2$FRAMA, col='green', on=1, lwd=3);
              add_TA(tmp3$FRAMA, col='red', on=1, lwd=3);
              add_TA(ema$EMA, col='orange', on=1, lwd=3)", 
             theme=myTheme, subset="2007::2008")

legend(x=5, y=30, legend=c("FRAMA FC=1, n=20, SC=200", 
                           "FRAMA FC=4, n=126, SC=300", 
                           "FRAMA FC=40, n=252, SC=252",
                           "EMA n=126"),
       fill=c("blue", "green", "red", "orange"), bty="n")