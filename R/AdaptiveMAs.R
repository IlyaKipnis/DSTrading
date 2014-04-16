require(Rcpp)
require(inline)

########### SCRATCHWORK FILE. NOTHING TO SEE HERE. NOTHING IN HERE WILL BE EXPORTED. 
########### EVER.

################ Transforms ####################

#The filter function cannot deal with NAs for some godforsaken reason. Stupid function.


################# Oscillators ###################








#Phase Accumulator
#Rocket Science For Traders Chapter 7
#REPLACE WITH RCPP FUNCTIONALITY WHEN SAID KNOWLEDGE IS OBTAINED
#75K ROWS IN 27.8 SECONDS--UNACCEPTABLE FOR TICK DATA
phaseAccumulator <- function(HLC, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  smooth <- WMA(price, n=4)
  smooth[1:3] <- price[1:3]
  smooth <- as.numeric(smooth)
  detrender <- I1 <- Q1 <- phase <- deltaPhase <- instPeriod <- period <- rep(0, length(price))
  for(i in 7:length(smooth)){
    detrender[i] <- (.0962*smooth[i]+.5769*smooth[i-2]-.5769*smooth[i-4]-.0962*smooth[i-6])*(.075*period[i-1]+.54)
    Q1[i] <- (.0962*detrender[i]+.5769*detrender[i-2]-.5769*detrender[i-4]-.0962*detrender[i-6])*(.075*period[i-1]+.54)
    I1[i] <- detrender[i-3]
    
    I1[i] <- .15*I1[i]+.85*I1[i-1]
    Q1[i] <- .15*Q1[i]+.85*Q1[i-1]
    
    if(abs(I1[i])>0) {
      phase[i] <- atan(abs(Q1[i]/I1[i]))
    }
    if(I1[i]<0 & Q1[i]>0) {phase[i]=pi-phase[i]}
    if(I1[i]<0 & Q1[i]<0) {phase[i]=pi+phase[i]}
    if(I1[i]>0 & Q1[i]<0) {phase[i]=2*pi-phase[i]}
    
    deltaPhase[i] <- phase[i-1]-phase[i]
    if (phase[i-1] < pi/2 & phase[i] > 3*pi/2) {
      deltaPhase[i] <- 2*pi+phase[i-1]-phase[i]
    }
    if (deltaPhase[i] < 7*pi/180) {deltaPhase[i] <- 7*pi/180}
    if (deltaPhase[i] > pi/3)     {deltaPhase[i] <- pi/3}
    
    phaseSum=0
    limit <- min((i-1), 40)
    for(j in 0:limit) {
      phaseSum <- phaseSum+deltaPhase[i-j]
      if(phaseSum > 2*pi & instPeriod[i]==0){
        instPeriod[i] <- j
      }
    }
    if(instPeriod[i]==0) {instPeriod[i] <- instPeriod[i-1]}
    period[i] <- .25*instPeriod[i] + .75*period[i-1]
  }
  period <- xts(period, order.by=index(HLC))
  return(period)
}

#Homodyne Discriminator
#Rocket Science For Traders Chapter 7
#REPLACE WITH RCPP FUNCTIONALITY WHEN SAID KNOWLEDGE IS OBTAINED
#13 seconds on 75k rows
homodyne <- function(HLC, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  smooth <- WMA(price, n=4)
  smooth[1:3] <- price[1:3]
  smooth <- as.numeric(smooth)
  detrender <- I1 <- Q1 <- jI <- jQ <- I2 <- Q2 <- Re <- Im <- period <- smoothPeriod <- rep(0, length(price))
  for(i in 7:length(smooth)){
    detrender[i] <- (.0962*smooth[i]+.5769*smooth[i-2]-.5769*smooth[i-4]-.0962*smooth[i-6])*(.075*period[i-1]+.54)
    
    #compute InPhase and Quadrature components
    Q1[i] <- (.0962*detrender[i]+.5769*detrender[i-2]-.5769*detrender[i-4]-.0962*detrender[i-6])*(.075*period[i-1]+.54)
    I1[i] <- detrender[i-3]
    
    #Advance the phase of I1 and Q1 by 90 degrees
    jI[i] <- (.0962*I1[i]+.5769*I1[i-2]-.5769*I1[i-4]-.0962*I1[i-6])*(.075*period[i-1]+.54)
    jQ[i] <- (.0962*Q1[i]+.5769*Q1[i-2]-.5769*Q1[i-4]-.0962*Q1[i-6])*(.075*period[i-1]+.54)
    
    #Phasor addition for 3 bar averaging
    I2[i] <- I1[i] - jQ[i]
    Q2[i] <- Q1[i] + jI[i]
    
    #Smooth components before applying discriminator
    I2[i] <- .2*I2[i]+.8*I2[i-1]
    Q2[i] <- .2*Q2[i]+.8*Q2[i-1]
    
    #Homodyne Discriminator
    Re[i] <- I2[i]*I2[i-1]+Q2[i]*Q2[i-1]
    Im[i] <- I2[i]*Q2[i-1]-Q2[i]*I2[i-1]
    
    Re[i] <- .2*Re[i]+.8*Re[i-1]
    Im[i] <- .2*Im[i]+.8*Im[i-1]
    
    if (Im[i] != 0 & Re[i] != 0) {
      period[i] <- 2*pi/atan(Im[i]/Re[i])
    }
    if (period[i] > 1.5*period[i-1]) {period[i] <- period[i]*1.5}
    if (period[i] < period[i-1]*2/3) {period[i] <- period[i-1]*2/3}
    if (period[i] < 6) {period[i] <- 6}
    if (period[i] > 50) {period[i] <- 50}
    period[i] <- .2*period[i]+.8*period[i-1]
    smoothPeriod[i] <- period[i]/3 + smoothPeriod[i-1]*2/3
  }
  smoothPeriod <- xts(smoothPeriod, order.by=index(price))
  return (smoothPeriod)
}

#Enhanced Signal to Noise Ratio
#From Rocket Science For Traders Chapter 8
#Non-instantaneous, needs RCpp at some point.
ESNR <- function(HLC, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  smooth <- WMA(price, n=4)
  smooth[1:3] <- price[1:3]
  smooth <- as.numeric(smooth)
  detrender <- I1 <- Q1 <- jI <- jQ <- I2 <- Q2 <- Re <- Im <- period <- smoothPeriod <- 
    I3 <- Q3 <- Signal <- Noise <- SNR <- rep(0, length(price))
  for(i in 7:length(smooth)){
    detrender[i] <- (.0962*smooth[i]+.5769*smooth[i-2]-.5769*smooth[i-4]-.0962*smooth[i-6])*(.075*period[i-1]+.54)
    
    #compute InPhase and Quadrature components
    Q1[i] <- (.0962*detrender[i]+.5769*detrender[i-2]-.5769*detrender[i-4]-.0962*detrender[i-6])*(.075*period[i-1]+.54)
    I1[i] <- detrender[i-3]
    
    #Advance the phase of I1 and Q1 by 90 degrees
    jI[i] <- (.0962*I1[i]+.5769*I1[i-2]-.5769*I1[i-4]-.0962*I1[i-6])*(.075*period[i-1]+.54)
    jQ[i] <- (.0962*Q1[i]+.5769*Q1[i-2]-.5769*Q1[i-4]-.0962*Q1[i-6])*(.075*period[i-1]+.54)
    
    #Phasor addition for 3 bar averaging
    I2[i] <- I1[i] - jQ[i]
    Q2[i] <- Q1[i] + jI[i]
    
    #Smooth components before applying discriminator
    I2[i] <- .2*I2[i]+.8*I2[i-1]
    Q2[i] <- .2*Q2[i]+.8*Q2[i-1]
    
    #Homodyne Discriminator
    Re[i] <- I2[i]*I2[i-1]+Q2[i]*Q2[i-1]
    Im[i] <- I2[i]*Q2[i-1]-Q2[i]*I2[i-1]
    
    Re[i] <- .2*Re[i]+.8*Re[i-1]
    Im[i] <- .2*Im[i]+.8*Im[i-1]
    
    if (Im[i] != 0 & Re[i] != 0) {
      period[i] <- 2*pi/atan(Im[i]/Re[i])
    }
    if (period[i] > 1.5*period[i-1]) {period[i] <- period[i]*1.5}
    if (period[i] < period[i-1]*2/3) {period[i] <- period[i-1]*2/3}
    if (period[i] < 6) {period[i] <- 6}
    if (period[i] > 50) {period[i] <- 50}
    period[i] <- .2*period[i]+.8*period[i-1]
    smoothPeriod[i] <- period[i]/3 + smoothPeriod[i-1]*2/3
    
    Q3[i] <- .5*(smooth[i]-smooth[i-2])*(.1759*smoothPeriod[i]+.4607)
    limit <- max(0,min((i-1), floor(smoothPeriod[i]/2-1)))
    for(j in 0:limit) {
      I3[i] <- I3[i]+Q3[i-j]
    }
    I3[i] <- 1.57*I3[i]/floor(smoothPeriod[i]/2)
    if(I3[i] > 10000000 | I3[i] < -100000) { I3[i] <- 0}
  }
  Signal <- I3*I3+Q3*Q3
  Noise <- stats::filter(.1*(Hi(HLC)-Lo(HLC))^2*.25, .9, method="recursive")
  SNR[Signal != 0 & Noise !=0] <- stats::filter(.33*10*log(Signal[Signal > 0]/Noise[Signal > 0])/log(10),.67,method="recursive")
  SNR <- xts(SNR, order.by=index(HLC))
  return(SNR)
}


