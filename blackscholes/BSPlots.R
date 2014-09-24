rm(list=ls(all=TRUE)); cat("\014")
source(file="/Users/HassassiN/Dropbox/Panacea/blackscholes/BlackScholesGreeks.R")
S_MID = 100
K     = S_MID
r     = 0.01
T     = 0.25
vol   = 0.20

Spots = seq(from = S_MID - 25, to = S_MID + 25, by = 0.05)

# Plots
plotGraph = function(X, Y, TYPE){
  plot(X,Y,type='l',col='green', xlab="Spot", ylab="Greek Value", main = TYPE)
  abline(h=0, col='red')
  abline(v=100, col='gray80')
}


Deltas = calc_call_delta(Spots, K, r, 0.0000001, vol)
plotGraph(Spots, Deltas, "DELTA")

Gammas = calc_gamma(Spots, K, r, 0.25, 0.2)
plotGraph(Spots, Gammas, "GAMMA")


Vegas = calc_vega(Spots, K, r, 0.01, vol)
plotGraph(Spots, Vegas, "VEGA")

Vannas = calc_vanna(Spots, K, r, 0.1, vol)
plotGraph(Spots, Vannas, "VANNA")



