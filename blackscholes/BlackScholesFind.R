###############################################################################
source()
# Calculate d1 and d2 - for stocks & fx options
calc_d1 = function (S, K, r, time, imp_vol){
  return(calc_d1_fx(S,K,r,0,time,imp_vol))
}
calc_d1_fx = function (S, K, rd, rf, time, imp_vol){
  return((log(S/K) + (rd - rf + (imp_vol^2)/2)*time)/(imp_vol*sqrt(time)))
}
calc_d2 = function (S, K, r, time, imp_vol){
  return(calc_d2_fx(S,K,r,0,time,imp_vol))
}
calc_d2_fx = function (S, K, rd, rf, time, imp_vol){
  return(calc_d1_fx(S,K,rd,rf,time,imp_vol) - (imp_vol*sqrt(time)))
}


calc_impliedVol_call = function (Px, S, K, r, time){
  
}
