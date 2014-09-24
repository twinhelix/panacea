###############################################################################
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
###############################################################################
###############################################################################
# Calculate Greeks    # r = rd, q = rf
###############################################################################
# 1st Order

# DELTA
calc_call_delta = function (S, K, r, time, imp_vol){
  return(calc_call_delta_fx(S, K, r, 0, time, imp_vol))
}

calc_call_delta_fx = function (S, K, rd, rf, time, imp_vol){
    return(exp(-rf*time) * pnorm(calc_d1_fx(S, K, rd, rf, time, imp_vol)))
}

calc_put_delta = function (S, K, r, time, imp_vol){
  return(calc_put_delta_fx(S, K, r, 0, time, imp_vol))
}

calc_put_delta_fx = function (S, K, rd, rf, time, imp_vol){
  return(-exp(-rf*time) * pnorm(-calc_d1_fx(S, K, rd, rf, time, imp_vol)))
}

# VEGA / TAU
calc_vega = function (S, K, r, time, imp_vol){
  return(calc_vega_fx(S, K, r, 0, time, imp_vol))
}

calc_vega_fx = function (S, K, rd, rf, time, imp_vol){
  return( S * exp(-rf * time) * dnorm(calc_d1_fx(S, K, rd, rf, time, imp_vol)) * sqrt(time))  
}

# THETA
calc_call_theta = function (S, K, r, time, imp_vol){
  return(calc_call_theta_fx(S, K, r, 0, time, imp_vol))
}
calc_call_theta_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  return(-exp(-rf*time)*S*dnorm(d1)*imp_vol/(2*sqrt(time)) - rd*K*exp(-rd*time)*pnorm(d2) + rf*S*exp(-rf*time)*pnorm(d1))
}

calc_put_theta = function (S, K, r, time, imp_vol){
  return(calc_put_theta_fx(S, K, r, 0, time, imp_vol))
}
calc_put_theta_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  return(-exp(-rf*time)*S*dnorm(d1)*imp_vol/(2*sqrt(time)) + rd*K*exp(-rd*time)*pnorm(-d2) - rf*S*exp(-rf*time)*pnorm(-d1))
}

# RHO
calc_call_rho = function (S, K, r, time, imp_vol){
  return(calc_call_rho_fx(S, K, r, 0, time, imp_vol))
}
calc_call_rho_fx = function (S, K, rd, rf, time, imp_vol){
  return( K * time * exp(-rf * time) * pnorm(calc_d2_fx(S, K, rd, rf, time, imp_vol)))  
}

calc_put_rho = function (S, K, r, time, imp_vol){
  return(calc_put_rho_fx(S, K, r, 0, time, imp_vol))
}
calc_put_rho_fx = function (S, K, rd, rf, time, imp_vol){
  return( -K * time * exp(-rf * time) * pnorm(-calc_d2_fx(S, K, rd, rf, time, imp_vol)))  
}
###############################################################################
# 2nd Order
# Gamma - Ddelta^2
calc_gamma = function (S, K, r, time, imp_vol){
  return(calc_gamma_fx(S, K, r, 0, time, imp_vol))
}
calc_gamma_fx = function (S, K, rd, rf, time, imp_vol){
  return(exp(-rf*time)*dnorm(calc_d1_fx(S, K, rd, rf, time, imp_vol))/(S*imp_vol*sqrt(time)))
}

# Vanna - Ddelta Dvol
calc_vanna = function (S, K, r, time, imp_vol){
  return(calc_vanna_fx(S, K, r, 0, time, imp_vol))
}
calc_vanna_fx = function (S, K, rd, rf, time, imp_vol){
  return(-exp(-rf*time)*dnorm(calc_d1_fx(S, K, rd, rf, time, imp_vol))
         *calc_d2_fx(S, K, rd, rf, time, imp_vol)/imp_vol)
}

# Vomma - dTau/dVol
calc_vomma = function (S, K, r, time, imp_vol){
  return(calc_vomma_fx(S, K, r, 0, time, imp_vol))
}
calc_vomma_fx = function (S, K, rd, rf, time, imp_vol){
  vega = calc_vega_fx(S, K, rd, rf, time, imp_vol)
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  
  vomma = vega * d1 * d2 / imp_vol
  return(vomma)
}

# Charm - DdeltaDtime - Delta decay
calc_call_charm = function (S, K, r, time, imp_vol){
  return(calc_call_charm_fx(S, K, r, 0, time, imp_vol))
}
calc_call_charm_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  
  fst = rf * exp(-rf*time)*pnorm(d1) 
  snd = -exp(-rf*time)*dnorm(d1)*(2*(rd-rf)*time - d2*imp_vol*sqrt(time))/(2*time*imp_vol*sqrt(time))
  return(fst+snd)
}

calc_put_charm = function (S, K, r, time, imp_vol){
  return(calc_put_charm_fx(S, K, r, 0, time, imp_vol))
}
calc_put_charm_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  
  fst = -rf * exp(-rf*time)*pnorm(-d1) 
  snd = -exp(-rf*time)*dnorm(d1)*(2*(rd-rf)*time - d2*imp_vol*sqrt(time))/(2*time*imp_vol*sqrt(time))
  return(fst+snd)
}

# Veta - DvegaDtime 
calc_veta = function (S, K, r, time, imp_vol){
  return(calc_veta_fx(S, K, r, 0, time, imp_vol))
}
calc_veta_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  
  fst = S*exp(-rf*time)*dnorm(d1)*sqrt(time)
  snd = rf + (rd-rf)*d1/(imp_vol*sqrt(time)) - (1+d1*d2)/(2*time)
  return(fst*snd)
}

###############################################################################
# 3rd Order
# Colour -DgammaDtime - gamma decay 
calc_colour = function (S, K, r, time, imp_vol){
  return(calc_colour_fx(S, K, r, 0, time, imp_vol))
}
calc_colour_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  
  fst = -exp(-rf*time)*dnorm(d1)/(2*S*time*imp_vol*sqrt(time))
  snd = 2*rf*time + 1 + (2*(rd-rf)*time - d2*imp_vol*sqrt(time))*d1/(imp_vol*sqrt(time))
  return(fst*snd)
}

# Speed - DgammaDspot - rate of change in Gamma wrt underlying
calc_speed = function (S, K, r, time, imp_vol){
  return(calc_speed_fx(S, K, r, 0, time, imp_vol))
}
calc_speed_fx = function (S, K, rd, rf, time, imp_vol){
  d1    = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  gamma = calc_gamma_fx(S, K, rd, rf, time, imp_vol) 
  
  return(-gamma/S*(d1/(imp_vol*sqrt(time)) + 1))
}

# Ultima - DvommaDvol
calc_ultima = function (S, K, r, time, imp_vol){
  return(calc_ultima_fx(S, K, r, 0, time, imp_vol))
}
calc_ultima_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  vega = calc_vega_fx(S, K, rd, rf, time, imp_vol)
    
  return(-vega/imp_vol^2 * (d1*d2*(1-d1*d2) + d1^2 + d2^2))
}

# Zomma - DgammaDvol
calc_zomma = function (S, K, r, time, imp_vol){
  return(calc_zomma_fx(S, K, r, 0, time, imp_vol))
}
calc_zomma_fx = function (S, K, rd, rf, time, imp_vol){
  d1 = calc_d1_fx(S, K, rd, rf, time, imp_vol)
  d2 = calc_d2_fx(S, K, rd, rf, time, imp_vol)
  vega = calc_vega_fx(S, K, rd, rf, time, imp_vol)
  
  return(vega * d1 * d2 / imp_vol)
}

###############################################################################
# Special Greeks
# Dual Delta - Actual prob of an option finishing at a particular price of K
# First derivative of option price with respect to strike


calc_call_dual_delta = function (S, K, r, time, imp_vol){
  return(calc_call_dual_delta_fx(S, K, r, 0, time, imp_vol))
}

calc_call_dual_delta_fx = function (S, K, rd, rf, time, imp_vol){
  return(- exp(-rd*time) * pnorm(calc_d2_fx(S, K, rd, rf, time, imp_vol)))
}

calc_put_dual_delta = function (S, K, r, time, imp_vol){
  return(calc_put_dual_delta_fx(S, K, r, 0, time, imp_vol))
}

calc_put_dual_delta_fx = function (S, K, rd, rf, time, imp_vol){
  return(exp(-rd*time) * pnorm(-calc_d2_fx(S, K, rd, rf, time, imp_vol)))
}

# Dual Gamma - 2nd order of dual - change in actual prob
calc_dual_gamma = function (S, K, r, time, imp_vol){
  return(calc_dual_gamma_fx(S, K, r, 0, time, imp_vol))
}
calc_dual_gamma_fx = function (S, K, rd, rf, time, imp_vol){
  return(exp(-rd*time)*dnorm(calc_d2_fx(S, K, rd, rf, time, imp_vol))/(K*imp_vol*sqrt(time)))
}

