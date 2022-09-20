#####################################################################################################
################  Progetto Advanced Times Series - Martina Chiesa and Matteo Zucca	 ################ 
#####################################################################################################

##### This script includes two user-defined functions that will be used through the script

## Functioning:
#   1. Use adjusted price instead of original to take into account splits and dividends.
#   2. Adjust also other prices by same coefficients of closing price.
#   3. Perform overnights adjustment: aim is to prepare quantities to compute
#      the unbiased analytic scale invariant estimator (sigma_6 in the presentation).
#      It requires also sigma_4, the previous estimator (stored in variable x below)

garmanklass.measure =
function(data, volatility = TRUE){
  nobs  = NROW(data)

  #### Intradaily
  ## Extract
  coef = data$Adjusted / data$Close
  H1 = log( data$High * coef )
  L1 = log( data$Low * coef )
  O1 = log( data$Open * coef )
  C1 = log( data$Close * coef )
  u1 = H1 - O1
  d1 = L1 - O1
  c1 = C1 - O1
  ## Values
  x = 0.511 * (u1 - d1)^2 +
    (-0.019) * (c1 * (u1 + d1) - 2 * u1 * d1) +
    (-0.383) * c1^2 # sigma_4
  
  #### Overnight adjustment
  ret.close.open = c(NA, log( data$Open[-1] / data$Close[-nobs] ) )
  ret.open.close = log( data$Close / data$Open )
  x1 = sum( ret.close.open^2, na.rm = TRUE)
  x2 = sum( ret.open.close^2, na.rm = TRUE )  
  f  = x1 / (x1 + x2)
  f[f < 0.01] = 0.01; f[f > 0.99] = 0.99
  a = 0.12
  x = a * ret.close.open^2 / f + ( (1 - a) / (1 - f) ) * x # sigma_6
  
  #### Answer
  if ( volatility == TRUE ) 
    return(sqrt(x))
  else
    return(x)
}

# ------------------------------------------------------------------------------

error.measures = 
function(y, fit, naive)
{ 
  #### Errors
  u = y - fit

  #### Error measures
  ME   = mean( u )
  MAE  = mean( abs(u) )
  RMSE = sqrt( mean( u^2 ) )
  #### Percentage error measures
  if ( all(y > 0) )
  {
    ur  = u / y
    MPE    = mean( ur )
    MAPE   = mean( abs( ur ) )
    RMSPE  = sqrt( mean( ur^2 ) )
  }
  else
  {
    MPE    = NULL
    MAPE   = NULL
    RMSPE  = NULL
  }

  #### Scaled error measures
  u1 = y - naive
  ScMAE  = MAE / mean( abs(u1) )
  ScRMSE = RMSE / sqrt( mean( u1^2 ) )
  
  ####
  c(ME = ME, MAE = MAE, RMSE = RMSE, 
    MPE = MPE, MAPE = MAPE, RMSPE = RMSPE, 
    ScMAE = ScMAE, ScRMSE = ScRMSE)
}

# ------------------------------------------------------------------------------



##################### End of Auxiliary Functions ###############################
