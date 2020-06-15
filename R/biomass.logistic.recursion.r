

  biomass.logistic.recursion = function( par, O, C, errorType="lognormal" ) {
      eps = 1e-4 
    
    r=max( eps, par["r"] ) # intrinsic rate of increase of biomass
    K=max( eps, par["K"] ) # carrying capacity
    q=max( eps, par["q"] )# catchability  -- force in interval (0,1) 
    B0=par["B0"] / K # starting model abundance
    
    nx = length(O) # no. data points in Time Series
    B = rep( 0, nx ) # model biomass
    B[1] =  B0 * (1 + r * ( 1 - B0 )) - C[1]/K  # initial conditions 
    for ( t in 2:nx)  {
      B[t] = max( eps, B[t-1] * ( 1 + r * ( 1 - B[t-1] )) - C[t-1]/K )
    }
    
    O = O/K
    Op = q * B  # modelled index of abundance
  
    # normal and lognormal errors are coded .. for others, the underlying function needs a minor modification:

    if (errorType == "normal" ) error = sum ( (O - Op)^2, na.rm=T )
    if (errorType == "lognormal" ) error = sum ( (log(O) - log(max(Op,eps)))^2, na.rm=T )

    return( error )
  }



