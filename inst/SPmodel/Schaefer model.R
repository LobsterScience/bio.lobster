#model fitter

  biomass.schaefer.recursion = function( par, O, C, q.est=T, errorType="lognormal" ) { 
  	#set q to false if you want to fix it if using assessment outputs or other q adjs
  	
    eps = 1e-4 #keeps results positive
    
    r=max( eps, par["r"] ) # intrinsic rate of increase of biomass
    K=max( eps, par["K"] ) # carrying capacity
    q=max( eps, par["q"] )# catchability  -- force in interval (0,1) 
    B0=par["B0"]  # starting model abundance
    
    nx = length(O) # no. data points in Time Series
    B = rep( 0, nx ) # model biomass
    B[1] =  B0  + r*B0 * ( 1 - B0/K ) - C[1]  # initial conditions 
    for ( t in 2:nx)  {
      B[t] = max( eps, B[t-1]  + r*B[t-1] * ( 1 - B[t-1]/K) - C[t-1] )
    }
    if(q.est) {Op = q * B }  # modelled index of abundance
    else {Op = B ; q=1}

    if (errorType == "normal" ) error = sum ( (O - Op)^2, na.rm=T )
    if (errorType == "lognormal" ) error = sum ( (log(O) - log(Op))^2, na.rm=T )
    

    return( error )
  }
 
   # ----------------

  #from model fits
  
  biomass.schaefer = function(r, K, q, B0, O, C,hes,q.est=T, yrs=NULL, errorType="lognormal" ) {
    
    # estimate full solution set at a fixed set of paramaeters and O, biomass index, and C, catch

    if (is.null(yrs)) yrs = 1:length(C)

    eps = 1e-4

    nx  = length(O) # no. data points in Time Series
    np = 0
    nt = np + nx

    # output data
    res = data.frame( yrs=0:(nt-1) )
    res$yrs = res$yrs + yrs[1]
    B = rep( 0, nx ) # model biomass
    B[1] =  B0 * (1 + r * ( 1 - B0/K )) - C[1]  # initial conditions 
    for ( t in 2:nx)  {
      B[t] = max( eps, B[t-1] * ( 1 + r * ( 1 - B[t-1]/K )) - C[t-1] )
    }
    
    if(q.est) {Op = q*B } # modelled index of abundance
    else {Op = B; q=1}
   
    if (errorType == "normal" ) error = sum ( (O - Op)^2, na.rm=T )
    if (errorType == "lognormal" ) error = sum ( (log(O) - log(Op))^2, na.rm=T )


    MSY <- r * K / 4  # maximum height of of the latent productivity (yield)
    BMSY <- K/2  # biomass at MSY
    FMSY <- 2 * MSY / K # fishing mortality at MSY

    res$B =  B  
    res$C = C   
    res$Op = q*res$B  # modelled index of abundance
    res$O = NA
    res$O[1:nx] = O  
    
    # estimates of surplus production
    res$P = NA
    res$P[1] <- res$B[2]- res$B[1] + res$C[1]
    for (i in 2:(nx-1) ){
      res$P[i] <- (res$B[i+1]- res$B[i-1])/2 + res$C[i]  # linear interpolation
    }
    res$P[nx] <- res$B[nx]- res$B[nx-1] + res$C[nx]
    se <-diag(1/hes)
    params = data.frame(cbind(rbind( r=r, K=K, q=q,B0=B0),cbind(se)))
    extras = data.frame(error=error, errorType=errorType, msy=MSY, fmsy=FMSY, bmsy=BMSY, row.names="Schaefer" )
    
    out = list( res=res, params=params,extras=extras )
    return(out)

  }

  # ----------------

#get data in from clipboard, removes prior data where catch is greater than biomass, scales biomass units downward
	data.in<-function(){
		aaa<-read.table('clipboard',header=T)
		aaa$dd<- aaa$C/aaa$B
		if(any(aaa$dd>0.95)) {
			a3<-which(aaa$dd>0.95)
			a3<-max(a3)
			aaa<-aaa[-1:-a3,]		
		}
		a5<-round(log10(max(aaa$B)))-3
		dat<-list(B=aaa$B/10^a5,C=aaa$C/10^a5)
		return(dat)
		}