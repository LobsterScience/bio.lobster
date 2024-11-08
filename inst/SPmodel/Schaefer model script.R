source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/Schaefer model.R')

		source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/logistic data simulator.R')
		dats = sim.logistic.data(seed=1001,r=0.3,K=100,n0=50,t0=1,tot.t=65,sd.proc=3,sd.obs=1,catchs=T,expl=0.25,expl.range=0.05)
		dats = list(B=dats[,2],C=dats[,3])

#dats = data.in()
params = c(r=0.3, K=100, q=1 , B0=70)

  # lognormal timeseries method
  res0 = optim( params, fn=biomass.schaefer.recursion, O=dats$B, C=dats$C, 
      errorType="lognormal" ,hessian=T) 

# Take parameter estimates from timeseries method using “optim” optimizer
# to obtain accessory estimates using solution parameters obtained above

  res = res0$par
  hes = a2$hessian
  o = biomass.schaefer( r=res["r"], K=res["K"], q=res["q"], B0=res["B0"],hes=hes,
    O=dats$B, C=dats$C, yrs=1:length(aaa$B) ) 
  
  # a few useful plots
  par(mfrow=c(2,2))
  ymax = max( c( o$res$O, o$res$Op ), na.rm=T )
  plot( Op ~ yrs, o$res, type="b", ylim=c(0, ymax), pch=20 )  # predicted catch rate
  points( O ~ yrs, o$res, pch=22, col="red" )  # observed catch rate
  lines( O ~ yrs, o$res, lty="dashed", col="red" )  # observed catch rate
  
  # catch
  plot( C ~ yrs, o$res, type="b" )

  # biomass
  plot( B ~ yrs, o$res, type="b" ) 

  # surplus production by yr
  #plot( P ~ yrs, o$res, type="b", pch=20 )
  #abline ( v=dats$yrs[ length(dats$yrs)]+0.5, lty="dotted" )

  # surplus production quadratic
  plot( P ~ B, o$res, type="p", pch=20 )
  tmp = na.omit( o$res[, c("P","B")] )
  tmp = tmp[ order( tmp$B ), ]
  tmp$B2 <- tmp$B^2
  lines( fitted(lm(P ~ B+B2+0, data=tmp) )~ tmp$B )

