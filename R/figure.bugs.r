
  figure.bugs = function( vname="", type="density", sb=NULL, y=NULL, fn=NULL, labs=c("N-ENS","S-ENS","4X") ,save.plot=T) {
  sb$IOA = sb$I
    yrs0 = sb$yr
    yrs = sb$yr
    yrs.last = max(yrs0) + 0.5
    ndata = length(yrs0)
    hdat = 1:ndata


    x11()
   
    prr = NULL
    prr$class ="none"  # no priors by default


    if ( type=="density" ) {  # default
    
      if ( vname=="K" ) {
        qs = apply( y$K[,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$K[,,])
           prr=NULL
           prr$class="lognormal"

          # E(X) = exp(mu + 1/2 sigma^2)
          # med(X) = exp(mu)
          # Var(X) = exp(2*mu + sigma^2)*(exp(sigma^2) - 1)
          # CV = sqrt( Var(X) ) / E(X) = sqrt(exp(sigma^2) - 1) ~ sigma; sigma < 1/2
          # SD(X) = sqrt( exp(2*mu + sigma^2)*(exp(sigma^2) - 1) )
          #  or   = CV * E(X) 

           prr$meanlog= sb$K.a
           prr$sdlog = sqrt(sb$K.b)
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
         
     }

      if ( vname=="r" ) {
        qs = apply( y$r[,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
        prr=NULL
          prr=NULL
          prr$class='normal'
          prr$mean=sb$r.a
          prr$sd=sqrt(sb$r.b)
          pdat = as.vector(y$r[,,])
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }



      if ( vname=="q" ) {
        qs = apply( y$q[,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
        pdat = as.vector(y$q[,,])
        prr=NULL
        prr$class='uniform'
        prr$max=1
        prr$min=0.001
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }

      
      if ( vname=="BMSY" ) {
        qs = apply( y$K[,,]/2, 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$K[,,]/2)
          prr=NULL
          prr$class="none"
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
       }
  
      if ( vname=="FMSY" ) {
        qs = apply( y$r[,,]/2, 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$r[,,]/2)
          prr=NULL
          prr$class="none"
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
        }

	if ( vname=="sd.o" ) {
        qs = apply( y$sd.o[,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
       
          pdat = as.vector(y$sd.o[,,])
          prr=NULL
          prr$class="lognormal"
          prr$meanlog= 1 
          prr$sdlog= 1
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }
          if ( vname=="sd.p" ) {
        qs = apply( y$sd.p[,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$sd.p[,,])
          prr=NULL
          prr$class="lognormal"
          prr$meanlog=1
          prr$sdlog=1
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
         }

     
    }

    # --------------

    if ( type=="timeseries" ) {
      yrs0 = yrs=1982:2015
      if (vname=="biomass") { 

        SI =  apply( y$q, 1, mean, na.rm=T  )
          qIOA = sb$I / SI
          IOA = sb$I 
          meanval = apply( y$B[,,], 1, mean, na.rm=T  )

          prs = seq( from=0.25, to=0.75, length.out=600)
          Bq =  apply( y$B[,,], 1, quantile, probs=prs, na.rm=T  )

          yran = range(c(0, Bq, sb$I ), na.rm=T )*1.01
          plot(yrs, Bq[1,], type="n", ylim=yran, xlab="", ylab=""  )
          cols = gray.colors( floor(length( prs)/2) )
          cols2 = c(cols[length(cols):1], cols )
          for ( j in 1:length(prs) ) {
            lines ( yrs, Bq[j,], lwd=4, col=cols2[j] )
          }
          # lines( yrs, B, lwd=3, col="darkgreen" )
          title( ylab="Fishable biomass (kt)" ) 
          title( xlab="Year" ) 
          #points( yrs0, qIOA, pch=20, col="darkgreen" )
          #lines ( yrs0, qIOA, lwd=3, col="darkgreen", lty="dashed" )
          #lines ( yrs, meanval, lwd=2, col="blue", lty="dotted" )
          #points( yrs0, IOA, pch=20, col="darkred" )
          #lines( yrs0, IOA, lwd=3, lty="dashed", col="red" )
      }}

      if (vname=="fishingmortality") { 
        Fmsy = apply( y$r/2, 1, mean, na.rm=T ) 
        
          prs = seq( from=0.25, to=0.75, length.out=600)
          Fi = apply( y$F[1:sb$N,,], 1, quantile, probs=prs, na.rm=T )
          yran = range(c(0, max(c(Fi,Fmsy))), na.rm=T )*1.01
          yran = pmin( yran, 1.2 )
          plot( yrs0, Fi[1,], type="n", ylim=yran, xlab="", ylab="" )
          cols = gray.colors( floor(length( prs)/2) )
          cols2 = c(cols[length(cols):1], cols )
          for ( j in 1:length(prs) ) {
            lines ( yrs0, Fi[j,], lwd=4, col=cols2[j] )
          }
          title( ylab="Fishing mortality" ) 
          title( xlab="Year" ) 
        #  abline (h=-log(1-0.2), lwd=2, lty="dashed" )
          abline (h=mean(y$r/2), lwd=2, lty="solid", col="red" )
    
    }

    if (type=="hcr") {
      if (vname=="default") {
          B =  apply( y$B, c(1), mean, na.rm=T  )
          F =  apply( y$F, c(1), mean, na.rm=T  )
          K =  apply( y$K, c(1), mean, na.rm=T  )
          FMSY = apply( y$r/2, c(1), mean, na.rm=T  )
          BMSY = apply( y$K/2, c(1), mean, na.rm=T  )

          ylims = c(0, min( 1, max( FMSY * 1.25, F[hdat] ) ) )
          plot( B[hdat], F[hdat],  type="b", xlim=c(0, K * 1.1 ),  
            ylim=ylims, col="darkorange", cex=0.8, lwd=2, xlab="", ylab="", pch=20 )
 

          # nn = as.matrix( cbind( Bx=as.vector( y$B[ndata,i,,] ), Fx = as.vector( y$F[ndata,i,,] ) ))
          # ellipse.2d(nn[,1], nn[,2], pv=0.05, sc=30)
          
           title( xlab="Fishable biomass (kt)" ) 
          title( ylab="Fishing mortality" ) 

          F30 = -log(1-0.3)
          F10 = -log(1-0.1)

          Fref =  0.22
          Bmsy = K * 0.5
          Bref = K * 0.5*0.8 
          BK = K 
          BK25 = K * .25 
          Fhistorical = mean( F[hdat], na.rm=T )
          Bhistorical = mean( B[hdat], na.rm=T ) 
          yl = 0.05
         
            polygon(x=c(Bmsy,Bmsy*2,Bmsy*2, Bmsy),y=c(-0.1,-0.1,FMSY,FMSY),col='lightgreen',border=NA)
          polygon(x=c(Bmsy/2,Bmsy,Bmsy, Bmsy/2),y=c(-0.1,-0.1,FMSY,FMSY),col='lightgoldenrod',border=NA)
          polygon(x=c(0,Bmsy/2,Bmsy/2, 0),y=c(-0.1,-0.1,FMSY,FMSY),col='darksalmon',border=NA)

        lines( B[hdat], F[hdat],  type="b", xlim=c(0, K * 1.1 ),  
            ylim=ylims, col="darkblue", cex=0.8, lwd=2, xlab="", ylab="", pch=20 )
        


          abline (h=Fref, lty="solid", col="gray", lwd=2 )
    
          abline (h=F10, lty="dotted", col="gray")
          # text( 0.05*K[i], F10, "10% HR", pos=1 )
          
          abline (h=F30, lty="dotted", col="gray")
          # text( 0.05*K[i], F30, "30% HR", pos=1 )
 

          abline (h=FMSY, lty="dashed", col="red" )

          # abline (h=Fhistorical, lty="dashed")
          # text( 0.05*K[i], Fhistorical, "Mean", pos=1, lwd=2 )
          
          # abline (v=Bref, lty="dotted")
          # text( Bref-0.2, 0.25, "Lower biomass reference point\n (LBRP = 0.2 * BMSY)" , srt=90, pos=3)

          abline (v=Bmsy, lty="dotted")
        
          abline (v=BK, lty="dotted")
    
          abline (v=BK25, lty="dotted")
   
          text( Bmsy-0.01*K, yl, "K/2" , srt=90, pos=3)
          text( BK-0.01*K, yl, "K" , srt=90, pos=3)
          text( BK25-0.01*K, yl, "K/4" , srt=90, pos=3)
          text( 0.05*K, Fref, "20% HR", pos=1 )
          text( 0.05*K, FMSY, "FMSY", pos=3, lwd=2, col="red" )
          text( B[hdat], F[hdat],  labels=yrs0, pos=3, cex= 0.8 )
  
          
          # abline (v=Bhistorical, lty="dashed")
          # text( Bhistorical-0.01*K[i], yl, "Mean" , srt=90, pos=3,  lwd=2)

        }
      
      
      if (vname=="default.unmodelled") {
      
        B =  sb$IOA
          F =  apply( y$F, c(1,2), mean, na.rm=T  )
            
          areas = c("cfa4x", "cfasouth", "cfanorth" )
          regions = c("4X", "S-ENS", "N-ENS")
    
          td = exploitationrates(p=p, areas=areas, labels=regions, CFA4X.exclude.year.assessment=FALSE )
   
          K =  apply( y$K, c(1), mean, na.rm=T  )
          FMSY = apply( y$FMSY, c(1), mean, na.rm=T  )
          BMSY = apply( y$BMSY, c(1), mean, na.rm=T  )

        for (i in 1:3 ) {
          ylims = c(0, FMSY[i] * 1.25)
          plot( B[hdat,i], F[hdat,i],  type="b", xlim=c(0, K[i] * 1.1 ),  
            ylim=ylims, col="darkorange", cex=0.8, lwd=2, xlab="", ylab="", pch=20 )

          # nn = as.matrix( cbind( Bx=as.vector( y$B[ndata,i,,] ), Fx = as.vector( y$F[ndata,i,,] ) ))
          # ellipse.2d(nn[,1], nn[,2], pv=0.05, sc=30)
          
          if (i==3) title( xlab="Fishable biomass (kt)" ) 
          if (i==2) title( ylab="Fishing mortality" ) 

          F30 = -log(1-0.3)
          F10 = -log(1-0.1)

          Fref =  0.22
          Bmsy = BMSY[i]
          Bref = K[i] * 0.2 
          BK = K[i] 
          BK25 = K[i] * .25 
          Fhistorical = mean( F[hdat,i], na.rm=T )
          Bhistorical = mean( B[hdat,i], na.rm=T ) 
          yl = 0.05
          
       
          abline (h=Fref, lty="solid", col="gray", lwd=2 )
    
          abline (h=F10, lty="dotted", col="gray")
          # text( 0.05*K[i], F10, "10% HR", pos=1 )
          
          abline (h=F30, lty="dotted", col="gray")
          # text( 0.05*K[i], F30, "30% HR", pos=1 )
 

          abline (h=FMSY[i], lty="dashed", col="red" )

          # abline (h=Fhistorical, lty="dashed")
          # text( 0.05*K[i], Fhistorical, "Mean", pos=1, lwd=2 )
          
          # abline (v=Bref, lty="dotted")
          # text( Bref-0.2, 0.25, "Lower biomass reference point\n (LBRP = 0.2 * BMSY)" , srt=90, pos=3)

          abline (v=Bmsy, lty="dotted")

          abline (v=BK, lty="dotted")
    
          abline (v=BK25, lty="dotted")
        
          text( 0.05*K[i], Fref, "20% HR", pos=1 )
          text( 0.05*K[i], FMSY[i], "FMSY", pos=3, lwd=2, col="red" )
          text( BK-0.01*K[i], yl, "K" , srt=90, pos=3)
          text( Bmsy-0.01*K[i], yl, "K/2" , srt=90, pos=3)
          text( BK25-0.01*K[i], yl, "K/4" , srt=90, pos=3)
          text( B[hdat,i], F[hdat,i],  labels=yrs0, pos=3, cex= 0.8 )
  
          text( 0, ylims[2]*0.9,  labels=labs[i], pos=3, cex= 0.85 )



          # abline (v=Bhistorical, lty="dashed")
          # text( Bhistorical-0.01*K[i], yl, "Mean" , srt=90, pos=3,  lwd=2)

        }
      }
   
      if (vname=="simple") {
        require(car)
     
          B =  apply( y$B, c(1,2), mean, na.rm=T  )
          F =  apply( y$F, c(1,2), mean, na.rm=T  )
          C =  apply( y$C, c(1,2), mean, na.rm=T  )
          K =  apply( y$K, c(1), mean, na.rm=T  )
#          for (i in 1:3) C[,i] = C[,i]  * K[i]
          FMSY = apply( y$FMSY, c(1), mean, na.rm=T  )
          BMSY = apply( y$BMSY, c(1), mean, na.rm=T  )
        
          labs = c("N-ENS", "S-ENS", "4X")

        for (i in 1:3 ) {
          ylims = max(C[,i] )* c(0, 1.1)
          plot( B[hdat,i], C[hdat,i],  type="l", xlim=c(0, K[i]*1.05  ),  
            ylim=ylims, xlab="", ylab="",  lwd=2, col="darkorange" )

          abline(0,0.1, lty="dotted", lwd=2, col="gray" )
          abline(0,0.2, lwd=3, col="gray" )
          abline(0,0.3, lty="dotted", lwd=2, col="gray" )

          points( B[hdat,i], C[hdat,i], col="orange", cex=0.8, pch=20 )
          text( B[hdat,i], C[hdat,i],  labels=yrs0, pos=3, cex= 0.85 )
          
          text( 0, ylims[2]*0.9,  labels=labs[i], pos=3, cex= 0.85 )

          # nn = as.matrix( cbind( Bx=as.vector( y$B[ndata,i,,] ), Fx = as.vector( y$F[ndata,i,,] ) ))
          # ellipse.2d(nn[,1], nn[,2], pv=0.05, sc=30)

          if (i==3) title( xlab="Fishable biomass (kt)" ) 
          if (i==2) title( ylab="Catch (kt)" ) 

          Cmsy = ( exp( FMSY[i] ) - 1) 
          Cref = ( exp( FMSY[i] * 0.2 ) - 1) * K[i]
          Bmsy = BMSY[i]
          Bref = K[i] * 0.2 
          BK = K[i] 
          BK25 = K[i] * .25 
          Chistorical = mean( C[hdat,i], na.rm=T )
          Bhistorical = mean( B[hdat,i], na.rm=T ) 
          yl = 0.1 * max(C[hdat,i])
          
          # abline (h=Fref, lty="dotted")
          # text( 0.25, Fref, "Target\n (0.2 * FMSY) ", pos=1 )

          abline (0, Cmsy, lty="dotted", col="red")
          # text( 0.1*K[i], Cmsy, "FMSY", pos=1 )

          # abline (h=Chistorical, lty="dashed")
          # text( 0.1*K[i], Chistorical, "Mean", pos=3, lwd=2 )
          
          # abline (v=Bref, lty="dotted")
          # text( Bref-0.2, 0.25, "Lower biomass reference point\n (LBRP = 0.2 * BMSY)" , srt=90, pos=3)

        
          abline (v=BK, lty="dotted")
          text( BK-0.01*K[i], yl, "K" , srt=90, pos=3)
  
          abline (v=BK/2, lty="dotted")
          text( BK/2-0.01*K[i], yl, "K/2" , srt=90, pos=3)
    
          abline (v=BK25, lty="dotted")
          text( BK25-0.01*K[i], yl, "K/4" , srt=90, pos=3)

        }


      }
    }


    
    if (type=="diagnostic.phase") {
      
      B =  apply( y$B, c(1,2), mean, na.rm=T  )
      K =  apply( y$K, c(1), mean, na.rm=T  )
         
      for (i in 1:3 ) {
        plot( B[1:ndata-1,i], B[2:ndata,i],  type="b", xlab="t", ylab="t+1",
          xlim=c(0, K[i] * 1.25 ), ylim=max(K[i] )* c(0, 1.25), lwd=2, col="darkorange" )

         # abline(0,0.1, lty="dotted", lwd=2, col="gray" )
         abline( coef=c(0,1) )
       #text( B[1:ndata-1,], B[2:ndata,],  labels=yrs4 , pos=4, cex=0.8 )
      }
    }




    if (type=="diagnostic.production") {
       
      B =  apply( y$B, c(1,2), mean, na.rm=T  )
      P =  apply( y$P, c(1,2), mean, na.rm=T  )

      MSY = apply( y$MSY, c(1), mean, na.rm=T  )
      FMSY = apply( y$FMSY, c(1), mean, na.rm=T  )
      BMSY = apply( y$BMSY, c(1), mean, na.rm=T  )
      C =  apply( y$C, c(1,2), mean, na.rm=T  )
      K =  apply( y$K, c(1), mean, na.rm=T  )
         
      # production vs biomass
      x11()
      layout( matrix(c(1,2,3), 3, 1 ))
      par(mar = c(5, 4, 0, 2))
      for (i in 1:3) {
        plot( B[,i], P[,i], type="n", pch=20, ylim=c(0, max( c(P[,i], MSY[i]))*1.1), xlim=c(0,K[i]*1.05), xlab="Biomass; kt", ylab="Yield; kt"  )
        a = MSY[i] / (BMSY[i])^2 
        curve( -a*(x-BMSY[i])^2 + MSY[i], from=0, to=K[i], add=TRUE, lwd=3, col="gray" )
        abline(v=BMSY[i], lty="dotted", lwd=3, col="gray")
        points( B[,i], P[,i], type="p", pch=20  )
        text( B[,i], P[,i], yrs0, pos=3, cex=0.8 )
        # abline(h=0)
      }
   
    }


    if (is.null(fn)) fn = paste(vname, "tmp", ".png", sep="" )
  if(save.plot) savePlot( filename=fn, type="png" )
    return( fn)

  }


 


