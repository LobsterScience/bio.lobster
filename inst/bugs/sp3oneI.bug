model {
        for(i in 1:N) {
				Imean[i] <- log(q*K*P[i]);
				I[i] ~ dlnorm(Imean[i],itau2);
                 B[i] <- K*P[i] 
                 }
        
#Process equation
	#P[1]
	
	Pmean[1] <- log(P0)	;
	P[1] ~ dlnorm(Pmean[1],isigma2)
	P.res[1] <- log(P[1])-Pmean[1]
	
	#P[2:N]
	for (i in 2:N) {
		    Pmean[i] <- log(max(P[i-1] + r*P[i-1]*(1-P[i-1]) - k*C[i-1],0.01));
		    P[i] ~ dlnorm(Pmean[i],isigma2);
		    P.res[i] <- log(P[i]) - Pmean[i]
		    }

   for(i in 1:N) {
       F[i] <- -log( max(1 - C[i] / B[i], 0.0001))  
     }
  

#####Prior on r######

		r~runif(r.a,r.b);
				
#####Prior on k######
		i.k.b<-1/k.b
		k ~ dlnorm(k.a,i.k.b);
		K <- 1/k;	
		
#Prior on P0
P0 ~dunif(P0.a,P0.b)
		
#####Prior on Q#####
		
		q ~ dunif(q.a,q.b)
				
#######Priors on isigma2 and itau2#####
	
		sd.p ~ dunif(0.01,7)
		sd.o ~ dunif(0.01,7)
		isigma2 <- pow(sd.p,-2)
		itau2    <- pow(sd.o,-2)
			
	}
