#dri <- c(0.406428125,0.635400557,0.546394326,0.617912352,0.47904692,0.306138109,0.425384463,0.14532679,0.190812252,0.275705384,0.247734324,0.261650835,0.150248207,0.137900672,0.096043386,0.12721116,0.098580546,0.130854006,0.110983798,0.156357334,0.203163765,0.090080965,0.102308927,0.0730146,0.048147606,0.051318733,0.095459269,0.102086194,0.052133702,0.047964836,0.042373515,0.054657783,0.051947913,0.033641296,0.031778707,0.067515018,0.033808192,0.052922688,0.042054461,0.038664296,0.030865806)
#resp <- c(3.600169771,3.596411638,3.646112147,3.716003666,3.606855747,3.652494925,3.636907613,3.686208901,3.675304034,3.738824525,3.66079058,3.641537183,3.680923465,3.653170995,3.709093675,3.693432724,3.673799437,3.688009251,3.691789332,3.600773595,3.59017723,3.657143396,3.594187272,3.588794765,3.334966655,3.221189013,3.10234166,3.079498806,3.163331717,2.78987485,3.030314617,3.172552367,3.245496276,3.150516762,3.159478152,3.169142777,3.328002766,3.30813768,3.323440271,3.282328446,3.265915006)
#yr <- 1970:2010


gamTimeTrends <- 	function(y, x,niter=4999,save.loc= "C:\\Users\\CookA\\Desktop\\") {
	plot(x,y,type="b",pch=16)
	output <- list()
			require('mgcv')
				ogam <- list()
				mod = formula(y~s(x,k=20))
				test.model <- try(gam(mod),silent=T)
				if(length(test.model)>1) {
					if(summary(test.model$gam)$edf>1.2) {
				ogam[[1]] <- try(gam(mod),silent=T)
				ogam[[2]] <- try(gam(mod, correlation = corARMA(form = ~ yr, p = 1)),silent=T)
				ogam[[3]] <- try(gam(mod, correlation = corARMA(form = ~ yr, p = 2)),silent=T)
				ogam[[4]] <- try(gam(mod, correlation = corARMA(form = ~ yr, p = 3)),silent=T)
				aic.ogam <- c(AIC(ogam[[1]]$lme),AIC(ogam[[2]]$lme),AIC(ogam[[3]]$lme),AIC(ogam[[4]]$lme))
				mod <- which.min(floor(aic.ogam/2)*2)
				model <- ogam[[mod]]$gam
				mm <- matrix(NA,nrow=length(yr),ncol=niter+1)
				fd <- matrix(NA,nrow=length(yr),ncol=niter+1)
				sd <- matrix(NA,nrow=length(yr),ncol=niter+1)
				rs <- model$residuals
				fs <- model$fitted
				dd <- try(derivs(m=model,yr=yr),silent=T)
				if(length(dd)>1	) {
				mm[,1] <- model$fitted
				fd[,1] <- dd[[1]]
				sd[,1] <- dd[[2]]
			#boot strapping
			for(i in 1:niter) {
					ri <- sample(rs,length(rs),replace=T)
					new.resp <- as.numeric(fs+ri)
					new.model <- try(gamm(new.resp ~ s(yr,k=20)),silent=T)
					if(length(new.model)>1) {
					dd <- try(derivs(m=new.model$gam,yr=yr),silent=T)
					mm[,i+1] <- new.model$gam$fitted
					fd[,i+1] <- dd[[1]]
					sd[,i+1] <- dd[[2]]
					}
				}
			
			#95% CI bootstrap
				mmCI <- matrix(NA,nrow=length(rs),ncol=3)
				fdCI <- matrix(NA,nrow=length(rs),ncol=3)
				sdCI <- matrix(NA,nrow=length(rs),ncol=3)
				for(i in 1:nrow(fd)) {
					mmCI[i,] <- quantile(mm[i,],probs=c(0.025,0.5,0.975),na.rm=T)
					fdCI[i,] <-quantile(fd[i,],probs=c(0.025,0.5,0.975),na.rm=T)
					sdCI[i,] <-quantile(sd[i,],probs=c(0.025,0.5,0.975),na.rm=T)
			}
			slopes.pos <- which(fdCI[,1]>0 & fdCI[,2]>0 & fdCI[,3]>0)
			slopes.neg <- which(fdCI[,1]<0 & fdCI[,2]<0 & fdCI[,3]<0)
			thresholds <- which(sdCI[,1]>0 & sdCI[,2]>0 & sdCI[,3]>0)
			par(mar=c(5,4,3,5),xpd=T)
			plot(yr,mmCI[,2],type='l',ylab=indicator.name,ylim=c(min(resp),max(resp)),xlab='Year')
			polygon(c(yr,rev(yr)),c(mmCI[,1],rev(mmCI[,3])),col='grey',border='grey')
			lines(yr,mmCI[,2],lwd=2)
			points(yr,resp,pch=16,cex=0.5)
			lines(yr[slopes.pos],mmCI[slopes.pos,2],lwd=4,col='red')
			lines(yr[slopes.neg],mmCI[slopes.neg,2],lwd=4,col='blue')
			lines(yr[thresholds],mmCI[thresholds,2],lwd=4,col='green')
			legend('topright',inset=c(-0.2,0),lty=c(1,1,1,1),lwd=c(1,4,4,4),col=c('black','blue','red','green'),bty='n',cex=0.7,c('GAM model','Sig. -ve','Sig. +ve','Threshold'))	
			db <- data.frame(Positive.slope.years = rep(NA,45),Negative.slope.years=NA,Threshold.years=NA)
			if(length(slopes.pos)>0) db$Positive.slope.years[1:length(slopes.pos)] <- yr[slopes.pos]
			if(length(slopes.neg)>0) db$Negative.slope.years[1:length(slopes.neg)] <- yr[slopes.neg]
			if(length(thresholds)>0) db$Threshold.years[1:length(thresholds)] <- yr[thresholds]
			db <- db[apply(db, 1, function(y) !all(is.na(y))),] #remove rows that are all na
			output[[1]]<-db
				}
			}
		}
			return(output)
}					
	
	


derivs <- function(m=model,x=x,eps=0.01) {
				pre 	<- predict(m,type='response',se.fit=T)
				mod1 	<- predict(m,type='lpmatrix')
				newx 	<- data.frame(x=x-eps)
				mod2 	<- predict(m,newx,type='lpmatrix')
				modp 	<- (mod1-mod2)/eps
				fd_mod 	<- modp %*% coef(m) #first derivative
				model2 	<- gam(fd_mod~s(yr,k=20))
				mod3 	<- predict(model2,type='lpmatrix')
				mod4 	<- predict(model2,newyr,type='lpmatrix')
				modpp 	<- (mod3-mod4)/eps
				sdmod 	<- modpp %*% coef(model2) 
			return(list(fd=fd_mod,sd=sdmod))
				}
