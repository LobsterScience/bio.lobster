#LFA 38
require(bio.lobster)

fd=file.path(project.datadirectory('Framework_LFA35_38'),'outputs','SURVEYS')
setwd(fd)
io = readRDS('IndicesFromFullComboModelOct92000+.rds')
ind35 = io[[3]]

ioF = readRDS('IndicesFromFullComboModelSept26.rds')
ind35F = ioF[[3]]


##converting number to biomass using fall length frequencies from all fall surveys in bay of fundy

yp = ILTS_ITQ_All_Data(redo_base_data = F,size=c(82,300),aggregate=F,species=2550,biomass = F,extend_ts = F)
yp2 = subset(yp, month(SET_DATE)>8)

yp2 = aggregate(SA_CORRECTED_PRORATED_N~FISH_LENGTH,data=yp2,FUN=mean)
names(yp2)[2]='meanden'
yp2$Prop = yp2$meanden/sum(yp2$meanden)
yp2$Weight = lobLW(CL=yp2$FISH_LENGTH,sex = 1)
yp2s = subset(yp2,Prop>0)
mnW = sum(yp2s$Prop*yp2s$Weight)

ind35$estB = ind35$est*mnW/1000
ind35$lwrB = ind35$lwr*mnW/1000
ind35$uprB = ind35$upr*mnW/1000


land = lobster.db('seasonal.landings')
land$year = substr(land$SYEAR,6,9)
ind35$year = ind35$year+1
Da = merge(ind35,land[,c('year','LFA38')])

			input1 <-  list()
			input1$N <- nrow(Da)
			input1$I <- Da$estB/1000
			input1$C <- Da$LFA38
		
			
			
#Two phase K
#Add in Priors
			Ku<-max(input1$I)
			
			#rs <- find.moments(lo=0.1,up=0.8,l.perc=0.05,u.perc=0.95,dist='norm')
 			
			k2s <- findMoments(lo=1/Ku,up=1/(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K2s <<- findMoments(lo=Ku,up=(Ku*5),l.perc=0.05,u.perc=0.95,dist='lnorm')
			
			input1$r.a<-9 ; input1$r.b <- 1/0.993347893 #informative prior from mcallister work
			input1$k2.a<-k2s[[1]] ; input1$k2.b <- k2s[[2]]
			input1$q.a<-0.01 ; input1$q.b <- 1
			#Process = a b observation=c d
			input1$a0<-0.000001;  input1$b0<-5; 
			input1$c0<-0.000001; input1$d0<-5;
			
			
			
spBUGS3 <- function(input=input1,inits=inits,n = 10000, burn = 2000, thin = 10, debug = F, wd=getwd()){
			  require(R2OpenBUGS)
			  
			  #	Initial values for WinBUGS one for each chain
			  inits <- list(list(P=rep(0.75,times=input$N),
			                     r2=0.8,
			                     k2=0.003,
			                     q2=0.4,
			                     P.p = matrix(0.1,ncol=4,nrow=6)
			  ),
			  list(P=rep(0.8,times=input$N),
			       r2=0.9,
			       k2=0.005,
			       q2=0.4,
			       P.p = matrix(0.1,ncol=4,nrow=6)
			       
			  ))
			  
			  # Parameters to be returned 
			  parameters <- c('Tau21','Sigma21','r2','K2','q2','B','Imean','P.res','BMSP2','BMSPc2','MSP2','MSPc2','FMSP2','FMSPc2')
			  
			  
			  ## Call to WinBUGS ##
			  tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = file.path("C:",'Users',"CookA","Documents","git","bio.lobster","inst","SPmodel","BoFSp", "sp2000p.txt"), 
			              n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin,  debug = debug)
			  return(tmp)
			} 

a<- spBUGS3(debug=F)
			
#########plots
op<-par(mfrow=c(3,2),mar=c(4,4,3,1))
# r
plot(density(rnorm(10000,input1$r.a,sqrt(input1$r.b))),xlab='r',xlim=c(0,1.5),lty=2,main='')
#lines(density(a$sims.list$r1),col='red',lty=1)
lines(density(a$sims.list$r2),col='blue',lty=1)


#K
plot(density(rlnorm(10000,K2s[[1]],sqrt(K2s[[2]]))),xlab='K',lty=2,main='')
#lines(density(rlnorm(10000,K2s[[1]],sqrt(K2s[[2]]))),lty=2)
#lines(density(a$sims.list$K1),col='red',lty=1)
lines(density(a$sims.list$K2),col='blue',lty=1)

#q
plot(1,1,type='n',xlim=c(input1$q.a/1.1,input1$q.b*1.1),xlab='q',main='',ylab='Density')
polygon(x=c(input1$q.a,input1$q.a,input1$q.b,input1$q.b),y=c(0,dunif(c(input1$q.a,input1$q.b),input1$q.a,input1$q.b),0))
#lines(density(a$sims.list$q1),col='red',lty=1)
lines(density(a$sims.list$q2),col='blue',lty=1)

#process errors
plot(1,1,type='n',xlim=c(0,3),xlab='Process Variance',main='',ylab='Density',ylim=c(0,0.6))
polygon(x=c(0.25^2,0.25^2,5,5),y=c(0,0.4,0.4,0),border='black',lty=2)
#lines(density(sqrt(a$sims.list$Sigma2)),col='red',lty=1,lwd=1)
lines(density(sqrt(a$sims.list$Sigma21)),col='blue',lty=1,lwd=1)

plot(1,1,type='n',xlim=c(0,1),xlab='Observation Variance',main='',ylab='Density',ylim=c(0,0.6))
polygon(x=c(0.25^2,0.25^2,1,1),y=c(0,0.4,0.4,0),border='black',lty=2)
#lines(density(sqrt(a$sims.list$Tau2)),col='red',lty=1,lwd=1)
lines(density(sqrt(a$sims.list$Tau21)),col='blue',lty=1,lwd=1)


plot(Da$year,a$median$P.res,type='h',ylab='Residuals',xlab='Year')
abline(h=0,col='red')

graphics.off()

##############################

dat1<-data.frame(years=Da$year,Biomass=0,Landings=0)
dat1$Biomass<-a$mean$B
dat1$Landings<-input1$C

dat1$ASP <- 0
for(i in 1:(nrow(dat1)-1)) {
  dat1[i,'ASP'] <- dat1[i+1,'Biomass'] - dat1[i,'Biomass'] +dat1[i,'Landings'] 
}

#MSY V B plots
plotArrows <- function (Y,X) {
  #//trace arrows around x,y points from start to end
  xl <- c(min(X),max(X))
  yl <- c(min(Y),max(Y))
  if(Y[length(Y)]==0) {Y<-Y[-length(Y)]}
  if(length(Y) !=length(X)) {X<-X[-length(X)]}
  n <- length(Y)
  cl <- rainbow(n)
  plot(1,1,ylim=yl,xlim=xl,type='n',xlab='Biomass',ylab = 'Annual Surplus Production')
  
  
  for(i in 1:n) {
    arrows(x0=X[i],x1=X[i+1],y0=Y[i],y1=Y[i+1],angle=45,length=0.1,lwd=2.5,col=cl[i])
  }
  points(X,Y,pch=16,cex=1,col='black')
  points(X[1],Y[1],pch=17,cex=2,col='black')
  points(X[n],Y[n],pch=17,cex=2,col='black')
}

plotArrows(dat1$ASP,dat1$Biomass)



#MSY plots with 40 and 80 BMSY
af=3
fit <- lower <- upper <- numeric()
N<-(Da$year)
n<-length(N)
for(i in 1:length(N)) {
  fit[i]<-median(a$sims.list$B[,i])
  lower[i]<-quantile(a$sims.list$B[,i],0.25)
  upper[i]<-quantile(a$sims.list$B[,i],0.75)
}
yl <- c(min(c(fit,lower,upper)),max(c(fit,lower,upper)))
plot(1,1, type='n',ylab='Biomass',xlab='Year',ylim=yl,xlim=c(min(N),max(N)))
#polygon(c(1992,2011,2011,1992),c(0.8*a$median$BMSP,0.8*a$median$BMSP,0.4*a$median$BMSP,0.4*a$median$BMSP),density=10,col='orange')
lines(N,fit)
lines(N,lower,lty=2)
lines(N,upper,lty=2)

polygon(c(1976,2024,2024,1976),c(0.4*a$median$BMSP2,0.4*a$median$BMSP2,0.4*a$median$BMSP2,0.4*a$median$BMSP2),density=10,col='blue')
#legend('topright',fill=c('orange','blue'),c('Det','Sto'),bty='n',cex=0.8,density=20)


par(mfrow=c(2,2))

plot(density(a$sims.list$BMSP1),main='',xlab='BMSP')
lines(density(a$sims.list$BMSP2),main='',xlab='BMSP')

plot(density(a$sims.list$MSP*10^af),main='',xlab='MSP')
lines(density(a$sims.list$MSPc*10^af),col='red')

plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
legend(0.2,0.7,c('Det','Sto'),lty=c(1,1),col=c('black','red'),lwd=2,bty='n')

dev.off()
}
#---------------------------

Land=input1$C
dd = data.frame(fit,lower,upper,N,Land)

ggplot(dd,aes(x=N,y=fit,ymin=lower,ymax=upper))+geom_point()+geom_ribbon(alpha=.25)+geom_path()+theme_test(base_size = 14)+labs(x='Year',y='Biomass')+geom_hline(yintercept=a$median$BMSP2*0.4, colour='blue',linewidth=1.2)
dd$lwrF = U2F(dd$Land/dd$lower)
dd$uprF = U2F(dd$Land/dd$upper)
dd$F = U2F(dd$Land/dd$fit)


ggplot(dd,aes(x=N,y=F,ymin=lwrF,ymax=uprF))+geom_point()+geom_ribbon(alpha=.25)+geom_path()+theme_test(base_size = 14)+labs(x='Year',y='Fishing Mortality')+geom_hline(yintercept=a$median$FMSP2, colour='blue',linewidth=1.2)+scale_y_continuous(limits=c(0,2.5))


