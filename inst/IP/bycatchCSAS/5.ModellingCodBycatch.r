
require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()


wd = ('C:/Users/CookA/Desktop/dellshared/Bycatch in the Lobster Fishery')
setwd(wd)




aA = read.csv(file=file.path('results','CompliedDataForModelling.csv'))


###predicting lobster landings from obs
#Cod v lob catches
gP = glm(Cod~Period+GP+Lobster,data=aA,family=poisson(link='log'))
zP = zeroinfl(Cod~Period+GP+Lobster,data=aA, dist='negbin')




#poisson and test for dispersion
gP = glm(Cod~Period+GP+LobsterWt,data=aA,family=poisson(link='log'))
require(AER)
dispersiontest(gP) #not surprising over dispersion, >0 over; <0 under, ~0 none

#Prediction data
aAs = subset(aA,!is.na(GridGroup))

bb = aggregate(NUM_OF_TRAPS~LFA+GridGroup+Period+SYEAR,data=b,FUN=sum)
bb$GP = paste(bb$LFA,bb$GridGroup,sep="-")

bba = aggregate(NUM_OF_TRAPS~GP+Period, data=bb,FUN=mean)
aAsL = aggregate(LobsterWt~Period+GP,data=aA,FUN=mean)

newdat = merge(bba,aAsL)

#zero inflated poisson 
require(pscl)
zP = zeroinfl(Cod~Period+GP+LobsterWt,data=subset(aA,!is.na(GridGroup)), dist='poisson')
predzp<- predict(zP,type = "response")
rmsemodelzp<-ModelMetrics::rmse(aA$Cod,round(predzp))
maemodelzp<-ModelMetrics::mae(aA$Cod,round(predzp))
zPnull <- update(zP, . ~ Period+GP)
pchisq(2 * (logLik(zP) - logLik(zPnull)), df = 3, lower.tail = FALSE)


newdat$preds = predict(zP,newdata = newdat,type='response')
newdat$CodPred = newdat$NUM_OF_TRAPS * newdat$preds
f

zNB = zeroinfl(Cod~Period+GP+LobsterWt,data=subset(aA,!is.na(GridGroup)), dist='negbin')
predznb<- predict(zNB,newdata=subset(aA,!is.na(GridGroup)),type = "response")
rmsemodelznb<-ModelMetrics::rmse(aA$Cod,round(predznb))
maemodelznb<-ModelMetrics::mae(aA$Cod,round(predznb))


##(https://freakonometrics.hypotheses.org/tag/tweedie)
require(statmod)
gt = glm(Cod~LobsterWt,data=aA,family=tweedie(var.power=1.5, link.power=0))
pente=function(gamma) summary(glm(Cod~LobsterWt,family=tweedie(var.power=gamma,link.power=0),data=aA))$coefficients[2,1:2]
Vgamma = seq(0,2,by=.05)
Vpente = Vectorize(pente)(Vgamma)
plot(Vgamma,Vpente[1,],type="l",lwd=3,xlab="power",ylab="slope")

lW = .5
CC = aA[which(round(aA$LobsterWt,2)==round(lW,2)),'Cod']
err=function(gamma) predict(glm(y~x,family=tweedie(var.power=gamma,link.power=0),data=base),newdata=data.frame(LobsterWt=1),type="response")-y[x==1] 
Verreur = Vectorize(erreur)(Vgamma)
plot(Vgamma,Verreur,type="l",lwd=3,ylim=c(.001,.32),xlab="power",ylab="error")

#Comparing Temporal sampling with Fishing (by LFA)
b = bycatch.db('logbook.merge',wd=wd) 
bW = aggregate(NUM_OF_TRAPS~WOS+SYEAR+LFA,data=b,FUN=sum)


aO = bycatch.db('ISDB.reshape')
aS = bycatch.db(DS='SWLSS')

              ii = c(33,34,35)
              jj = 2019:2021
              par(mfrow=c(3,3))
              for(i in ii){
                for(j in jj){
                  
                      bss = subset(bW,LFA==i & SYEAR==j)
                      bss$CE = cumsum(bss$NUM_OF_TRAPS)/sum(bss$NUM_OF_TRAPS)
                      plot(bss$WOS,bss$CE,type='l',lwd=2,xlab='WOS',ylab='Cumulative Effort',main=paste(i,j,sep="-"))
                      
                      aOs = subset(aO,LFA==i & SYEAR==j)
                      if(nrow(aOs)>0){
                        print(c(i,j))
                      aOs = aggregate(UID~WOS,data=aOs,FUN=function(x) length(unique(x)))
                      aOs$CE = cumsum(aOs$UID)/sum(aOs$UID)
                      lines(aOs$WOS,aOs$CE,type='l',lwd=2,col='red',lty=2)
                      }
                      aSs = subset(aS,LFA==i & SYEAR==j)
                      if(nrow(aSs)>0){
                        print(c(i,j))
                      aSs = aggregate(UID~WOS,data=aSs,FUN=function(x) length(unique(x)))
                      aSs$CE = cumsum(aSs$UID)/sum(aSs$UID)
                      lines(aSs$WOS,aSs$CE,type='l',lwd=2,col='green',lty=3)
                      }
                }
              }

            #by year, by lfa by grid group
            
            
            ii = c(33,34,35)
            jj = 2019:2021
            pdf('Figures/SamplingbyArea.pdf')
            for(i in ii){
              for(j in jj){
                
                bss = subset(b,LFA==i & SYEAR==j)
                gg = c(na.omit(unique(bss$GridGroup)))
                for(g in gg){
                bsst = subset(bss,GridGroup==g)
                bsst = aggregate(NUM_OF_TRAPS~WOS, data=bsst,FUN=sum)
                bsst$CE = cumsum(bsst$NUM_OF_TRAPS)/sum(bsst$NUM_OF_TRAPS)
                plot(bsst$WOS,bsst$CE,type='l',lwd=2,xlab='WOS',ylab='Cumulative Effort',main=paste(i,j,g,sep="-"))
                
                aOs = subset(aO,LFA==i & SYEAR==j & GridGroup==g)
                if(nrow(aOs)>0){
                  print(c(i,j))
                  aOs = aggregate(UID~WOS,data=aOs,FUN=function(x) length(unique(x)))
                  aOs$CE = cumsum(aOs$UID)/sum(aOs$UID)
                  lines(aOs$WOS,aOs$CE,type='l',lwd=2,col='red',lty=2)
                }
                aSs = subset(aS,LFA==i & SYEAR==j& GridGroup==g)
                if(nrow(aSs)>0){
                  print(c(i,j))
                  aSs = aggregate(UID~WOS,data=aSs,FUN=function(x) length(unique(x)))
                  aSs$CE = cumsum(aSs$UID)/sum(aSs$UID)
                  lines(aSs$WOS,aSs$CE,type='l',lwd=2,col='green',lty=3)
                  }
                }
              }
            }
            graphics.off()
            #by y
