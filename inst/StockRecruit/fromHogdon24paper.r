require(bio.lobster)
require(devtools)
require(ggplot2)
la()

sr = read.csv(file.path(project.datadirectory('bio.lobster'),'data','stock-recruit','SR-hogdonpaper.csv'))
names(sr)[2:3] = c('R','SSB')

sr <- sr[order(sr$SSB),]

sr$logR  = log(sr$R)
sr$SSB = sr$SSB/100

##Beverton Holt
bh.dd <- logR~log(a*SSB) - log(b+SSB) #density dependent
bh.di <- logR~log(a*SSB) #density independent
Rmax <- max(sr$R)

plot(R~SSB,data=sr)

bh.dd.nls <- nls(bh.dd,data=sr,start=list(a=0.15,b=0.01),lower=c(0.01,0.00001),upper=c(Rmax,1000), algorithm='port')
bh.di.nls <- nls(bh.di,data=sr,start=list(a=0.15),algorithm='port')


#beverton holt with gamma errors (Jiao et al 2004 CJFAS 61)
a  <- glm(R~I(1/(SSB)),data=sr,family=Gamma(link='inverse'))
b <- coef(a)
bvh_ig <- function(a,b,S) {
  1 / (a+(b/S))
}


ggplot(data=sr,aes(x=SSB,y=R)) + geom_point()+
  geom_smooth(method='glm', formula = y~I(1/x), method.args=list(family=Gamma(link='inverse')))

bvh <- function(a,b,S) {
  a*S / (1+b*S)
}


anova(bh.dd.nls,bh.di.nls)
AIC(bh.dd.nls,bh.di.nls)
dn <- data.frame(SSB=c(0,sr[order(sr$SSB),'SSB']))
plot(R~SSB,data=sr,xlim=c(0,1200),ylim=c(0,1000))
bd <- exp(predict(bh.dd.nls,newdata=dn)	)
bi <- exp(predict(bh.di.nls,newdata=dn)	)
lines(dn[,1],bd,lty=1)
lines(dn[,1],bi,lty=2)
big <- 1/(predict(a,newdata=dn)	)
lines(dn[,1],big,lty=3)
legend('topleft',lty=c(1,2,3),cex=.75,c('Beverton-Holt LN','Density Independent',"Beverton-Holt Gamma"),bty='n')

#density independent has lower AIC 

##Ricker
rk.dd <- logR~log(a)+log(SSB)-b*SSB #density dependent
rk.di <- logR~log(a*SSB) #density independent


plot(R~SSB,data=sr)

rk.dd.nls <- nls(rk.dd,data=sr,start=list(a=0.15,b=0.01),lower=c(0.01,0.0000001),upper=c(Rmax,1000), algorithm='port')
rk.di.nls <- nls(rk.di,data=sr,start=list(a=0.15))


anova(rk.dd.nls,rk.di.nls)
AIC(rk.dd.nls,rk.di.nls)

dn <- data.frame(SSB=c(0,sr[order(sr$SSB),'SSB']))
plot(R~SSB,data=sr)
bd <- exp(predict(rk.dd.nls,newdata=dn)	)
bi <- exp(predict(rk.di.nls,newdata=dn)	)
lines(dn[,1],bd,lty=1)
lines(dn[,1],bi,lty=2)
legend('topleft',lty=c(1,2),cex=.75,c('Ricker','Density Independent'),bty='n')

#density independent has marginally lower AIC ....perhaps better model, either way neither fit well

#Nonparametric #not going with AIC or AICc selection of nonparametric loess as overfitting will be a big problem (too wiggly and the number of parameters correction factor will not completly cover this off)
plot(R~SSB,data=sr)
sro <- sr[order(sr$SSB),]
a <- predict(loess(R~SSB,data=sro,span=.2))
lines(sro$SSB,a)
legend(lty=1,'Loess span=0.52',cex=.75,bty='n')

