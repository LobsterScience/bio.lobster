require(bio.lobster)
require(bio.utilities)
require(lubridate)
a = lobster.db('atSea')
head(a)
head(atSea)
a = subset(atSea,SPECIESCODE==2550)
a = subset(atSea,SPECIESCODE==2550 & SEX %in% 2:3 & CARLENGTH>40 & CARLENGTH<130)
a$yr=year(a$STARTDATE)
a$syr = round(a$yr/5)*5
a$cl = round(a$CARLENGTH)

re = aggregate(TRIPNO~LFA+syr+cl,data=a,FUN=length)
reb = aggregate(TRIPNO~LFA+syr+cl,data=subset(a,SEX==3),FUN=length)
reb$N = reb$TRIPNO
reb$TRIPNO=NULL
rea = merge(re,reb,all=T)
rea = na.zero(rea)
rea$pB = rea$N/rea$TRIPNO



##bino
xG = subset(a,LFA %in% c('31A','31B'))
xG$Berried = ifelse(xG$SEX==3,1,0)
df=data.frame(CARLENGTH=40:110)
xM = glm(Berried~CARLENGTH,data=subset(xG,syr==2015,CARLENGTH<110),family=binomial(link = 'logit'))

##bino2
require(mgcv)
oxMA = gam(cbind(N,TRIPNO-N)~s(cl),data=subset(rea,LFA==27 & syr==1990),family=binomial(link = 'logit'))
plot(subset(rea,LFA==27 & syr==1990)$cl,predict(xMA,type='response'))


##nonparametric smoother
# local averaging (cv span selection)
#http://users.stat.umn.edu/~helwig/notes/smooth-notes.html
locavg <- with(subset(rea,LFA==27 & syr==2015), supsmu(cl, pB))
# local regression (gcv span selection)
locreg <- with(subset(rea,LFA==27 & syr==2015), loess.gcv(cl, pB))
# kernel regression (gcv span selection)
kern <- with(subset(rea,LFA==27 & syr==2015), ksmooth.gcv(cl, pB))




with(subset(rea,LFA==27 & syr==2015),plot(cl,pB))
lines(locavg,lwd=2)
lines(locreg,lwd=2,col='red')
lines(kern,lwd=2,col='blue')
legend("topleft", c("supsmu", "loess", "ksmooth"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")


#find local maxima to id peak

kP = findingCL(x=kern$x,y=kern$y,m=7)
lrP = findingCL(x=locreg$x,y=locreg$y,m=7)
laP = findingCL(x=locavg$x,y=locavg$y,m=7)




#nls following Lebris using proportions

m1 = formula(pB~1/(1+(exp(a+(b*cl))))) #general
m2 = formula(pB~1/(1+(exp(-a * (cl-b))))) # a is slope b is som50 
m3 = formula(pB~d/(1+(exp(-a * (cl-b))))) # a is slope b is som50 , d = asymptote which is fixed



lf = unique(rea$LFA)
lf = lf[-2] #no lfa 28
li = list()
m=0
for(k in 1:length(lf)){
    Y = unique(subset(rea,LFA==lf[k],select=syr))[,1]
      for(i in 1:length(Y)){

                x = subset(rea,LFA==lf[k]&syr==Y[i])
                locavg <- with(x, supsmu(cl, pB))
                locreg <- with(x, loess.gcv(cl, pB))
                kern <- with(x, ksmooth.gcv(cl, pB))
                #find local maxima to id peak

                
                with(x,plot(cl,pB))
                lines(locavg,lwd=2)
                lines(locreg,lwd=2,col='red')
                lines(kern,lwd=2,col='blue')
                legend("topleft", c("supsmu", "loess", "ksmooth"),
                       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")
                
                
                                
                maxs=list()
                maxs[[1]] = c(findingCL(x=kern$x,y=kern$y,m=7),'kern')
                maxs[[2]] = c(findingCL(x=locreg$x,y=locreg$y,m=7),'localreg')
                maxs[[3]] = c(findingCL(x=locavg$x,y=locavg$y,m=7),'localavg')
                points(x$cl[as.numeric(maxs[[1]][1])],x$pB[as.numeric(maxs[[1]][1])],pch=16,col='blue')
                points(x$cl[as.numeric(maxs[[2]][1])],x$pB[as.numeric(maxs[[2]][1])],pch=16,col='red')
                points(x$cl[as.numeric(maxs[[2]][1])],x$pB[as.numeric(maxs[[2]][1])],pch=16,col='black')
                
                if(m<1000){
                  question1 <- readline("Would you like to skip this iteration? (Y/N)")
                  if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
                    next
                  }
                
                for(n in 1:length(maxs)){
                  m=m+1
                  xi = subset(x,cl<=maxs[[n]][2])
                  di= max(xi$pB)
                i1=try(nls(m3,data=xi,start=list(a=.2,b=(as.numeric(maxs[[n]][2])-10),d=di),lower=list(a=-10,b=30,d=di),upper=list(a=10,b=130,d=di),weights=TRIPNO,algorithm = 'port'),silent=T)  #weighted by sample size
                if(class(i1)=='try-error') next
                      rms = rmse(xi$pB,predict(i1))
                      ma = mae(xi$pB,predict(i1))
                      li[[m]] = c(lf[k],Y[i],coef(i1)[1],coef(i1)[2],di,rms,ma,sm,sum(xi$TRIPNO),sum(xi$N),maxs[[n]][3])
                }
    #windows()
#plot(x$cl,x$pB,main=Y[i])
#cc1 = function(x,a=coef(i1)[1],b=coef(i1)[2]) 1/(1+(exp(-a *(x-b))))
#curve(cc1,from=50, to=140,add=T,col='red')
  }
      }
}
som = as.data.frame(do.call(rbind,li))
names(som) = c('LFA','SYR','Slope','Som50','max','rmse','mae','minBerr','ntot','nberr','smooth')
som = toNums(som,2:10)

require(ggplot2)
ggplot(som,aes(x=SYR,y=Som50)) +geom_point() + geom_smooth()+facet_wrap(~LFA,scales='free_y')

