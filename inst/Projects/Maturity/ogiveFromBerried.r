require(bio.lobster)
require(bio.utilities)
require(lubridate)
lobster.db('atSea')
a = subset(atSea,SPECIESCODE==2550)
a = subset(atSea,SPECIESCODE==2550 & SEX %in% 2:3 & CARLENGTH>40 & CARLENGTH<130)
a$yr=year(a$STARTDATE)
a$SYEAR = ifelse(month(a$STARTDATE) %in% c(10,11,12) & a$LFA %in% c(33,34,35,36,38),a$yr+1,a$yr)

a$syr = round(a$SYEAR/5)*5
a$cl = round(a$CARLENGTH)

#remove likely errors by LFA
g  = aggregate(TRIPNO~cl+LFA+SYEAR,data=subset(a,SEX==3),FUN=length)
g = split(g,f=list(g$LFA,g$SYEAR))
g = rm.from.list(g)
out = data.frame(LFA=NA,minCL=NA,SYEAR=NA)
for(i in 1:length(g)){
  k = g[[i]]
  l = which(diff(k$cl)==1)[1]
  out[i,] = c(unique(k$LFA),k$cl[l],unique(k$SYEAR))  
}


out = toNums(out,cols=2:3)
i = which(out$LFA==33 & out$minCL<60)
out = out[-i,]
i = which(out$LFA==38 & out$minCL<70)
out = out[-i,]

ggplot(out,aes(x=SYEAR,y=minCL))+geom_point()+geom_smooth(se=F)+facet_wrap(~LFA)



re = aggregate(TRIPNO~LFA+syr+cl,data=a,FUN=length)
reb = aggregate(TRIPNO~LFA+syr+cl,data=subset(a,SEX==3),FUN=length)
reb$N = reb$TRIPNO
reb$TRIPNO=NULL
rea = merge(re,reb,all=T)
rea = na.zero(rea)
rea$pB = rea$N/rea$TRIPNO

rea$cl = floor(rea$cl/3)*3+1
rea = aggregate(cbind(TRIPNO,N)~LFA+syr+cl,data=rea,FUN=sum)
rea$pB = rea$N/rea$TRIPNO




##bino
xG = subset(a,LFA %in% c('31A','31B'))
xG$Berried = ifelse(xG$SEX==3,1,0)
df=data.frame(CARLENGTH=40:110)
xM = glm(Berried~CARLENGTH,data=subset(xG,syr==2015,CARLENGTH<110),family=binomial(link = 'logit'))

##bino2
require(mgcv)
oxMA = gam(cbind(N,TRIPNO-N)~s(cl),data=subset(rea,LFA==27 & syr==1990),family=binomial(link = 'logit'))
plot(subset(rea,LFA==27 & syr==1990)$cl,predict(oxMA,type='response'))


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
                title(paste('N=',sum(x$N),'SS=',sum(x$TRIPNO)))
                sm = min(x$cl[x$pB>0])
                                
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

                  question1 <- readline("Would you like to define max? (Y/N)")
                  if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
                    o = identify(x$cl,x$pB,n=1)
                    browser()
                    maxs[[4]] = c(o,x$cl[o],'user_id')

                  }
                  
                for(n in 1:length(maxs)){
                  m=m+1
                  xi = subset(x,cl<=as.numeric(maxs[[n]][2]))
                  di= max(xi$pB)
                  if(m==20) browser()
                i1=try(nls(m3,data=xi,start=list(a=.2,b=(as.numeric(maxs[[n]][2])-10),d=di),lower=list(a=-10,b=30,d=di),upper=list(a=10,b=130,d=di),weights=TRIPNO,algorithm = 'port'),silent=T)  #weighted by sample size
                if(class(i1)=='try-error') next
                      rms = rmse(xi$pB,predict(i1))
                      ma = mae(xi$pB,predict(i1))
                      li[[m]] = c(lf[k],Y[i],coef(i1)[1],coef(i1)[2],di,sm,rms,ma,sum(xi$TRIPNO),sum(xi$N),maxs[[n]][3])
                }
    #windows()
#plot(x$cl,x$pB,main=Y[i])
#cc1 = function(x,a=coef(i1)[1],b=coef(i1)[2]) 1/(1+(exp(-a *(x-b))))
#curve(cc1,from=50, to=140,add=T,col='red')
  }
      }
}
som = as.data.frame(do.call(rbind,li))
names(som) = c('LFA','SYR','Slope','Som50','max','minBerr','rmse','mae','ntot','nberr','smooth')
som = toNums(som,2:10)

so = split(som,f=list(som$LFA,som$SYR))

so = bio.utilities::rm.from.list(so)
oi = list()
for(i in 1:length(so)){
    j = so[[i]]
    oi[[i]] = subset(j,rmse==min(rmse))[1,]
  
}

somR = as.data.frame(do.call(rbind,oi))

write.csv(som,file.path(project.datadirectory('bio.lobster'),'data','SoMFromBerried_Mar2024.csv'))

require(ggplot2)
ggplot(somR,aes(x=SYR,y=minBerr)) +geom_point() + geom_line()+facet_wrap(~LFA,scales='free_y')+ylim(c(40,100))

