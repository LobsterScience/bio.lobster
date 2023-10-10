
setwd("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38/LFA35_Update")

require(rgdal)
require(devtools)
require(roxygen2)
require(geosphere)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(rstanarm)
require(rlang)
require(glue)
require(PBSmapping)
require(dplyr)


p = bio.lobster::load.environment()
la()
assessment.year = 2022 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year



figdir = file.path("LFA35-38")

p$lfas = c("35") # specify lfa

#### CHECKING CPUE AND TEMP MODEL ####


###### CURRENT CPUE MODEL ######
logs=lobster.db("process.logs")
TempModelling = TempModel( annual.by.area=F, redo.data=F)
CPUE.data<-CPUEModelData(p,redo=T,TempModelling)

## Commercial CPUE MOdels
mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)

CPUE.data<- CPUEModelData(p,redo=F)
t=with(subset(CPUE.data,DOS==1),tapply(TEMP,LFA,mean))

pData=list()

CPUEModelResults = list()

for(i in 1:length( p$lfas)){
  
  mdata = subset(CPUE.data,LFA==p$lfas[i]&SYEAR%in%p$yrs)
  if(nrow(mdata)>10){
    CPUEModelResults[[i]] = CPUEmodel(mf1,mdata,t=t[i],d=1)
    pData[[i]]=CPUEModelResults[[i]]$pData
    pData[[i]]$LFA=p$lfas[i]
  }
}

names(CPUEModelResults) = p$lfas

CPUEindex=do.call("rbind",pData)


# plot

#tiff("C:/Users/Howsevj/Documents/3638_Update/36plotcpue.tiff", width=7,height=6, units="in",res=300)

l35 = subset(CPUEindex,LFA==35,c("YEAR","mu"))
k = median(l35$mu[which(l35$YEAR %in% 2011:2018)])
usr = .4*k
lrp = .2*k


plot(l35$YEAR,l35$mu,xlab='Year',ylab=' Standardized CPUE (kg/TH)',type='p',pch=16,ylim=c(0,6),xlim=c(2005,2022))
running.median = with(rmed(l35$YEAR,l35$mu),data.frame(YEAR=yr,running.median=x))
l35=merge(l35,running.median,all=T)
lines(l35$YEAR,l35$running.median,col='blue',lty=1,lwd=3)
points(l35$YEAR[nrow(l35)],l35$mu[nrow(l35)], type='b', pch=24,bg='blue')
abline(h=usr,col='green',lwd=2,lty=2)
abline(h=lrp,col='red',lwd=2,lty=3)

dev.off()


## BASIC CPUE


logs=lobster.db("process.logs")

h = subset(logs,LFA==35)
h = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=logs,FUN=sum)
h$CPUE = h$WEIGHT_KG/h$NUM_OF_TRAPS


## Raw CPUE
plot(h$SYEAR, h$CPUE, xlab="Year", ylab="CPUE (kg/TH)", type='p', pch=16)
