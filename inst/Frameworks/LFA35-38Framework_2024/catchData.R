

##### FRAMEWORK LANDINGS #####

setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Data Output")


require(devtools)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(ggplot2)
require(dplyr)

p = bio.lobster::load.environment()
la()
assessment.year = 2024 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year


###################### LOAD DATA ########################

lobster.db('logs.redo') 
lobster.db('logs')

lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

lobster.db('annual.landings.redo')
land =lobster.db("annual.landings")

lobster.db('seasonal.landings.redo')
Sland = lobster.db('seasonal.landings')


a=lobster.db('process.logs')
a = subset(a,LFA==35)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS
#h=lobster.db('seasonal.landings.redo')
h=lobster.db('seasonal.landings')
h
h=h[,c('SYEAR','LFA35')]
h=na.omit(h)
h$SYEAR=1976:2024
merge(b,h)
g=merge(b,h)
g$EFF = g$LFA35/g$CPUE

landP<- ggplot()
landP<- landP + geom_bar(data=h, aes(x=SYEAR, y = LFA35), stat= "identity", fill= "#003f5c")
landP<-landP +geom_point(data=b, aes(x=SYEAR,y=CPUE), color="#f95d6a")




################## ENGLISH PLOT #######################
par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)

plot(h$SYEAR,h$LFA35,xlab='Year',ylab='Landings (t)',type='h',ylim=c(0,max(h$LFA35)*1.2),pch=15,col='#003f5c',lwd=10,lend=3)
lines(h$SYEAR[nrow(h)],h$LFA35[nrow(h)],type='h',pch=21,col='#f95d6a',lwd=10,lend=3)
par(new=T)

plot(b$SYEAR,b$EFF,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(b$EFF,na.rm=T)))
points(b$SYEAR[nrow(b)],b$EFF[nrow(b)], type='b', pch=24,cex = 1.5,bg='#f95d6a')
axis(4)
mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)	

dev.off()		


subsamp<-g[g$SYEAR >2009 & g$SYEAR <2023,]
range(subsamp$LFA38)
median(subsamp$LFA38)



#Color palette
pal=c("#003f5c",
      "#2f4b7c",
      "#665191",
      "#a05195",
      "#d45087",
      "#f95d6a",
      "#ff7c43",
      "#ffa600")