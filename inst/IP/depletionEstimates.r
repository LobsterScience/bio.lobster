#depletion

require(bio.lobster)
require(devtools)
require(MASS)
la()

g = lobster.db('process.logs.unfiltered')
h = lobster.db('annual.landings')

h=h[order(h$YR),]

t = setupDepletionData(logs=subset(g,LFA==29&SYEAR>2007),landings=h[,c('YR','LFA29')],lfa=29)

#xample
x = subset(t,season==2020)
deluryLeslie(y=x,estimate='delury',method='robust',weight = x$n / max(x$n) )




##SPB
outSPBd = c()
outSPBl = c()

t = setupDepletionData(logs=subset(g,LFA==29&COMMUNITY_CODE %in% c(10925,10928,10810) & SYEAR>2008 & SYEAR <2025),landings=NULL,lfa=29)
yy = unique(t$season)

for(i in 1:length(yy)){
x = subset(t,season==yy[i])
rr = deluryLeslie(y=x,estimate='leslie',method='robust',weight = x$n / max(x$n) )
outSPBl = c(outSPBl,rr[[3]])
rr = deluryLeslie(y=x,estimate='delury',method='robust',weight = x$n / max(x$n) )
outSPBd = c(outSPBd,rr[[3]])

}

#plot(yy,outSPBl,type = 'b' ,pch=16, col='black', main="Leslie")
plot(yy,outSPBd,type = 'b' ,pch=16, col='black', main="Delury Depletion")


##!SPB
outoSPBd = c()
outoSPBl = c()

t = setupDepletionData(logs=subset(g,LFA==29&COMMUNITY_CODE %ni% c(10925,10928,10810) & SYEAR>2008 & SYEAR <2021),landings=NULL,lfa=29)
yy = unique(t$season)

for(i in 1:length(yy)){
x = subset(t,season==yy[i])
rr = deluryLeslie(y=x,estimate='leslie',method='robust',weight = x$n / max(x$n) )
outoSPBl = c(outoSPBl,rr[[3]])
rr = deluryLeslie(y=x,estimate='delury',method='robust',weight = x$n / max(x$n) )
outoSPBd = c(outoSPBd,rr[[3]])

}
par(new=T)
#plot(yy,outoSPBl,type = 'b' ,pch=16, col='red', main="Leslie")
plot(yy,outoSPBd,type = 'b' ,pch=16, col='red')
legend("bottomright", c("SPB", "Not SPB"),col=c("black", "red"), lty=c(1,1), bty="n")
