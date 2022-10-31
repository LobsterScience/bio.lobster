require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)

x = lobster.db('process.logs')

sx = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+VR_NUMBER+LFA+COMMUNITY_CODE,data=x,FUN=sum)
lx = aggregate(SD_LOG_ID~SYEAR+VR_NUMBER+LFA+COMMUNITY_CODE,data=x,FUN=function(x) length(unique(x)))

mx=aggregate(NUM_OF_TRAPS~VR_NUMBER,data=x,FUN=max)
mx = subset(mx,NUM_OF_TRAPS<150)

sl = merge(sx,lx)

sl = subset(sl,VR_NUMBER %ni% mx$VR_NUMBER)

#define quantiles
xx=aggregate(SD_LOG_ID~LFA,data=subset(sl,SYEAR>2010),FUN=function(x) quantile(x,c(0.95,0.05)))
xx = data.frame(LFA=xx$LFA,Lower=xx$SD_LOG_ID[,2],Upper=xx$SD_LOG_ID[,1])

slx = merge(sl,xx,all.x=T)

#pruning
sls = subset(slx,SD_LOG_ID>Lower & SD_LOG_ID<Upper)

sls$CPUE = sls$WEIGHT_KG / sls$NUM_OF_TRAPS
sls$ID = paste(sls$LFA,sls$SYEAR,sep="-")
sls = subset(sls,SYEAR>2002)

o = data.frame(LFA=NA,SYEAR=NA,gini=NA)

k = unique(sls$ID)
for(i in 1:length(k)){
	if(nrow(subset(sls,ID==k[i]))>5){
	o[i,'LFA'] = unique(subset(sls,ID==k[i],select=LFA))
	o[i,'SYEAR'] = unique(subset(sls,ID==k[i],select=SYEAR))
	o[i,'gini'] = with(subset(sls,ID==k[i]),giniFootprint(CPUE))
}
}

o = na.omit(o)
g = lobster.db('annual.landings')
g = g[,c(1,2,4:7,9)]
k = lobster.db('seasonal.landings')
k$SYEAR=1976:2023

gg = reshape(g,direction='long',idvar='YR',varying=list(2:7),v.names='Landings')
gg$LFA = rep(names(g)[2:ncol(g)],each=nrow(g))
gg$LFA = substring(gg$LFA,4,8)

k = lobster.db('seasonal.landings')
k$SYEAR=1976:2023
kk = reshape(k[,1:(ncol(k)-1)],direction='long',idvar='SYEAR',varying=list(2:6),v.names='Landings')
kk$LFA = rep(names(k)[2:(ncol(k)-1)],each=nrow(k))
kk$LFA = substring(kk$LFA,4,8)

names(gg)[1] = 'SYEAR'

ou = rbind(gg,kk)
ou = subset(ou,SYEAR>2000 & SYEAR<2022, select=c(SYEAR, Landings,LFA))

ooo=merge(o,ou)



ggplot(subset(ooo,LFA==33),aes(x=SYEAR,y=gini)) + geom_point()+facet_wrap(~LFA)+geom_smooth(se=F)
ggplot(subset(ooo,LFA==33),aes(x=Landings,y=gini)) + geom_point()+facet_wrap(~LFA,scales='free')+geom_smooth(se=F)


ggplot(subset(ooo,LFA==30),aes(x=SYEAR,y=gini)) + geom_point()+facet_wrap(~LFA)+geom_smooth(se=F)
ggplot(subset(ooo,LFA==30),aes(x=Landings,y=gini)) + geom_point()+facet_wrap(~LFA,scales='free')+geom_smooth(se=F)



load_all('C:/Users/Cooka/Documents/git/bio.growth/')

x = subset(ooo,LFA==30)

meanBreakPointRegression(x=x$Landings,y=x$gini,weighted=F,sd.x=NULL,sd.y=NULL)
 meanSegmented(x$Landings,x$gini,weighted=F,breaks=500,title="",xl='Landings',yl='Gini')

x = subset(ooo,LFA==33)

meanBreakPointRegression(x=x$Landings,y=x$gini,weighted=F,sd.x=NULL,sd.y=NULL)
 meanSegmented(x$Landings,x$gini,weighted=F,breaks=8000,title="",xl='Landings',yl='Gini',add_conf = F)
