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
sw = aggregate(CPUE~ID,data=sls,FUN= giniFootprint)

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

ggplot(o,aes(x=SYEAR,y=gini)) + geom_line()+facet_wrap(~LFA)+geom_smooth(se=F)
