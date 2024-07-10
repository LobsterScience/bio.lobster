#comparing logbooks and slips

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(lubridate)
require(ggplot2)
la()

a = lobster.db('process.logs.unfiltered')
b = lobster.db('slips')
sl = subset(b, SPECIES_CODE==700 & NIL_REPORT_FLAG=='N') #nil reports still have to be submitted; ~37% missing price data
sl$yr = year(sl$DATE_LANDED)
sl$mn = month(sl$DATE_LANDED)
sl$SYEAR = sl$yr
sl$SYEAR = ifelse(sl$LFA <33 & sl$mn %in% c(1:3),sl$SYEAR-1,sl$SYEAR )

sl$SYEAR = ifelse(sl$LFA %in% 33:34 & sl$mn %in% c(11:12),sl$SYEAR+1,sl$SYEAR )
sl$SYEAR = ifelse(sl$LFA %in% 35 & sl$mn %in% c(10:12),sl$SYEAR+1,sl$SYEAR )
sl$SYEAR = ifelse(sl$LFA %in% 36:38 & sl$mn %in% c(11:12),sl$SYEAR+1,sl$SYEAR )


sA = aggregate(SLIP_WEIGHT_LBS~LICENCE_ID+LFA+SYEAR,data=sl,FUN=sum)
lA = aggregate(WEIGHT_LBS~LICENCE_ID+LFA+SYEAR,data=a,FUN=sum)

ls = merge(lA,sA)

ls$diff = ls$WEIGHT_LBS-ls$SLIP_WEIGHT_LBS
ls$percDiff = bio.utilities::percentDifference(ls[,c('WEIGHT_LBS','SLIP_WEIGHT_LBS')])

ls = subset(ls,SYEAR<2024)

ggplot(subset(ls,SYEAR %in% seq(2008,2023,3)),aes(x=as.factor(SYEAR),y=diff))+geom_boxplot()+facet_wrap(~LFA)
ggplot(subset(ls,SYEAR %in% seq(2008,2023,3)),aes(x=as.factor(SYEAR),y=percChange))+geom_violin()+facet_wrap(~LFA)


ga = aggregate(cbind(percDiff,WEIGHT_LBS,SLIP_WEIGHT_LBS)~SYEAR+LFA,data=ls,FUN=median)

ggplot(ga,aes(x=SYEAR,y=percDiff))+geom_point()+geom_smooth(method='lm') + facet_wrap(~LFA)
