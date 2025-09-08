#cc fishing 
require(bio.lobster)
require(devtools)
library(dplyr)
library(lubridate)
require(ggplot2)
la()

v = lobster.db('licence_characteristics')
va = subset(v,LFA==38)
vv = subset(v,LIC_TYPE=='CC VESSEL BASED LIMITED' & LFA ==38)

#syear would be the first year this licence was allocated to CC
vv <- vv %>%
  mutate(
    SYEAR = year(START_DATE) + if_else(month(START_DATE) > 11 | (month(START_DATE) == 11 & day(START_DATE) > 1), 1, 0)
  )


sl = lobster.db('process_slips')

sl = subset(sl,LFA==38 & SYEAR>2021 & SYEAR<2025)
sl = merge(sl,vv[,c('LICENCE_ID','SYEAR')],by.x='LICENCE_NO',by.y='LICENCE_ID',all=T)
sl$CC = ifelse(!is.na(sl$SYEAR.y) & sl$SYEAR.x>=sl$SYEAR.y,1,0 )
sl$SYEAR.y <- NULL
sl$SYEAR = sl$SYEAR.x
sl$SYEAR.x <- NULL

xl = aggregate(adj_wt_kg~LICENCE_NO+SYEAR+CC,data=sl,FUN=sum)
xla = aggregate(adj_wt_kg~SYEAR+CC,data=xl,FUN=function(x) quantile(x, c(0.025,.5,0.975)))
xla$CC = as.factor(xla$CC)

pos = position_dodge(width=.3)
ggplot(xla,aes(x=as.factor(SYEAR),y=adj_wt_kg[,2],ymin=adj_wt_kg[,1],ymax=adj_wt_kg[,3],colour=CC))+
  geom_point(position=pos)+
  geom_errorbar(position=pos, width=0)+
  xlab('Fishing Season')+
  ylab('Landings (kg)')+
  guides(fill=guide_legend(title = 'CC'))


b = lobster.db('process.logs.unfiltered')
b = subset(b,LFA %in% c(38) & SYEAR>2021& SYEAR<2025)
b = merge(b,vv[,c('LICENCE_ID','SYEAR')],by='LICENCE_ID',all=T)
b$CC = ifelse(!is.na(b$SYEAR.y) & b$SYEAR.x>=b$SYEAR.y,1,0 )
b$SYEAR.y <- NULL
b$SYEAR = b$SYEAR.x
b$SYEAR.x <- NULL

bt = aggregate(DATE_FISHED~LICENCE_ID+SYEAR+CC,data=b,FUN=function(x) length(unique(x)))
bta = aggregate(DATE_FISHED~SYEAR+CC,data=bt,FUN=function(x) quantile(x, c(0.025,.5,0.975)))

bl = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LICENCE_ID+SYEAR+CC,data=b,FUN=sum)
bla = merge(bl,xl,by.x=c('LICENCE_ID','SYEAR','CC'),by.y=c('LICENCE_NO','SYEAR','CC'))
bla$corNTRAPS = bla$NUM_OF_TRAPS * (bla$adj_wt_kg/bla$WEIGHT_KG)
bla$diff = (bla$adj_wt_kg - bla$WEIGHT_KG) / bla$WEIGHT_KG
blat = aggregate(corNTRAPS~SYEAR+CC,data=bla,FUN=function(x) quantile(x, c(0.025,.5,0.975)))

blata = aggregate(cbind(corNTRAPS,adj_wt_kg)~SYEAR+CC,data=bla,FUN=function(x) quantile(x, c(.5)))

blata$CPUE = blata$adj_wt_kg/blata$corNTRAPS

xla$CC = as.factor(xla$CC)



