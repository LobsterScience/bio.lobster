#slips Data
require(bio.lobster)
require(bio.utilities)
require(ggplot2)

sl = lobster.db('slips')
inf = lobster.db('inflation')
i = which(sl$PRICE<2)
j = which(sl$PRICE>30)
i = c(i,j)
sl$PRICE[i] = NA
sl = subset(sl, SPECIES_CODE==700 & NIL_REPORT_FLAG=='N') #nil reports still have to be submitted

sl$DYR = lubridate::decimal_date(as.Date(sl$DATE_LANDED)) - lubridate::year(as.Date(sl$DATE_LANDED))
sl$WYR = ceiling(sl$DYR*52)
sl$DWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + sl$WYR/52
sl$MWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + ceiling(sl$DYR*12)/12
sl$YR = lubridate::year(as.Date(sl$DATE_LANDED))

price.data = aggregate(PRICE~DWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
price.data2 = aggregate(PRICE~MWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
price.data3 = aggregate(PRICE~YR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))

sll = bio.utilities::fillNaDf2(sl, price.data, mergeCols=c('DWYR','LFA'),fillCols=c('PRICE'))
slll = bio.utilities::fillNaDf2(sll, price.data2, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))
sllll = bio.utilities::fillNaDf2(slll, price.data3, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))
s = sllll
y = 2004

inf = subset(inf, year>=y)
inf$amount = c(1,rep(NA, nrow(inf)-1))
for(i in 2:(nrow(inf))){inf$amount[i]=inf$amount[i-1] + inf$amount[i-1]*inf$inflation.rate[i]}

si = merge(s,inf,by.x='YR',by.y='year')
si$PriceCorr = si$PRICE/si$amount

si$Value = si$SLIP_WEIGHT_LBS*si$PRICE
si$ValueCorr = si$SLIP_WEIGHT_LBS*si$PriceCorr
si$mn = month(si$DATE_LANDED)

si$SYEAR = si$YR
i = which(si$LFA %in% c('33','34','35','36','38') & si$mn %in% 10,11,12)
si$SYEAR[i] = si$SYEAR[i]+1
ss = aggregate(cbind(Value,ValueCorr,SLIP_WEIGHT_LBS)~SYEAR+LFA+LICENCE_ID,data=si,FUN=sum)
ssp = aggregate(PriceCorr~SYEAR+LFA+LICENCE_ID,data=si,FUN=mean)
sspp = aggregate(PriceCorr~SYEAR+LFA,data=ssp,FUN=summary)
ggplot(subset(sspp,SYEAR>2006 & SYEAR<2022),aes(x=SYEAR,y=PriceCorr[,4]))+geom_line()+facet_wrap(~LFA)

ssa = aggregate(cbind(Value,ValueCorr,SLIP_WEIGHT_LBS)~SYEAR+LFA,data=ss,FUN=function(x) summary(x))

ggplot(subset(ssa,SYEAR>2006 & SYEAR<2022),aes(x=SYEAR,y=Value[,4]))+geom_line()+facet_wrap(~LFA)

ggplot(subset(ssa,SYEAR>2006 & SYEAR<2022),aes(x=SYEAR,y=Value[,4]))+geom_line()+
  geom_line(data=subset(ssa,SYEAR>2006 & SYEAR<2022), aes(x=SYEAR,y=ValueCorr[,4],colour='red'))+facet_wrap(~LFA)


ggplot(subset(ssa,SYEAR>2006 & SYEAR<2022),aes(x=SYEAR,y=SLIP_WEIGHT_LBS[,4]))+geom_line()+facet_wrap(~LFA)
