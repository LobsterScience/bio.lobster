#slips Data
require(bio.lobster)
require(bio.utilities)
require(ggplot2)

inf = lobster.db('inflation')
sl = lobster.db('process_slips')
inf = subset(inf, year>=min(lubridate::year(sl$Date)))
inf$amount = c(1,rep(NA, nrow(inf)-1))
for(i in 2:(nrow(inf))){inf$amount[i]=inf$amount[i-1] + inf$amount[i-1]*inf$inflation.rate[i]}
##need to check inflation stuff june 18 2024
si = merge(sl,inf,by.x='YR',by.y='year')
si$PriceCorr = si$PRICE/si$amount

si$ValueCorr = (si$adj_wt_kg*2.2046)*si$PriceCorr

ss = aggregate(cbind(value,ValueCorr,adj_wt_kg)~SYEAR+LFA+LICENCE_NO,data=si,FUN=sum)

ssa = aggregate(cbind(value,ValueCorr,adj_wt_kg)~SYEAR+LFA,data=ss,FUN=function(x) summary(x))

ggplot(subset(ssa, SYEAR<2023 & LFA<40),aes(x=SYEAR,y=value[,4]))+geom_line()+facet_wrap(~LFA)

ggplot(subset(ssa,SYEAR>2006 & SYEAR<2022),aes(x=SYEAR,y=adj_wt_kg[,4]))+geom_line()+facet_wrap(~LFA)

ssaa = aggregate(cbind(value,adj_wt_kg)~SYEAR+LFA,data=ss,FUN=function(x) sum(x))

tidyr::pivot_wider(subset(ssaa,SYEAR>2017 & SYEAR<2023,select=c(LFA,SYEAR,adj_wt_kg)),id_cols=LFA,names_from=SYEAR,values_from=adj_wt_kg)