lobster.db('seasonal.landings.redo')
lobster.db('seasonal.landings.redo')
lobster.db('annual.landings.redo')
lobster.db('seasonal.landings')
g = lobster.db('seasonal.landings')
g$yr = substr(g$SYEAR,6,9)
g = subset(g,yr>2017 & yr<2024, select=c(yr,LFA33,LFA34))
g$sums = apply(g[,2:3],1,sum)


a = lobster.db('annual.landings')
a = subset(a,!is.na(YR)& YR>1995)
sa = a %>% gather(key='LFA',value='Landings',-YR)
sa = subset(sa,LFA<'LFA33')
sa = subset(sa,LFA %ni% 'LFA31')
sa = sa[order(sa$LFA,sa$YR),]

a = lobster.db('seasonal.landings')
a$SYEAR = as.numeric(substring(a$SYEAR,6,9))
a = subset(a,!is.na(SYEAR)& SYEAR>1995)
sa1 = a %>% pivot_longer(cols=starts_with('LFA'),names_to="LFA",values_to='Landings')

sl = lobster.db('slips')
inf = lobster.db('inflation')
i = which(sl$PRICE<2)
j = which(sl$PRICE>30)
i = c(i,j)
sl$PRICE[i] = NA
sl = subset(sl, SPECIES_CODE==700 & NIL_REPORT_FLAG=='N') #nil reports still have to be submitted; ~37% missing price data

sl$DYR = lubridate::decimal_date(as.Date(sl$DATE_LANDED)) - lubridate::year(as.Date(sl$DATE_LANDED))
sl$WYR = ceiling(sl$DYR*52)
sl$DWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + sl$WYR/52
sl$MWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + ceiling(sl$DYR*12)/12
sl$YR = lubridate::year(as.Date(sl$DATE_LANDED))

price.data = aggregate(PRICE~DWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T))) #price per week of year 
price.data2 = aggregate(PRICE~MWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
price.data3 = aggregate(PRICE~YR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))

sll = bio.utilities::fillNaDf2(sl, price.data, mergeCols=c('DWYR','LFA'),fillCols=c('PRICE'))  #reduces to .03%missing
slll = bio.utilities::fillNaDf2(sll, price.data2, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))#reduces to .01%missing
sllll = bio.utilities::fillNaDf2(slll, price.data3, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))#reduces to .005%missing
s = sllll

s$mn = lubridate::month(s$DATE_LANDED)
i= which(s$LFA %in% c('33','34','35','36', '38')) 
j = which(s$mn %in% c(10,11,12))
k = intersect(i,j)
s$SYEAR = s$YR
s$SYEAR[k] = s$SYEAR[k]+1
s$Value = s$PRICE * s$SLIP_WEIGHT_LBS

##supply demand

sd = aggregate(cbind(SLIP_WEIGHT_LBS,Value)~DWYR+YR,data=s,FUN=sum)

ggplot(subset(sd,YR>2010 & YR<2024),aes(x=(SLIP_WEIGHT_LBS),y=Value/SLIP_WEIGHT_LBS,colour=as.factor(YR)))+geom_point()+geom_smooth(method='lm',se=F)


ss = aggregate(cbind(Value,SLIP_WEIGHT_LBS)~LFA+SYEAR,data=s,FUN=sum)
ss$Value = ss$Value/1000
ss$LFA = paste('LFA',ss$LFA,sep="")
ss1 = merge(sa,ss,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

ss2 = merge(sa1,ss,by.x=c('SYEAR','LFA'),by.y=c('SYEAR','LFA'))
ss1 = ss1[order(ss1$LFA,ss1$YR),]

ss2 = ss2[order(ss2$LFA,ss2$SYEAR),]

gg = ggplot(ss1,aes(x=YR,y=Value,fill=factor(YR)))+geom_bar(stat='identity')+
  facet_wrap(~LFA, scales='free_y' )+xlab('Season')+ylab("Value ('000s)")
last_bar_color <- "black"  # Change this to the color you want for the last bar
bar_colors1 <- ifelse(ss1$YR <max(ss1$YR), last_bar_color, "orange")

gg2= ggplot(subset(ss2,LFA=='LFA34'),aes(x=SYEAR,y=Value,fill=factor(SYEAR)))+geom_bar(stat='identity')+
  facet_wrap(~LFA, scales='free_y' )+xlab('Season')+ylab("Value ('000s)")
last_bar_color <- "black"  # Change this to the color you want for the last bar
bar_colors2 <- ifelse(ss2$SYEAR <max(ss2$SYEAR), last_bar_color, "orange")


#prices for LFA 41

lobster.db('logs41')
sl=logs41
sl$DYR = lubridate::decimal_date(as.Date(sl$FV_FISHED_DATETIME)) - lubridate::year(as.Date(sl$FV_FISHED_DATETIME))
sl$WYR = ceiling(sl$DYR*52)
sl$DWYR = lubridate::year(as.Date(sl$FV_FISHED_DATETIME)) + sl$WYR/52
sl$MWYR = lubridate::year(as.Date(sl$FV_FISHED_DATETIME)) + ceiling(sl$DYR*12)/12
sl$YR = lubridate::year(as.Date(sl$FV_FISHED_DATETIME))
sl$PRICE=NA
price.data = subset(price.data,LFA==33)
price.data2 =subset(price.data2,LFA==33)
price.data3 = subset(price.data3,LFA==33)
sll = bio.utilities::fillNaDf2(sl, price.data, mergeCols=c('DWYR'),fillCols=c('PRICE'))
slll = bio.utilities::fillNaDf2(sll, price.data2, mergeCols=c('MWYR'),fillCols=c('PRICE'))
sllll = bio.utilities::fillNaDf2(slll, price.data3, mergeCols=c('YR'),fillCols=c('PRICE'))
s = sllll

s$value = s$ADJCATCH * s$PRICE

ga = aggregate(cbind(value,ADJCATCH)~YR,data=s,FUN=sum)

