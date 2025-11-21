##QAT LFA 27

t = lobster.db('annual.landings')
t = subset(t,YR %in% 2021:2024,select=c(YR,LFA27) )
t2l = 2204.62
t$LandingsLBS =(t$LFA27)*t2l #mean lbs per year including harvesters submitting to sGSL


sl = lobster.db('slips')
sl$YR = lubridate::year(sl$DATE_LANDED)
sl = subset(sl,LFA==27 & SPECIES_CODE==700 & YR %in% 2021:2024)

#how many entries
sl$I=1
sll = aggregate(I~YR,data=sl,FUN=sum)
sum(sll$I)/4

slw = aggregate(SLIP_WEIGHT_LBS~YR,data=sl,FUN=sum)
sum(slw$SLIP_WEIGHT_LBS)/4 # cell c9



#reported weight to actual weight using at Sea sample
lobster.db('atSea')
a = subset(atSea,LFA==27 )
a$yr = lubridate::year(a$STARTDATE)
a = subset(a,yr %in% 2021:2024)

#ntraps sampled per trip
a$trid = paste(a$TRIPNO,a$TRAPNO,a$STRINGNO,a$LATITUDE,sep="_")
gg = aggregate(trid~TRIPNO+yr,data=a, FUN=function(x) length(unique(x)))
a$legLob = ifelse(a$SPECIESCODE==2550 & a$SEX<3 & a$CARLENGTH>81,a$CALWT*0.00220462,0)
a$legLob = ifelse(a$CARLENGT==82,a$legLob/2,a$legLob)
ag = aggregate(legLob~trid+yr+TRIPNO+STARTDATE+LICENCE_ID, data=a,FUN=sum)
aga = aggregate(legLob~yr+TRIPNO+STARTDATE+LICENCE_ID, data=ag,FUN=function(x) c(quantile(x,c(0.25,.5,0.75)),mean(x)))

lobster.db('logs')
logs$DATE = as.Date(format(logs$DATE_FISHED, '%Y-%m-%d'))
sa = merge(logs,aga,by.x=c('VR_NUMBER','DATE'),by.y=c('LICENCE_ID','STARTDATE'))
saa = merge(logs,aga,by.x=c('LICENCE_ID','DATE'),by.y=c('LICENCE_ID','STARTDATE'))

sa$WT = apply(sa[,c('WEIGHT_LBS','WEIGHT_LBS_B','WEIGHT_LBS_C')],1,sum,na.rm=T)
sa$Ntr = apply(sa[,c('NUM_OF_TRAPS','NUM_OF_TRAPS_B','NUM_OF_TRAPS_C')],1,sum,na.rm=T)

sa$atSeaMwt = sa$legLob[,4]* sa$Ntr
sa$atSealwt = sa$legLob[,1]* sa$Ntr
sa$atSeauwt = sa$legLob[,3]* sa$Ntr

sa = subset(sa,atSeaMwt>0)

require(dplyr)
require(broom)
model_stats <- sa %>%
  do({
    fit <- lm(WT ~ atSeaMwt, data = .)
    tidy_fit <- tidy(fit)
    glance_fit <- glance(fit)
    data.frame(
      slope = tidy_fit$estimate[2],
      r2 = glance_fit$r.squared,
      x = max(.$atSeaMwt) - 2,  # position for label
      y = min(.$WT)+50       # position for label
    )
  })


ggplot(sa,aes(x=atSeaMwt,y=WT))+
  geom_point()+
  geom_errorbarh(aes(xmin=atSealwt,xmax=atSeauwt),colour='grey')+
  geom_smooth(method='lm')+
geom_text(data = model_stats,
            aes(x = x, y = y,
                label = paste0("Slope = ", round(slope, 2), "\nR² = ", round(r2, 2))),
            inherit.aes = FALSE) +
  labs(x='Landed Wt At Sea Samples',y='Reported Weight Logs')+
  theme_test(base_size = 14)



GC = split(sa,f=list(sa$yr))
og = list()
for(i in 1:length(GC)){
  O = GC[[i]]        
  L = with(O,lm(WT~atSeaMwt-1)) #treating the SWLSS data as 'truth'
  RL = with(O,MASS::rlm((WT~atSeaMwt-1))) #treating the SWLSS data as 'truth'
  biasS = SimDesign::bias(O$WT,O$atSeaMwt,type='relative')
  og[[i]] = c(unique(O$LFA),unique(O$yr),nrow(O),biasS,coef(L),summary(L)$sigma,coef(RL),summary(RL)$sigma)   
  
}
og = as.data.frame(do.call(rbind,og))
names(og) = c('LFA', 'YR','SampleSize','Bias','slopeOflm','sigmaOflm','slopeOfrlm','sigmaOfrlm')
#negative bias at sea samples more weight than reported in logs for same trip
#do the weights fall within bounds?

sa$InError = ifelse(sa$WT>=sa$atSealwt & sa$WT<=sa$atSeauwt,1,0)


sa = merge(sl,aga,by.x=c('VR_NUMBER','DATE_LANDED'),by.y=c('LICENCE_ID','STARTDATE'))



#mergeing slips and logs

b = lobster.db('process.logs.unfiltered')
b = subset(b,LFA==27 & SYEAR %in% 2021:2024)
ba = aggregate(WEIGHT_LBS~LICENCE_ID+SYEAR,data=b,FUN=sum)

sla = aggregate(SLIP_WEIGHT_LBS~YR+LICENCE_ID,data=sl,FUN=sum)

slaba = merge(sla,ba,by.x=c('LICENCE_ID','YR'), by.y=c('LICENCE_ID','SYEAR'))



require(dplyr)
require(broom)
require(MASS)
model_stats_sl_lo <- slaba %>%
  do({
    fit <- rlm(SLIP_WEIGHT_LBS ~ WEIGHT_LBS, data = .)
    tidy_fit <- tidy(fit)
    glance_fit <- glance(fit)
    data.frame(
      slope = tidy_fit$estimate[2],
      r2 = glance_fit$r.squared,
      x = max(.$WEIGHT_LBS) - 10000,  # position for label
      y = min(.$SLIP_WEIGHT_LBS)+50       # position for label
    )
  })


ggplot(slaba,aes(x=WEIGHT_LBS,y=SLIP_WEIGHT_LBS))+
  geom_point()+
  geom_smooth(method='rlm')+
  geom_text(data = model_stats_sl_lo,
            aes(x = x, y = y,
                label = paste0("Slope = ", round(slope, 2), "\nR² = ", round(r2, 2))),
            inherit.aes = FALSE) +
  labs(x='Reported Wt Logs',y='Reported Weight Slips')+
  theme_test(base_size = 14)



GC = split(sa,f=list(sa$yr))
og = list()
for(i in 1:length(GC)){
  O = GC[[i]]        
  L = with(O,lm(WT~atSeaMwt-1)) #treating the SWLSS data as 'truth'
  RL = with(O,MASS::rlm((WT~atSeaMwt-1))) #treating the SWLSS data as 'truth'
  biasS = SimDesign::bias(O$WT,O$atSeaMwt,type='relative')
  og[[i]] = c(unique(O$LFA),unique(O$yr),nrow(O),biasS,coef(L),summary(L)$sigma,coef(RL),summary(RL)$sigma)   
  
}
og = as.data.frame(do.call(rbind,og))
names(og) = c('LFA', 'YR','SampleSize','Bias','slopeOflm','sigmaOflm','slopeOfrlm','sigmaOfrlm')
#negative bias at sea samples more weight than reported in logs for same trip
#do the weights fall within bounds?


