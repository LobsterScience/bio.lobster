##run 1.LogbookData.r



##by licence

aL = a
#aL = subset(aL,GRID_NUM>0 & !is.na(temp))

##LFA 38
a8 = subset(aL, LFA==38)
m = which(is.na(a8$GRID_NUM))
a8$GRID_NUM[m] = -99
a8 = merge(a8,v,by.x=c('LFA','COMMUNITY_CODE'),by.y=c('LFA','PORT_CODE'))
os = subset(o,LFA==38)
id = unique(a8$LICENCE_ID)
sa8 = split(a8,f=a8$LICENCE_ID)
out = list()
y=0
for(i in 1:length(sa8)){
  j = sa8[[i]]
  j = j[order(j$DATE_FISHED),]
  l = unique(j$SYEAR)
  outa= list()
  y=y+1
  for(oo in 1:length(l)){
    vv = subset(j,SYEAR==l[oo])
    va = tryCatch( aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DOS+WOS+SYEAR+COMMUNITY_CODE+GRID_NUM+SUBMITTER_NAME+VESSEL_NAME+DATE_FISHED+VR_NUMBER+LICENCE_ID,data=vv,FUN=sum),error=function(e) e)
    if(inherits(va,'error')){
      va = aggregate(LFA~DOS+WOS+SYEAR+COMMUNITY_CODE+GRID_NUM+SUBMITTER_NAME+VESSEL_NAME+DATE_FISHED+VR_NUMBER+LICENCE_ID,data=vv,FUN=length)
      va$LFA = NULL
      va$WEIGHT_KG = va$NUM_OF_TRAPS=NA
    }
    outa[[oo]] = va
  }
  out[[y]] = do.call(rbind,outa)
}


#compiling by licence
grList = c(38:42,48:53,61:66,74:78,86:89,97:99,107,108)
oul = list()
for(i in 1:length(out)){
  x = out[[i]]
  x$Trips = 1
  xtr = aggregate(Trips~SYEAR+GRID_NUM+VR_NUMBER,data=x,FUN=sum)
  xGr = aggregate(GRID_NUM~SYEAR,data=subset(xtr,GRID_NUM %in% grList),FUN=function(x) length(unique(x)))
  xV = length(unique(x$VR_NUMBER))
  xS = aggregate(Trips~SYEAR+VR_NUMBER+SUBMITTER_NAME, data=x,FUN= sum)
  xL =  subset(o,Licence_Id==unique(x$LICENCE_ID))
  if(nrow(xL)>0) {if(length(grep(xL$Name_Last_First, o$Name_Last_First))>1)  browser()} #if name has 2 lics
  x1 = merge(x,te1z, by.x=c('GRID_NUM','DATE_FISHED'),by.y=c('grid','date'))
}



#Logbook Processing
a$DYR = lubridate::decimal_date(a$DATE_FISHED) - lubridate::year(a$DATE_FISHED)
a$WYR = ceiling(a$DYR*52)
a$DWYR = lubridate::year(a$DATE_FISHED) + a$WYR/52
a$P=1

xa = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+VR_NUMBER+LICENCE_ID+LFA+SD_LOG_ID,data=a,FUN=sum)
xa$P = 1
x = aggregate(cbind(P,WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+VR_NUMBER+LICENCE_ID+LFA,data=xa,FUN=sum)
x$CPUE = x$WEIGHT_KG/x$NUM_OF_TRAPS


#CPUE and vessel merge

xv = merge(x,d, by.x=c('VR_NUMBER','SYEAR','LFA'),by.y=c('VR_NUMBER','YR_FISHED','LFA'),all.x=T)

##CPUE and vessel and operator merge

xvo = merge(xv, o, by.x=c('LICENCE_ID','LFA'),by.y=c('Licence_Id','LFA'),all.x=T)




##aggregate catches and landing
aa = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+DATE_FISHED+LFA+GRID_NUM+LICENCE,data=a,FUN=sum)
ab = aggregate(VR_NUMBER~SYEAR+DATE_FISHED+LFA+GRID_NUM,data=a,FUN=function(x) length(unique(x)))
aa = list(aa,ab) %>% purrr::reduce(full_join)
aT = merge(aa,te1z,by.x=c('LFA','GRID_NUM','DATE_FISHED'),by.y=c('LFA','grid','date'),all.x=T)
aT = subset(aT,GRID_NUM>0 & !is.na(temp))


