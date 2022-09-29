#Lobster Map With Attributes

require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(devtools)
p = bio.lobster::load.environment()
la()

## Fishery Footprint - Landings
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),Z = c('CPUE and CCIR','CPUE','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR',"Trawl Survey Indices and Relative F","Standardized CPUE","Standardized CPUE",'Standardized CPUE'))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  
  
  #Assessment Methods
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c('CPUE and CCIR','CPUE','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR',"Trawl Survey Indices and Relative F","Standardized CPUE","Standardized CPUE",'Standardized CPUE'))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  
  
  #Fisheries Independent Data
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c('None','None','None','None','None','None','None','None',"4 Trawl Surveys","2 Trawl Surveys","2 Trawl Surveys",'2 Trawl Surveys'))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  
  #Year May Indicator
  #rate of chance since indicator
  ###CPUE 
  
  a = lobster.db('process.logs')
  a = subset(a,SYEAR %in% 2005:2021)
  aa = split(a,f=list(a$LFA,a$SYEAR))
  cpue.lst<-list()
  m=0
  for(i in 1:length(aa)){
    tmp<-aa[[i]]
    if(nrow(tmp)>5){
      m=m+1
      tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
      names(tmp)<-c('time','catch','effort')
      tmp$date<-as.Date(tmp$time)
      first.day<-min(tmp$date)
      tmp$time<-julian(tmp$date,origin=first.day-1)
      tmp$time = floor(tmp$time/7) *7
      g<-c(biasCorrCPUE(tmp,by.time=F),unique(aa[[i]]$LFA),unique(aa[[i]]$SYEAR))
      cpue.lst[[m]] <- rbind(g)
    }
  }
  
  cc =as.data.frame(do.call(rbind,cpue.lst))
  cc = toNums(cc,c(1:5,7))
  names(cc)[6:7] = c('LFA','YR')
  o=list()
  g = split(cc,f=cc$LFA)
  for(i in 1:length(g)){
    t = g[[i]]
    n = nrow(t)
    l = which.max(t$unBCPUE)
    y = t$YR[l]
    sl=0
    if((n-(l-1))>2){
      tt = t[l:n,]
      tt$unBCPUEp = tt$unBCPUE-max(tt$unBCPUE)
      tt$y = 0:(nrow(tt)-1)
      k = lm(unBCPUEp~y-1,data=tt)
      k = predict(k)
      k = k+max(tt$unBCPUE)
      sl=round(((k[length(k)]-k[1])/k[1]*100),2)
    }
    o[[i]] = c(unique(t$LFA),y,sl)  
    
  }
  
  
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Zp = c(-2.4,-26,-16,-20,-2,-10,0,-16,-20,-10,-17,-10),
                 labs=c(2018,2019,2018,2019,2019,2019,2021,2016,2016,2014,2018,2016))
  
  #1=0-5,2=6-10,3-11-15,4=16-20,5=21-25,6=26-30
  x$Z = c(1,6,4,4,1,2,1,4,4,2,4,2)
  xx = lobLFAAttr(Data=x[,c('LFA','Z')],lv=1:6)  
  x$LFA[which(x$LFA=='31A')] <- 311
  x$LFA[which(x$LFA=='31B')] <- 312
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8,special.labels = x)
  legend('bottomright',legend=c('0-5','6-10','11-15','16-20','21-25','26-30'),fill=xx$col,bty='n',cex=.8,title='% Dec from Max')
  
  
  
  
g=  lobster.db('annual.landings')
g = g[,c(-6,-7)]
g = g[,c(-13)]
g$t = apply(g[,2:ncol(g)],1,sum, na.rm=T)
ggplot(subset(g,YR %in% 1990:2021),aes(x=YR,y=t))+geom_line(size=2)+labs(
x="Year", y = "Landings (t)")
