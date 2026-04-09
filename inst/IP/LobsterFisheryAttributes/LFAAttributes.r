#Lobster Map With Attributes

require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(devtools)
p = bio.lobster::load.environment()
la()

## Fishery Footprint - Landings
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),Z = c('CPUE and CCIR','CPUE','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR',"Trawl Survey Indices and Relative F","Trawl Survey Indices","Trawl Survey Indices",'Trawl Survey Indices'))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  
  
  #Assessment Methods
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c('CPUE and CCIR','CPUE','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR','CPUE and CCIR',"Trawl Survey Indices and Relative F","Standardized CPUE","Standardized CPUE",'Standardized CPUE'))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)

  
  #Seasons
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c(rep('Spring',7),rep('Fall-Winter',5)))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Season Timing',line = 1.5)
  
  #Licences
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c('500-600','10-20','50-70','10-20','50-70','50-70','130-180','600-700','900-1000','90-130','130-180','130-180'))
  xx = lobLFAAttr(Data=x,lv=c('10-20','50-70','90-130','130-180','500-600','600-700','900-1000'))  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Number of Licences (A)',line = 1.5)
  
  #Traplimits
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c(275,rep(250,7),375,300,300,375))
  xx = lobLFAAttr(Data=x,lv=c(250,275,300,375))  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Number of Traps Per Licence (A)',line = 1.5)
  
  #Season Length
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c(rep(60,7),rep(183,2),230,155,230))
  xx = lobLFAAttr(Data=x,)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Season Length',line = 1.5)
  
  #MLS
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c(82.5,rep(84,2),rep(82.5,9)))
  xx = lobLFAAttr(Data=x,)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Minimum Legal Size',line = 1.5)
  
  #Biological Protections
  x = data.frame(LFA=c(28,29,30,'31A',"31B",32),
                 Z = c(rep('MLS 84',2),'Max Size','Window','V-notch','V-notch'))
  xx = lobLFAAttr(Data=x,)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Additional Biological Controls',line = 1.5)
  
  #Window
  x = data.frame(LFA=c('31A'),
                 Z = c('Window'))
  xx = lobLFAAttr(Data=x,)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Window',line = 1.5)

  #Max
  x = data.frame(LFA=c('30'),
                 Z = c('Maximum Size'))
  xx = lobLFAAttr(Data=x,)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Max Size',line = 1.5)
  
  
  #Vnotch
  x = data.frame(LFA=c("31B",32),
                 Z = c('V-notch','V-notch'))
  xx = lobLFAAttr(Data=x,)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('V-notch',line = 1.5)
  
  
  #Fisheries Independent Data
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c('None','None','None','None','None','None','None','None',"4 Trawl Surveys","2 Trawl Surveys","2 Trawl Surveys",'2 Trawl Surveys'))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  
  #At Sea Sampling
  x = data.frame(LFA=c(27,28,29,30,'31A',"31B",32,33,34,35,36,38),
                 Z = c('Annual','Historic','Annual-NA','Historic','Annual-NA','Annual-NA','Annual','Annual',"Annual","Annual",'Historic','Historic'))
  xx = lobLFAAttr(Data=x)  
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  
  LobsterMap('27-38',polylstend=T,labcex=.8)
  
  
  #Year May Indicator
  #rate of chance since indicator
  ###CPUE 
  
  a = lobster.db('process.logs')
  a = subset(a,SYEAR %in% 2005:2025)
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
      sl=as.numeric(round(((k[length(k)]-k[1])/k[1]*100),2))
    }
    o[[i]] = c(LFA=unique(t$LFA),mx.yr=y,slope=sl)  
    
  }
  o = as.data.frame(dplyr::bind_rows(o))
  
  x=o
  x$Z = x$mx.yr
  xx = lobLFAAttr(Data=x[,c('LFA','Z')])  
#  x$LFA[which(x$LFA=='31A')] <- 311
#  x$LFA[which(x$LFA=='31B')] <- 312
  LobsterMap('27-38',poly.lst=xx,polylstend=T,labcex=.8)
  legend('bottomright',legend=c(xx$lvls),fill=xx$col,bty='n',cex=.8)
  title('Year of Max CPUE',line=1.5)
  
  
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
ggplot(subset(g,YR %in% 1990:2025),aes(x=YR,y=t))+geom_line(size=2)+labs(
x="Year", y = "Landings (t)")

a=  lobster.db('annual.landings')
a = subset(a,YR<2026)
a = a[,c(-8,-15)]
a = subset(a,YR>1980)
a = a[order(a$YR),]
a = a %>% tidyr::pivot_longer(cols=starts_with('LFA'),values_to = 'Landings')

require(ggplot2)
ggplot(a, aes(x = YR, y = Landings, fill = name)) +
  geom_bar(stat = "identity")