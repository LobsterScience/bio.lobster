require(tidyr)
require(dplyr)
require(bio.lobster)
require(devtools)
require(ggplot2)
library(viridis)
la()
a = lobster.db('annual.landings')
a = subset(a,!is.na(YR)& YR>1975 & YR<2026)
sa = a %>% gather(key='LFA',value='Landings',-YR)
sa = subset(sa,LFA<'LFA33')
sa = subset(sa,LFA %ni% 'LFA31')
sa = sa[order(sa$LFA,sa$YR),]
gg = ggplot(sa,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity')+
  facet_wrap(~LFA, scales='free_y' )+xlab('Season')+ylab('Landings (kt)')


a = lobster.db('seasonal.landings')
a$SYEAR = as.numeric(substring(a$SYEAR,6,9))
a = subset(a,!is.na(SYEAR)& SYEAR>1975& SYEAR<2026)
sa1 = a %>% pivot_longer(cols=starts_with('LFA'),names_to="LFA",values_to='Landings')
names(sa1)[1] = "YR"

o = bind_rows(sa,sa1)
o1 = subset(o,LFA %ni% 'LFA38B')

mo1 = aggregate(Landings~LFA,data=subset(o1,YR %in% 1975:2024),FUN=function(x) c(median(x),quantile(x,c(0.25,0.75))))
o1$Lkt = o1$Landings/1000

o1 <- o1 %>%
  group_by(LFA) %>%
  mutate(is_max = Lkt == max(Lkt,na.rm=T))
ggplot(o1,aes(x=YR,y=Lkt,fill=is_max))+geom_bar(stat='identity',width=1)+
  facet_wrap(~LFA, scales='free_y' ,nrow=2)+xlab('Fishing Year')+ylab('Landings (kt)')+
  scale_x_continuous(breaks=round(seq(1975,2024,length=4)))+theme_test()+ theme(legend.position = 'none')+
  scale_fill_manual(values = c("FALSE" = "grey10", "TRUE" = "grey10")) +
  geom_hline(data=mo1,aes(yintercept=Landings[,1]/1000),linetype='solid',colour='red')

o1$LFA = gsub('LFA',"",o1$LFA)

v = lobster.db('landings_by_vessel')
d = aggregate(MT~SYEAR+LFA,data=v,FUN=function(x) c(mean(x),quantile(x,c(0.25,.5,.75))))
d$IQR = d$MT[,4]-d$MT[,2]
d$MT = d$MT[,1]
d$YR= d$SYEAR
od = merge(o1,d)
od =subset(od,LFA %in% 34:38)
lo = split(od,f=od$LFA)
out = list()
for(i in 1:length(lo)){
  k = lo[[i]]
  v = which.max(k$Landings)
  w =   which.max(k$MT)
  
  l1 = lm(log(Landings)~YR,data=k[v:nrow(k),])
  l2 = lm(log(MT)~YR,data=k[w:nrow(k),])
  out[[i]]=c(lfa = unique(k$LFA),LandRate =coef(l1)[2],VesRate = coef(l2)[2])
  
}
ou = bind_rows(out)

ggplot(subset(d,LFA=='38B'),aes(x=SYEAR,y=MT))+geom_point()+theme_test(base_size = 14)+labs(y='Landings Per Vessel (T)',x='Year')
       