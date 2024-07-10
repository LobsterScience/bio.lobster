require(tidyr)
require(dplyr)
require(bio.lobster)
require(devtools)
require(ggplot2)
library(viridis)
la()
a = lobster.db('annual.landings')
a = subset(a,!is.na(YR)& YR>1995 & YR<2024)
sa = a %>% gather(key='LFA',value='Landings',-YR)
sa = subset(sa,LFA<'LFA33')
sa = subset(sa,LFA %ni% 'LFA31')
sa = sa[order(sa$LFA,sa$YR),]
gg = ggplot(sa,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity')+
  facet_wrap(~LFA, scales='free_y' )+xlab('Season')+ylab('Landings (kt)')


a = lobster.db('seasonal.landings')
a$SYEAR = as.numeric(substring(a$SYEAR,6,9))
a = subset(a,!is.na(SYEAR)& SYEAR>1995& SYEAR<2024)
sa1 = a %>% pivot_longer(cols=starts_with('LFA'),names_to="LFA",values_to='Landings')
names(sa1)[1] = "YR"

o = bind_rows(sa,sa1)
 o1 = subset(o,LFA %ni% 'LFA38B')

ggplot(o1,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity',width=1)+
  facet_wrap(~LFA, scales='free_y' )+xlab('Fishing Year')+ylab('Landings (kt)')+scale_x_continuous(breaks=round(seq(1995,2023,length=4)))+theme_test()

ggplot(o1, aes(fill=LFA, x=YR, y=Landings/1000)) + 
  geom_bar(position="stack", stat="identity",colour='black')+theme_test()+xlab('Fishing Year')+ylab('Landings (kt)')+
 scale_fill_viridis(discrete=T) 

l38 = aggregate(Landings~YR,data=subset(o,LFA %in% c('LFA38','LFA38B')),FUN=sum)

ggplot(l38,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity',width=1)
