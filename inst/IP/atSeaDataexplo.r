require(bio.lobster)
require(tidyr)
require(devtools)
require(ggplot2)
require(dplyr)
la()

lobster.db('atSea')

g = subset(atSea, SPECIESCODE==2550 & SEX==3 & !is.na(CLUTCH) & CLUTCH >0)
g$mn = months(g$STARTDATE)
g$n = 1

#egg stages
g$egg = ifelse(g$EGG %in% 1:2,1,g$EGG)
e = aggregate(n~egg+mn+LFA,data=subset(g,LFA %in% 33:35),FUN=sum)
ef = aggregate(n~mn+LFA,data=subset(g,LFA %in% 33:35),FUN=sum)
names(ef)[3]='tot'

e  = merge(e,ef)
e$prop = e$n / e$tot

e$month = factor(e$mn,levels=month.name[c(10:12,1:9)])
ggplot(subset(e,tot>10),aes(x=month,y=prop,fill=as.factor(egg)))+
    geom_bar(stat='identity',position = 'dodge')+facet_wrap(~LFA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#clutch
g = subset(g,EGG<4)
e = aggregate(n~CLUTCH+mn+LFA,data=subset(g,LFA %in% 33:35),FUN=sum)
ef = aggregate(n~mn+LFA,data=subset(g,LFA %in% 33:35),FUN=sum)
names(ef)[3]='tot'

e  = merge(e,ef)
e$prop = e$n / e$tot

e$month = factor(e$mn,levels=month.name[c(10:12,1:9)])
ggplot(subset(e,tot>10),aes(x=month,y=prop,fill=as.factor(CLUTCH)))+
  geom_bar(stat='identity',position = 'dodge')+facet_wrap(~LFA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###shell
g = subset(atSea, SPECIESCODE==2550  & !is.na(SHELL)) 
g$dec = floor(year(g$STARTDATE)/10)*10
g$mn = months(g$STARTDATE)
g$n = 1
g$shell = ifelse(g$SHELL %in% 5:7, 'pre','post' )

e = aggregate(n~SHELL+mn+LFA+dec,data=subset(g,LFA %in% 33:35),FUN=sum)
ef = aggregate(n~mn+LFA+dec,data=subset(g,LFA %in% 33:35),FUN=sum)
names(ef)[4]='tot'

e  = merge(e,ef)
e$prop = e$n / e$tot

e$month = factor(e$mn,levels=month.name[c(10:12,1:9)])
ggplot(subset(e,tot>100 & dec==1980),aes(x=month,y=prop,fill=as.factor(shell)))+
  geom_bar(stat='identity',position = 'dodge')+facet_wrap(~LFA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='1980s')
ggplot(subset(e,tot>100 & dec==2000),aes(x=month,y=prop,fill=as.factor(shell)))+
  geom_bar(stat='identity',position = 'dodge')+facet_wrap(~LFA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='2000s')

ggplot(subset(e,tot>100 & dec==2010),aes(x=month,y=prop,fill=as.factor(shell)))+
  geom_bar(stat='identity',position = 'dodge')+facet_wrap(~LFA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='2010s')

ggplot(subset(e,tot>100 & dec==2020),aes(x=month,y=prop,fill=as.factor(SHELL)))+
  geom_bar(stat='identity',position = 'dodge')+facet_wrap(~LFA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='2020s')


#sex ratio
g = subset(atSea, SPECIESCODE==2550 & !is.na(SEX))
g$n=1
g$sex = ifelse(g$SEX==3,2,g$SEX)
g$dec = floor(year(g$STARTDATE)/10)*10
g$mn = months(g$STARTDATE)
g$size = ifelse(g$CARLENGTH<83,'subleg',ifelse(g$CARLENGTH>82 & g$CARLENGTH<95,'newRec','large'))
ga = aggregate(n~SEX+LFA+mn+size+dec,data=subset(g,SEX==2),FUN=sum)
gb = aggregate(n~LFA+mn+size+dec,data=subset(g),FUN=sum)
names(gb)[5]='total'

ga = merge(ga,gb)
ga$prop = ga$n/ga$total

ga$month = factor(ga$mn,levels=month.name[c(10:12,1:9)])
ggplot(subset(ga,total>100 & dec==2000 & LFA %in% c(33,34,35,36,38,41)),aes(x=LFA,y=prop,fill=month))+
  geom_bar(stat='identity',position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='2000s',y='Proportion Female')


ga$month = factor(ga$mn,levels=month.name[c(10:12,1:9)])
ggplot(subset(ga,total>100 & dec==2010 & LFA %in% c(33,34,35,36,38,41)),aes(x=LFA,y=prop,fill=month))+
  geom_bar(stat='identity',position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='2010s',y='Proportion Female')

ga$month = factor(ga$mn,levels=month.name[c(10:12,1:9)])
ggplot(subset(ga,total>100 & dec==2020 ),aes(x=LFA,y=prop,fill=month))+
  geom_bar(stat='identity',position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='2020s',y='Proportion Female')

ga$month = factor(ga$mn,levels=month.name[c(10:12,1:9)])
ggplot(subset(ga,total>100 & dec==2010 & LFA %in% c(27,'31B',32) ),aes(x=LFA,y=prop,fill=month))+
  geom_bar(stat='identity',position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~size)+
  labs(title='2010s',y='Proportion Female')
