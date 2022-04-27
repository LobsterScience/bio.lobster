#LFA 37

require(bio.lobster)
require(bio.utilities)

a = lobster.db('process.logs')

b = subset(a, GRID_NUM %in% c(38,39,40,41,42) & LFA %in% c(36,38)&SYEAR<2022)

g = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA+GRID_NUM+DOS,data=b,FUN=sum)
gg = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA+GRID_NUM,data=b,FUN=sum)

names(gg)[4:5] = c('TTraps','Tweight')

g = merge(g,gg)

gf = split(g,f=list(g$SYEAR,g$LFA,g$GRID_NUM))
gf = rm.from.list(gf)
o = list()
for(i in 1 :length(gf)){
    l = gf[[i]]
    l = l[order(l$DOS),]
    d = data.frame(DOS=seq(min(l$DOS),max(l$DOS,na.rm=T)))
    
    l$ce = cumsum(l$NUM_OF_TRAPS) / l$TTraps
    l$cw = cumsum(l$WEIGHT_KG) / l$Tweight
    ll = merge(l,d,all=T)
    o[[i]] = ll
}

O = do.call(rbind, o)
theme_set(theme_bw())

pdf('Cumulative Effort LFA 37.pdf')
ggplot(subset(O,GRID_NUM==39),aes(x=DOS,y=ce,group=LFA,colour=LFA))+
  geom_point() +
  geom_line() +
  facet_wrap(~SYEAR)+
  labs(title='Grid Number 39', x='Day of Season',y='Cumulative Effort')

ggplot(subset(O,GRID_NUM==40),aes(x=DOS,y=ce,group=LFA,colour=LFA))+
  geom_point() +
  geom_line() +
  facet_wrap(~SYEAR)+
  labs(title='Grid Number 40', x='Day of Season',y='Cumulative Effort')
ggplot(subset(O,GRID_NUM==41),aes(x=DOS,y=ce,group=LFA,colour=LFA))+
  geom_point() +
  geom_line() +
  facet_wrap(~SYEAR)+
  labs(title='Grid Number 41', x='Day of Season',y='Cumulative Effort')
ggplot(subset(O,GRID_NUM==42),aes(x=DOS,y=ce,group=LFA,colour=LFA))+
  geom_point() +
  geom_line() +
  facet_wrap(~SYEAR)+
  labs(title='Grid Number 42', x='Day of Season',y='Cumulative Effort')
dev.off()


ggplot(gg,aes(x=SYEAR,y=TTraps, group=LFA,colour=LFA)) + 
  geom_line() +
  facet_wrap(~GRID_NUM)
