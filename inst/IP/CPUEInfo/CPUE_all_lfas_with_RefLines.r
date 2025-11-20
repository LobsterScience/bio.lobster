# CPUEs
require(bio.lobster)
require(bio.utilities)
require(devtools)
require(ggplot2)

la()
h = lobster.db('annual.landings')
i = lobster.db('seasonal.landings')
g = lobster.db('process.logs')

ref = data.frame(LFA=c(27:30,'31A','31B',32,33),lrp=c(.14,.12,.11,.28,.16,.16,.14,.14),usr=c(.27,.25,.22,.56,.41,.32,.29,.28))

g = subset(g, SYEAR<2025 & SYEAR>2004)
ga = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA,data=g,FUN=sum)
ga$cpue = ga$WEIGHT_KG/ga$NUM_OF_TRAPS
l = unique(ga$LFA)
o = list()
for(j in 1:length(l)){
n = subset(ga,LFA==l[j])
running.median = with(rmed(n$SYEAR,n$cpue),data.frame(SYEAR=yr,running.median=x))
o[[j]]=merge(n,running.median,all=T)
}
o = dplyr::bind_rows(o)

#dont need this anymore 
#k = aggregate(cpue~LFA,data=subset(o, LFA %in% 35:38 & SYEAR %in% 2011:2018),FUN=median)
#k$lrp = k$cpue *.2
#k$usr = k$cpue *.4

#k$cpue <- NULL
# ref =dplyr::bind_rows(list(ref,k))

ggplot(subset(o,LFA %in% c(27,28,29,30,'31A','31B',32,33)))+geom_point(aes(x=SYEAR,y=cpue))+geom_line(aes(x=SYEAR,y=running.median),color='blue',size=.8)+
  facet_wrap(~LFA,scales = 'free_y') + geom_hline(data=ref, aes(yintercept = lrp),color='orange',linetype='dashed',size=.8)+
   geom_hline(data=ref, aes(yintercept = usr),color='orange',linetype='solid',size=.8)+ theme_test()+labs(x='Fishing Year',y='CPUE (kg/TH)')

##subarea

g = subset(g, SYEAR<2025 & SYEAR>2004)
ga = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+LFA+subarea,data=g,FUN=sum)
ga$cpue = ga$WEIGHT_KG/ga$NUM_OF_TRAPS

ggplot(subset(ga,LFA %in% c(33,34)),aes(x=SYEAR,y=cpue))+geom_point()+geom_line()+
  facet_wrap(~subarea)+
  theme_test(base_size = 14)


ggplot(subset(o,LFA %in% c(34,35,36,38)))+geom_point(aes(x=SYEAR,y=cpue))+geom_line(aes(x=SYEAR,y=running.median),color='blue',size=.8)+
  facet_wrap(~LFA,scales = 'free_y') +
  theme_test()+labs(x='Fishing Year',y='CPUE (kg/TH)')
