require(bio.lobster)
require(devtools)
require(ggplot2)
la()


sp = lobster.db('process_slips')
sp$woy = lubridate::week(sp$Date)

#seasonal price
wtPri =aggregate(cbind(value,WT_LBS)~LFA+SYEAR+LICENCE_NO,data=sp,FUN=sum)
wtPri$mnV = wtPri$value/wtPri$WT_LBS

b = lobster.db('inflation')
i = which(b$year==2001)
b$nInf = b$amount[1:nrow(b)]/b$amount[i]

bw = merge(wtPri,b,by.x='SYEAR',by.y='year')
bw$infPr = bw$mnV/bw$nInf #adjusted to 1990
bw$Inf_Val = bw$infPr * bw$WT_LBS
bw$T = bw$WT_LBS/2204.62
bwa = aggregate(cbind(T,Inf_Val)~LFA+SYEAR,data=bw,FUN=function(x) c(mean(x),quantile(x,probs=c(0.25,0.75))))
ggplot(subset(bwa,LFA %ni% c(28,41)),aes(x=SYEAR,y=T[,1] ,ymin=T[,2],ymax=T[,3]))+geom_line()+geom_errorbar(width=0)+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Landings Per Licence (T)')+theme_test()+theme(axis.text.x = element_text(size = 7))


ggplot(subset(bwa,LFA %ni% c(28,41)),aes(x=SYEAR,y=Inf_Val[,1] /1000,ymin=Inf_Val[,2]/1000,ymax=Inf_Val[,3]/1000))+geom_line()+geom_errorbar(width=0)+facet_wrap(~LFA)+xlab('Fishing Season')+ylab("Landed Value Per Licence ('000$)")+theme_test()+
  theme(axis.text.x = element_text(size = 7))


#days fished per licence

a = lobster.db('process.logs.unfiltered')
a = subset(a, SYEAR <2025 & SYEAR>2005)


f = lobster.db('season.dates')
b = lobster.db('community_code')
f$days = f$END_DATE-f$START_DATE

sd = aggregate(days~LFA+SYEAR,data=subset(f),FUN=sum)
md = aggregate(days~LFA,data=subset(sd,SYEAR>2004),FUN=median)
names(md)[2]='medP'

ggplot(subset(sd,SYEAR>2004),aes(SYEAR,days))+geom_point()+facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Length of Season (days)')+geom_hline(data=md,aes(yintercept=medP),color='red')+theme_test()+  theme(axis.text.x = element_text(size = 5))

a$DYR = lubridate::decimal_date(a$DATE_FISHED) - lubridate::year(a$DATE_FISHED)
a$WYR = ceiling(a$DYR*52)
a$DWYR = lubridate::year(a$DATE_FISHED) + a$WYR/52
a$P=1

#how many trips
a$ID = paste(a$LICENCE_ID,a$DATE_FISHED,sep="_")
x1 = aggregate(ID~SYEAR+LICENCE_ID+LFA,data=subset(a,WEIGHT_KG>0 & NUM_OF_TRAPS>0),FUN=function(x) length(unique(x)))
x1$P=x1$ID
x1 = merge(x1,sd)
x1$Pp = x1$P/as.numeric(x1$days)

xx = aggregate(P~SYEAR+LFA,data=x1,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))
xp = aggregate(Pp~SYEAR+LFA,data=x1,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))

md = aggregate(Pp[,2]~LFA,data=xp,FUN=median)
names(md)[2]='medP'
#proportion of season 
ggplot(subset(xp,LFA !=28),aes(x=SYEAR,y=Pp[,2],ymin=Pp[,1],ymax=Pp[,3]))+geom_line()+
  #geom_errorbar(width=0)+
  facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Days Fished / Season Length ')+
  geom_hline(data=subset(md,LFA !=28),aes(yintercept=medP),color='red')+theme(axis.text.x = element_text(size = 7))+theme_test()+theme(axis.text.x = element_text(size = 7))

#number of days fished
mda = aggregate(P[,2]~LFA,data=xx,FUN=median)
names(mda)[2]='medP'
ggplot(subset(xx,LFA !=28),aes(x=SYEAR,y=P[,2],ymin=P[,1],ymax=P[,3]))+geom_line()+
  #geom_errorbar(width=0)+
  facet_wrap(~LFA)+xlab('Fishing Season')+ylab('Days Fished ')+
  geom_hline(data=subset(mda,LFA !=28),aes(yintercept=medP),color='red')+theme_test()+theme(axis.text.x = element_text(size = 7))

#Licences per area
GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
gra = st_transform(GrMap,32620)

gra <- gra %>%
  mutate(LFA = case_when(
    LFA == 311 ~ "31A",
    LFA == 312 ~ "31B",
    TRUE       ~ as.character(LFA)  # keep others unchanged
  ))

gra$area = st_area(gra)
gra=aggregate(area~LFA+GRID_NO,data=gra,FUN=sum)
grag=aggregate(area~LFA,data=gra,FUN=sum)
grag$area = as.numeric(grag$area)/1000000

gra$area = as.numeric(gra$area)/1000000
ag = aggregate(WEIGHT_KG~LFA+GRID_NUM+SYEAR,data=a,FUN=sum)

aga = merge(ag,gra,by.x=c('LFA','GRID_NUM'),by.y=c('LFA','GRID_NO'))
names(aga) = c('LFA','grid','SYEAR','landings','area')
library(dplyr)

grids_covering_pct <- function(data, pct = 0.95) {
    data %>%
      group_by(SYEAR, LFA, grid) %>%
      summarise(landings_grid = sum(landings, na.rm = TRUE), .groups = "drop_last") %>%
      arrange(SYEAR, LFA, desc(landings_grid), grid) %>%
      mutate(total_landings = sum(landings_grid),
             cum_landings   = cumsum(landings_grid),
             cum_prop       = cum_landings / total_landings) %>%
      filter(cum_prop <= pct | lag(cum_prop, default = 0) < pct) %>%
      ungroup()
      }

# Example:
re <- grids_covering_pct(aga, pct = 0.95)
rea = merge(gra,re, by.x=c('LFA','GRID_NO'),by.y=c('LFA','grid'))


reaa = aggregate(cbind(area,landings_grid)~LFA+SYEAR,data=rea,FUN=sum)

ggplot(subset(reaa,LFA != 37),aes(x=SYEAR,y=area))+geom_line()+facet_wrap(~LFA)+
  geom_hline(data=subset(grag,LFA !=37),aes(yintercept=area),colour='red')+theme_test()+labs(x='Fishing Season',y='Area Fished accounting for >95% landings')

reaa$land_km2 = reaa$landings_grid/reaa$area
ggplot(subset(reaa,LFA != 37),aes(x=SYEAR,y=land_km2))+geom_line()+facet_wrap(~LFA)+
  theme_test()+labs(x='Fishing Season',y='Landings/km2')


#active licences per LFA and area
ul = aggregate(LICENCE_ID~SYEAR+LFA,data=a,FUN=function(x) length(unique(x)))
ul = subset(ul, SYEAR>2008)

ru = merge(ul, reaa)
ru$lic_km2 = ru$LICENCE_ID/ru$area
ggplot(subset(ru,LFA != 37),aes(x=SYEAR,y=lic_km2))+geom_line()+facet_wrap(~LFA)+
  theme_test()+labs(x='Fishing Season',y='Licences/km2')


