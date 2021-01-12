#tides and cpue
options(stringsAsFactors=F)
require(bio.lobster)
require(bio.utilities)
require(lubridate)

# data from here: http://www.isdm-gdsi.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/data-donnees-eng.asp?user=isdm-gdsi&region=ATL&tst=1&no=65

a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','tides','STJOHNTIDEJAN20052020.csv'),skip=7)
names(a) = c('Date','Height')

a$date = as.Date(a$Date,'%Y/%m/%d')
a$year = year(a$date)

b = aggregate(Height~date+year,data=a,FUN=max)
b = subset(b,Height>6)
with(subset(b,year==2005),plot(date,Height,type='l'))

require(forecast)
					x = subset(b,year==2005)$Height
				 	n <- length(x)
				    x <- as.ts(x)
				    x <- residuals(tslm(x ~ trend)) #removing time series trend
				    n.freq <- 500
				    spec <- spec.ar(c(na.contiguous(x)), plot = T, n.freq = n.freq) # spectral analysis of cycle
				    period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5) #determiing the most likely period

				    plot( 1/spec$freq,spec$spec,type= 'l',xlab='Frequency', ylab='Spectral Density',xlim=c(0,40))
				    abline(h=max(spec$spec),v=1/spec$freq[which.max(spec$spec)],col='red',lwd=2)
			
			#period
			1/spec$freq[which.max(spec$spec)]

			1/spec$freq[which(spec$spec>8)]
			#14 and 28 day cycles


l = lobster.db('process.logs')
#l = subset(l,GRID_NUM %in% c(4,5,6,8,9,10,25,26,27,28,29,31)) #inshore LFA 36
l = subset(l,LFA==36)

a = merge(l,b,by.x='DATE_FISHED',by.y='date')


g = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~DATE_FISHED+SYEAR+WOS+Height+GRID_NUM,data=a,FUN=sum)
g$CPUE = g$WEIGHT_KG/g$NUM_OF_TRAPS

with(subset(g,GRID_NUM==4 & SYEAR==2010),plot(Height,CPUE))
g$GP = ifelse(g$WOS<20,1,2)
require(mgcv)
mo = gam(log(WEIGHT_KG)~as.factor(SYEAR)+s(Height)+as.factor(WOS)+offset(log(NUM_OF_TRAPS)),data=subset(g),family='gaussian')
