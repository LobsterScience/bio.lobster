#LFA 36 Season Extension
require(bio.lobster)
require(lubridate)
require(bio.utilities)
options(stringsAsFactors=F)
setwd(file.path(project.datadirectory('bio.lobster'),'data','LFA36RecTraps','2019'))


xy = read.csv('LFA36PositionTable_Dec19.csv')
ca = read.csv('Catch.csv')
tr = read.csv('Trapss.csv')


LobsterMap('36')
		xy = makePBS(xy,polygon=F,coords=c('Longitude','Latitude'))
		xy$X =convert.dd.dddd(xy$X)*-1
		xy$Y =convert.dd.dddd(xy$Y)
		addPoints(xy,col='red',pch=16,cex=.5)

LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
attr(LFAgrid,'projection') <- "LL"
g = findPolys(xy,LFAgrid)
xy = merge(xy,g[,c('EID','SID')])
xy$Date = as.Date(xy$Date,format='%d-%b-%Y')
i = which(xy$Date=='18-06-01')
xy$Date[i] = xy$Date[i]+365

Fu = merge(tr,merge(xy,ca,by=c('Record.Number')),by=c('Record.Number')) #lobster 0 is an empty trap


Fu$LID = paste(Fu$Lobster.Number,Fu$Trap.Number,Fu$Record.Number,sep="-")
Fu$TID = paste(Fu$Trap.Number,Fu$Record.Number,sep="-")

sz = read.csv('sizes.csv')

Fu$R1 = ifelse(Fu$Size %in% c(15,16) | c(Fu$Size == 17 & Fu$Short ==T),1,0)
Fu$R0 = ifelse(Fu$Size %in% c(18) | c(Fu$Size == 17 & Fu$Short ==FALSE),1,0)

Fu$DoSa = as.numeric(Fu$Date - min(Fu$Date))
Fu$WoSa = round(Fu$DoSa/7)

#Total area
a = aggregate(cbind(R1,R0)~DoSa, data=Fu, FUN=sum)
a$p = a$R0/(a$R1+a$R0)

logs = lobster.db('process.logs')
l = subset(logs,LFA==36 & SYEAR==2019 & DATE_FISHED > '2019-04-05')
l$DoSa = as.numeric(l$DATE_FISHED - min(l$DATE_FISHED))
b = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DoSa+DATE_FISHED, data=l, FUN=sum)
b$cumL = cumsum(b$WEIGHT_KG) / sum(b$WEIGHT_KG)


dd = merge(a,b,by.x='DoSa')
dd = subset(dd,is.finite(p))

require(bio.ccir)

#full season
da = list()
da$method = 'binomial'
da$n = nrow(dd)
da$p = dd$p
da$Cuml = dd$cumL
da$N = dd$R1+dd$R0
da$E = dd$R0
da$dates = dd$DATE_FISHED
		x = ccir_stan_run(dat = da)
			ccir_stan_plots(x,type='predicted')
			 	ccir_stan_plots(x,type='exploitation')
			 Full= ccir_stan_summarize(x)$ERf


#without extension
dd = subset(dd,DATE_FISHED<'2019-07-01')

require(bio.ccir)

#full season
da = list()
da$method = 'binomial'
da$n = nrow(dd)
da$p = dd$p
da$Cuml = dd$cumL
da$N = dd$R1+dd$R0
da$E = dd$R0
da$dates = dd$DATE_FISHED
		x = ccir_stan_run(dat = da)
			ccir_stan_plots(x,type='predicted')
			 	ccir_stan_plots(x,type='exploitation')
			 	Old = ccir_stan_summarize(x)$ERf
