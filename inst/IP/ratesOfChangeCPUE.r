#cpue modelling trends 
	cpue1= CPUEModelPlot(CPUEModelResults,TempModelling,lfa = '35',xlim=c(1989,2019.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=1,wd=11,ht=8)
stripped out the pData for LFA 36
load('~/tmp/l36cpuepdata.rdata')


dat = split(pData,f=pData$fYEAR)
gg = g = c()

for(i in 2:length(dat)){
	a = subset(dat[[i]],DOS<70)
	ag = coef(lm(log(mu)~DOS,data=a))
	g = c(g,round((1-exp(ag[2]*nrow(a))),digits=3))
	ag = coef(lm(log(TEMP)~DOS,data=a))
    gg = c(gg,round((1-exp(ag[2]*nrow(a))),digits=3))
	}

g is pretty constant