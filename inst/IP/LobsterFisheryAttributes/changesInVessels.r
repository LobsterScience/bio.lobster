require(bio.lobster)
la()
g = lobster.db('vessel.by.port.redo')

loa = aggregate(YEAR_BUILT~LFA+YR_FISHED,data=g,FUN=function(x) quantile(x,c(.025,.25,.5,.75,.975)))

rr = subset(loa,LFA==34 & YR_FISHED %in% 2004:2022)

names(rr)[3] = 'LOA'
 plot(rr$YR_FISHED,rr$LOA[,1],type='l',ylim=c(1955,2022),ylab='Year built')
 lines(rr$YR_FISHED,rr$LOA[,4],type='l')
 lines(rr$YR_FISHED,rr$LOA[,3],type='l')
 lines(rr$YR_FISHED,rr$LOA[,2],type='l')
 lines(rr$YR_FISHED,rr$LOA[,5],type='l')

