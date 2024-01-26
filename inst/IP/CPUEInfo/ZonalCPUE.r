##area cpues
require(ggplot2)
a=read.csv('~/tmp/uptoDateCPUE.csv')

aa = split(a,f=a$lfa)
calc_zsc = function(x,col) {x$zsc = scale(x[,col]); return(x)}

x1 <- lapply(aa,calc_zsc,'CPUE')

za = do.call(rbind,x1)

ggplot(subset(za,!is.na(lfa)),aes(x=yr,y=zsc))+geom_point()+geom_smooth(se=F)+facet_wrap(~lfa)