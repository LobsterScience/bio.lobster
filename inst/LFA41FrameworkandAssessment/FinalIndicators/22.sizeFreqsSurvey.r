
require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
load_all('~/git/bio.survey/')


de = groundfish.db('gsdet')
de = subset(de,spec==2550)
se = groundfish.db('gsinf')

des = merge(de,se,by='id',all.x=T)
des = makePBS(des,polygon=F)
dH = hist(des$len,freq=F,breaks=seq(0,250,3))

#just within lfa41

 l41 = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA41Offareas.csv'))
                         print('All LFA41 subsetted by LFA Area')
                          l41 = joinPolys(as.PolySet(l41),operation='UNION')
                        attr(l41,'projection') <- 'LL'
                        l41 = subset(l41, SID==1)
                  
                        des$EID = 1:nrow(des)
                        des = completeFun(des,c('X','Y'))
                        a = findPolys(des,l41)
                       iz = which(des$EID %in% a$EID)
                       ss = des[iz,]
sH = hist(ss$len,breaks=seq(0,250,3))


plot(sH$mids,sH$density/max(sH$density),type='l',lwd=2,xlab='Carapace Length',ylab='Scaled Density')
lines(dH$mids,dH$density/max(dH$density),type='l',lwd=2,col='red')
savePlot(file.path(project.figuredirectory('bio.lobster'),'DFORVSurveyLengthFreqAllv41.png'))
#legend('topright',c('All areas','LFA41'),lwd=2,lty=c(1,1),col=c('black','red'),bty='n')
 
 