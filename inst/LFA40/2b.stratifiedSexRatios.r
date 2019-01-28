# sex ratios surveys
require(bio.survey)
require(bio.groundfish)
require(bio.lobster)
la()
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

#by length for histograms
#DFO RV Analysis

         p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2018)
        p$length.based = F
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)
        p$size.class = c(50,300)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
		aa$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))
  
    g = aggregate(cbind(n.yst,ObsLobs)~yr,data=aa,FUN=sum)
    k = subset(aa,sex=='femberr',select=c('yr','n.yst'))
  
    aa = merge(g,k,by='yr')

    aa$sexratio = aa$n.yst.y / aa$n.yst.x



#LFA 40
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis")


        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA40'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2018)
        p$length.based = F
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)
        p$size.class=c(50,300)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        ab = do.call(rbind,out)
        ab$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))

    g = aggregate(cbind(n.yst,ObsLobs)~yr,data=ab,FUN=sum)
    k = subset(ab,sex=='femberr',select=c('yr','n.yst'))
  
    ab = merge(g,k,by='yr')

    ab$sexratio = ab$n.yst.y / ab$n.yst.x


#Rest
   require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis")


        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = F
        p$lobster.subunits=F
        p$area = 'custom'
        p$reweight.strata = F #this subsets 
        p$years.to.estimate = c(1999:2018)
        p$length.based = F
        p$by.sex = T
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = c(470:495)
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)
        p$size.class=c(50,300)
        a = list(1,c(2,3))
        out = list()
        for(i in 1:length(a)) {
          p$sex=a[[i]]
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        ac = do.call(rbind,out)
        ac$sex = rep(c('male','femberr'),each=length(p$years.to.estimate))

    g = aggregate(cbind(n.yst,ObsLobs)~yr,data=ac,FUN=sum)
    k = subset(ac,sex=='femberr',select=c('yr','n.yst'))
  
    ac = merge(g,k,by='yr')

    ac$sexratio = ac$n.yst.y / ac$n.yst.x


with(ac,plot(yr,sexratio,xlab='Year',ylab='Proportion Female',type='l',lwd=2.5,lty=3,col='blue',ylim=c(0.4,1)))
with(ab,lines(yr,sexratio,xlab='Year',ylab='Proportion Female',type='l',lwd=2.5,lty=1,col='black',ylim=c(0.4,1)))
with(aa,lines(yr,sexratio,xlab='Year',ylab='Proportion Female',type='l',lwd=2.5,lty=1,col='red',ylim=c(0.4,1)))

savePlot('~/tmp/Sexratios.png',type='png')
