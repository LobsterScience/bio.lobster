require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")

#nefsc

p$season =c('spring')# p$series =c('spring');p$series =c('fall')
p$define.by.polygons = F
p$lobster.subunits=F
p$area = 'LFA41'
p$reweight.strata = T #this subsets 
#p$area = 'georges.canada' # c('georges.US'); 'LFA412', 'Georges.Bank,'Georges.Basin','Crowell.Basin','SE.Browns','SW.Browns'
p$years.to.estimate = c(1968:2015)
p$length.based = T
p$size.class= c(82,300)
p$by.sex = T
p$sex = 1# male female berried c(1,2,3)
p$bootstrapped.ci=T
p$strata.files.return=F
p$strata.efficiencies=F
p$clusters = c( rep( "localhost", 7) )

p = make.list(list(yrs=p$years.to.estimate),Y=p)

aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)


p$season='fall'

bout =  nefsc.analysis(DS='stratified.estimates.redo',p=p)





#figure stratified analysis Note--the values after comments are the other options
p$add.reference.lines = F
p$time.series.start.year = 1968
p$time.series.end.year = 2015
p$reference.start.year = 1999
p$reference.end.year = 2013
p$add.primary.line = F # the center estimate for reference point
p$metric = 'numbers' #weights
p$measure = 'stratified.mean' #'stratified.total'

p$reference.measure = 'median' # mean, geomean


#stock reference lines based on primary measure as above
  p$add.upper.lower = F
        p$upper.reference.line = 0.8
        p$lower.reference.line = 0.4

        p$figure.title = 'Legal Sized Lobster 35-38'
        p$file.name = paste(p$figure.title,'png',sep=".")
        p$y.maximum = NULL # NULL # if ymax is too high for one year
	p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
p$legend=T
        p$legend.placement = 'topright'
        p$running.median = T
		p$running.length = 3
		p$running.mean = F #can only have rmedian or rmean
p$error.polygon=F
p$error.bars=T


     ref.out=   figure.stratified.analysis(x=aout,p=p,out.dir='bio.lobster')

sfp = file.path(fp,'analysis','saved p files')
dir.create(sfp,recursive=T,showWarnings=F)
save(p,file=file.path(sfp,paste('pfile',p$species,p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))

###----------------------------------------------------------------------------------

#DFO RV Analysis
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")


p$series =c('summer')# p$series =c('georges');p$series =c('fall')
p$define.by.polygons = T
p$lobster.subunits=F
p$area = 'LFA41'
p$reweight.strata = T #this subsets 
p$years.to.estimate = c(1999:2015)
p$length.based = T
p$size.class= c(82,300)
p$by.sex = T
p$sex = c(1,2) # male female berried c(1,2,3)
p$bootstrapped.ci=T
p$strata.files.return=F
p$vessel.correction.fixed=1.2
p$strat = NULL
p$clusters = c( rep( "localhost", 7) )
p$strata.efficiencies = F
p = make.list(list(yrs=p$years.to.estimate),Y=p)

aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)


#survey efficiency
#dfo summer defined by LFA41 polygons
p$strata.efficiencies = T
aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
save(aout,file = file.path(project.datadirectory('bio.lobster'),'analysis','lfa41restratified.survey.efficiency.rdata'))
  #these are currently for commercial size fish
  load(file = file.path(project.datadirectory('bio.lobster'),'analysis','lfa41restratified.survey.efficiency.rdata'))
    pdf(file.path(project.figuredirectory('bio.lobster'),'lfa41restratifiedsurveyefficiency.pdf'))
      with(aout[[1]],plot(yr-0.1,strat.effic.wt,type='h',col='black',xlab='Year', lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
      with(aout[[1]],lines(yr+0.1,alloc.effic.wt,type='h',col='grey40',lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
      legend('topright',lty=c(1,1),lwd=2,col=c('black','grey40'),c('Strata Efficiency','Allocation Efficiency'),bty='n',cex=0.9)
      dev.off()

#dfo summer defined by survey polygons
    p$strata.efficiencies = T
    p$define.by.polygons=F
    p$reweight.strata=F
    aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
    save(aout,file = file.path(project.datadirectory('bio.lobster'),'analysis','lfa41basestratified.survey.efficiency.rdata'))

  #these are currently for commercial size fish
  load(file = file.path(project.datadirectory('bio.lobster'),'analysis','lfa41basestratified.survey.efficiency.rdata'))
    pdf(file.path(project.figuredirectory('bio.lobster'),'lfa41basesurveyefficiency.pdf'))
      with(aout[[1]],plot(yr-0.1,strat.effic.wt,type='h',col='black',xlab='Year', lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
      with(aout[[1]],lines(yr+0.1,alloc.effic.wt,type='h',col='grey40',lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
      legend('topright',lty=c(1,1),lwd=2,col=c('black','grey40'),c('Strata Efficiency','Allocation Efficiency'),bty='n',cex=0.9)
      dev.off()




#by length for histograms
#DFO RV Analysis
        require(bio.lobster)
        p = bio.lobster::load.environment()
        p$libs = NULL
        fp = file.path(project.datadirectory('bio.lobster'),"analysis")


        p$series =c('summer')# p$series =c('georges');p$series =c('fall')
        p$define.by.polygons = T
        p$lobster.subunits=F
        p$area = 'LFA41'
        p$reweight.strata = T #this subsets 
        p$years.to.estimate = c(1999:2015)
        p$length.based = T
        p$size.class= c(82,300)
        p$by.sex = T
        p$sex = c(1,2) # male female berried c(1,2,3)
        p$bootstrapped.ci=F
        p$strata.files.return=F
        p$vessel.correction.fixed=1.2
        p$strat = NULL
        p$clusters = c( rep( "localhost", 7) )
        p$strata.efficiencies = F
        p = make.list(list(yrs=p$years.to.estimate),Y=p)

        a = seq(3,200,3)
        out = list()
        for(i in 1:length(a)) {
          p$size.class=c(a[i],a[i]+2)
          out[[i]] = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
          }

        aa = do.call(rbind,out)
        aa$FLEN = rep(a,each=17)
        save(aa,file=file.path(project.datadirectory('bio.lobster'),'analysis','LengthFrequenciesLFA41polygonSummerRV.rdata  '))

        aa = aa[order(aa$yr),]

        barplot(subset(aa,yr==1999),FLEN,w.yst))
