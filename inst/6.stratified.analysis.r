require(bio.survey)
p = bio.groundfish::load.groundfish.environment("BIOsurvey")

fp = file.path(project.datadirectory('bio.groundfish'),"analysis")
p$strat=490:495
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2015)
p$species = c(11)
p$vessel.correction = T
p$vessel.correction.fixed = 1.2
p$length.based = F
p$size.class= c(82,300)
p$by.sex = F
p$sex = 1# male female berried c(1,2,3)

#out = groundfish.db(DS='gsdet.spec.redo',p=p)
p$alpha = 0.05

#out = groundfish.analysis(DS='ab.redo',p=p)
#MPA functional groups
p$functional.groups = F

Xpaper=F
  if(Xpaper){
          p$years.to.estimate = 1999:2013
          p$strat = c(470:483)
          p$species = c(2526)
          p$file.name = 'snowcrab.inshore.4x.png'
          }
#
p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
p$runs = p$runs[order(p$runs$v),]
#parallel.run(groundfish.analysis,DS='stratified.estimates.redo',p=p,specific.allocation.to.clusters=T) #silly error arisingexit

#not finished

aout= groundfish.analysis(DS='stratified.estimates.redo',p=p)

#habitat associations
p$strata.files.return =T
p$plot.name = 'white.hake.4vw.habitat.associations.pdf'
aout= groundfish.analysis(DS='stratified.estimates.redo',p=p)
figure.habitat.associations(aout,p=p)

#redo a's and b's
p$alpha = 0.05
out = groundfish.analysis(DS='ab.redo',p=p)


#figure stratified analysis Note--the values after comments are the other options
p$add.reference.line = F
p$time.series.start.year = 1970
p$time.series.end.year = 2015
p$reference.start.year = 1999
p$reference.end.year = 2013
p$add.primary.line = F # the center estimate for reference point
p$metric = 'numbers' #weights
p$measure = 'stratified.mean' #'stratified.total'

p$reference.measure = 'median' # mean, geomean
p$file.name = 'legal-lfa35-38 lobster.png'

#stock reference lines based on primary measure as above
  p$add.upper.lower = F
        p$upper.reference.line = 0.8
        p$lower.reference.line = 0.4

        p$figure.title = 'Legal Sized Lobster 35-38'
        p$y.maximum = NULL # NULL # if ymax is too high for one year
	p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

        p$legend.placement = 'topright'
        p$running.median = T
		p$running.length = 3
		p$running.mean = F #can only have rmedian or rmean
p$error.polygon=F
p$error.bars=T


     ref.out=   figure.stratified.analysis(x=aout,p=p)

sfp = file.path(fp,'analysis','saved p files')
dir.create(sfp,recursive=T,showWarnings=F)
save(p,file=file.path(sfp,paste('pfile',p$species,p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))



###Standardarding catch rate data


