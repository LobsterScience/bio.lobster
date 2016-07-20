require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")

#nefsc

p$season =c('spring')# p$series =c('spring');p$series =c('fall')
p$area = 'georges.canada' # c('georges.US'); 'LFA412', 'Georges.Bank,'Georges.Basin','Crowell.Basin','SE.Browns','SW.Browns'
p$years.to.estimate = c(1968:2015)
p$lobster.subunits=F
p$length.based = T
p$size.class= c(82,300)
p$by.sex = F
p$sex = 1# male female berried c(1,2,3)



p$clusters = c( rep( "localhost", 7) )

p = make.list(list(yrs=p$years.to.estimate),Y=p)

#parallel.run(groundfish.analysis,DS='stratified.estimates.redo',p=p,specific.allocation.to.clusters=T) #silly error arisingexit

#not finished

aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)

#habitat associations

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


