 p = list()
 p$init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

p$strat=440:442
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2014)
p$species = c(202,204,300,320)
p$vessel.correction = T
p$vessel.correction.fixed = 1.2
p$length.based = F
p$size.class= c(30,60)
p$by.sex = F

#out = groundfish.db(DS='gsdet.spec.redo',p=p)
p$alpha = 0.05

#out = groundfish.analysis(DS='ab.redo',p=p)


#p$species = c(10,11,12,13,14,16,23,30,31,40,41,42,43,50,60,200,201,202,203,204,300,320,400,610,640)

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
p$add.reference.lines = T
p$time.series.start.year = 1970
p$time.series.end.year = 2013
p$reference.start.year = 1999
p$reference.end.year = 2013
p$add.primary.line = F # the center estimate for reference point
p$metric = 'weights' #weights
p$measure = 'stratified.total' #'stratified.total'

p$reference.measure = 'median' # mean, geomean 
p$file.name = 'winterskate-4vsw.png'
       
#stock reference lines based on primary measure as above
  p$add.upper.lower = T
        p$upper.reference.line = 0.8
        p$lower.reference.line = 0.4
        
        p$figure.title = 'Winter skate 4VsW'
        p$y.maximum = NULL # NULL # if ymax is too high for one year
	p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

        p$legend.placement = 'topright'
        p$running.median = T
		p$running.length = 3
		p$running.mean = F #can only have rmedian or rmean

		

     ref.out=   figure.stratified.analysis(x=aout,p=p)

sfp = file.path(fp,'analysis','saved p files')
dir.create(sfp,recursive=T,showWarnings=F)
save(p,file=file.path(sfp,paste('pfile',p$species,p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))
 




