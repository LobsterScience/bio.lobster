require(bio.survey)
require(bio.lobster)
require(bio.groundfish)

p = bio.lobster::load.environment()
p$libs = NULL
ff = "LFA35-38Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fpf1,showWarnings=F)
dir.create(fp1,showWarnings=F)

p$yrs = 1970:2020
p1 = p


stratifiedAnalysesCommercial = function( p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff,wd=10,ht=8){
   p$write.csv(aout,file=file.path(fpf, paste(lfa,'DFOCommB.csv')))

      p$add.reference.lines = F
    p$time.series.start.year = p$years.to.estimate[1]
    p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
    p$metric = 'weights' #weights
    p$measure = 'stratified.total' #'stratified.total'
    p$figure.title = ""
    p$reference.measure = 'median' # mean, geomean
    p$file.name =  file.path(f,paste(lfa,'DFOrestratifiedtotalweightscommercial.png',sep=""))

    p$y.maximum = NULL # NULL # if ymax is too high for one year
    p$show.truncated.weights = F #if using ymax and want to show the weights that are cut off as values on figure

    p$legend = FALSE
    p$running.median = T
    p$running.length = 3
    p$running.mean = F #can only have rmedian or rmean
     p$error.polygon=F
    p$error.bars=T
    require(bio.lobster)
     p$ylim2 = NULL
    xx = aggregate(ObsLobs~yr,data=aout,FUN=sum)
    names(xx) =c('x','y')
     p$ylim=NULL
     ref.out= figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
     return(aout)
  }



aout = stratifiedAnalysesCommercial(survey='DFO',lfa='LFA35-38')
write.csv(aout,file.path(fp1,'LFA3538CommercialB.csv'))

