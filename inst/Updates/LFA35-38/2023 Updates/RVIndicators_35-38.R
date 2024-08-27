###########RV SURVEY BOF INDICATORS #################


require(bio.survey)
require(bio.lobster)

p=list()
assessment.year = 2024 ##Check Year
p$syr = 1970:2023
p$yrs = p$syr:assessment.year

p$libs = NULL
ff = "LFA35-38Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fpf1,showWarnings=F)
dir.create(fp1,showWarnings=F)

p$yrs 
p1 = p


stratifiedAnalysesCommercial = function( p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff,wd=10,ht=8){
  p$series =c('summer')# p$series =c('georges');p$series =c('fall')
  p$years.to.estimate = p$yrs
  p$length.based = T
  p$by.sex = T
  p$size.class = c(83,300)
  p$sex = c(1,2)
  p$bootstrapped.ci=T
  p$strata.files.return=F
  p$vessel.correction.fixed=1.2
  p$strat = NULL
  p$clusters = c( rep( "localhost", 7) )
  p$strata.efficiencies = F
  p = make.list(list(yrs=p$years.to.estimate),Y=p)
  p$define.by.polygons = T
  p$lobster.subunits=F
  p$area = lfa
  p$reweight.strata = T #this subsets 
  
  aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
  write.csv(aout,file=file.path(fpf, paste(lfa,'DFOCommB.csv')))
  
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





require(bio.survey)
require(bio.lobster)
require(lubridate)
require(devtools)
require(bio.utilities)
require(PBSmapping)
#la()
#p = bio.lobster::load.environment()
p$libs = NULL
ff = "LFA35-38Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fpf1,showWarnings=F)
dir.create(fp1,showWarnings=F)
p1 = p
p1$yrs = 1970:2024

stratifiedAnalyses = function(p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff,wd=10,ht=8){
  p$series =c('summer')
  p$define.by.polygons = T
  p$lobster.subunits=F
  p$area = lfa
  p$years.to.estimate = p$yrs[-1]
  p$length.based = F
  p$by.sex = F
  p$bootstrapped.ci=T
  p$strata.files.return=F
  p$vessel.correction.fixed=1.2
  p$strat = NULL
  p$clusters = c( rep( "localhost", 7) )
  p$strata.efficiencies = F
  p = make.list(list(yrs=p$years.to.estimate),Y=p)
  p$reweight.strata = T #this subsets 
  
  
  aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
  write.csv(aout,file=file.path(fpf, paste(lfa,'DFOtotalabund.csv')))
  
  
  p$add.reference.lines = F
  p$time.series.start.year = p$years.to.estimate[1]
  p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
  p$metric = 'numbers' #weights
  p$measure = 'stratified.mean' #'stratified.total'
  p$figure.title = ""
  p$reference.measure = 'median' # mean, geomean
  p$file.name = file.path(f,paste(lfa,'DFOrestratifiednumbers.png',sep=""))
  
  p$y.maximum = NULL # NULL # if ymax is too high for one year
  p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
  
  p$legend = FALSE
  p$running.median = T
  p$running.length = 3
  p$running.mean = F #can only have rmedian or rmean
  p$error.polygon=F
  p$error.bars=T
  
  p$ylim=NULL
  if(lfa == 'LFA35-38') p$ylim=c(0,150)
  p$box=T
  p$file.name = file.path(f,paste(lfa,'DFOrestratifiednumbersNOY.png', sep=""))
  ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
  
  p$box=T
  p$ylim=NULL
  p$metric = 'weights'
  p$file.name = file.path(f,paste(lfa,'DFOrestratifiedweightsNOY.png',sep=""))
  ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
  
  p$box=NULL
  p$ylim=NULL
  p$file.name = file.path(f,paste(lfa,'DFOrestratifiedDWAO.png',sep=""))
  p$metric = 'dwao'
  ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
  
  p$file.name = file.path(f,paste(lfa,'DFOrestratifiedgini.png',sep=""))
  p$metric = 'gini'
  p$ylim =c(0,1)
  ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
  p$ylim = NULL
  return(aout)
}

a = stratifiedAnalyses(survey='DFO',lfa='LFA35-38')


###################### COMMERCIAL BIOMASS AND RELATIVE F ###########################

require(bio.lobster)
require(PBSmapping)

a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')
ff = "LFA35-38Assessment"

b$YR = substr(b$SYEAR,6,9)
a = subset(a,YR<1976)
b = subset(b,YR>1975 & YR <=2025)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)

LFA = c('LFA35','LFA36','LFA38')
for(i in LFA){
  aa = a[,c('YR',i)]
  bb = b[,c('YR',i)]
  aa = (rbind(aa,bb))
  aa = aa[order(aa$YR),]
  file.name = paste('Landings',i,'.png',sep="")
  png(file=file.path(fpf1,'LandingsL3538.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
  plot(aa$YR,aa[,i],type='h',lwd=4,col='black',xlab='Year',ylab='Landings (t)')
  lines(aa$YR,runmed(aa[,i],3),col='salmon',lwd=3)
  dev.off()
}


###lfa 35-38

df2 = read.csv(file.path(fpf1,'LFA35-38 DFOCommB.csv'))
df =  read.csv(file.path(fpf1,'LFA35-38 DFOtotalabund.csv'))
df = subset(df,yr<1999)
df2 = subset(df2,yr>1998)
df = as.data.frame(rbind(df,df2))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')] #proportion of total weight that is commercial
df$w.Yst[which(df$yr<1999)] <- df$w.Yst[which(df$yr<1999)]*0.746
df$w.ci.Yst.l[which(df$yr<1999)] <- df$w.ci.Yst.l[which(df$yr<1999)]*0.746
df$w.ci.Yst.u[which(df$yr<1999)] <- df$w.ci.Yst.u[which(df$yr<1999)]*0.746



#png(file=file.path(fpf1,'LFA35-38CommBDFOextended.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(df,plot(yr,w.Yst,pch=1,xlab='Year',ylab='Commerical Biomass (t)',ylim=c(0,9500)))
with(df,arrows(yr,y0=w.ci.Yst.u,y1=w.ci.Yst.l, length=0))
with(subset(df,yr>1998),points(yr,w.Yst,pch=16))
xx = rmed(df$yr,df$w.Yst)
xx = as.data.frame(do.call(cbind,xx))
with(subset(xx,yr<1999),lines(yr,x,col='salmon',lwd=1))
with(subset(xx,yr>1998),lines(yr,x,col='salmon',lwd=3))
dev.off()

a$L3538 = rowSums(a[,12:14])
b$L3538 = rowSums(b[,4:6])
c358 = as.data.frame(rbind(a[,c('YR','L3538')],b[,c('YR','L3538')]))
names(c358)[1] = 'yr'
df  =merge(df,c358)
df$rL = df$L3538/(df$w.ci.Yst.l+df$L3538)
df$rU =df$L3538/ (df$w.ci.Yst.u+df$L3538)
df$rM = df$L3538/(df$w.Yst+df$L3538)


png(file=file.path(fpf1,'LFA3538RelFDFO.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
plot(df$yr,df$rM,type='p',pch=16,col='black',xlab='Year',ylab='Relative F')
arrows(df$yr,y0 = df$rL,y1 = df$rU,length=0)
with(rmed(df$yr,df$rM),lines(yr,x,col='salmon',lwd=3))
dev.off()


############STRATIFIED ANALYSIS _ RECRUITS ##########################

require(bio.survey)
require(bio.lobster)
require(lubridate)
require(devtools)
require(bio.utilities)
require(PBSmapping)
#la()
#p = bio.lobster::load.environment()
p$libs = NULL
ff = "LFA35-38Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
dir.create(fpf1,showWarnings=F)
dir.create(fp1,showWarnings=F)
p1 = p
p1$yrs = 1970:2024

stratifiedAnalysesRecruits = function(p=p1, survey,lfa, fpf = fpf1, fp = fp1,f=ff,wd=10,ht=8){
  p$series =c('summer')# p$series =c('georges');p$series =c('fall')
  p$area = lfa
  p$years.to.estimate = p$yrs
  p$length.based = T
  p$by.sex = T
  p$size.class = c(70,82)
  p$sex = c(1,2)
  p$bootstrapped.ci=T
  p$strata.files.return=F
  p$vessel.correction.fixed=1.2
  p$strat = NULL
  p$clusters = c( rep( "localhost", 7) )
  p$strata.efficiencies = F
  p = make.list(list(yrs=p$years.to.estimate),Y=p)
  p$define.by.polygons = T
  p$lobster.subunits=F
  p$reweight.strata = T #this subsets 
  
  aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
  write.csv(aout,file=file.path(fpf, paste(lfa,'DFOrecruits.csv')))
  
  p$add.reference.lines = F
  p$time.series.start.year = p$years.to.estimate[1]
  p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
  p$metric = 'numbers' #numbers
  p$measure = 'stratified.mean' #'stratified.total'
  p$figure.title = ""
  p$reference.measure = 'median' # mean, geomean
  p$file.name =  file.path(f,paste(lfa,'DFOrestratifiednumbersrecruits.png',sep=""))
  p$y.maximum = NULL # NULL # if ymax is too high for one year
  p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
  p$legend = FALSE
  p$running.median = T
  p$running.length = 3
  p$running.mean = F #can only have rmedian or rmean
  p$error.polygon=F
  p$error.bars=T
  if(lfa == 'LFA35-38') p$ylim = c(0,1200)
  p$file.name =  file.path(f,paste(lfa,'NOYDFOrestratifiednumbersrecruits.png',sep=""))
  ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p,wd=wd,ht=ht)
}

stratifiedAnalysesRecruits(survey='DFO',lfa='LFA35-38')
