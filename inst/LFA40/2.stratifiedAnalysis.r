#####need to rerun this full thing, to get the combined results and to get the gini and dwao running medians figured out for the dfo summer rv surey with missing years
####sept 23, 2016

require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
la()

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
load_all('~/git/bio.survey/')




      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = F
      p$sex = c(1,2) # male female berried c(1,2,3)
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
  
      



##############################################################
#DFO RV Setup

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'custom'
      p$years.to.estimate = c(1970:2018)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = 470:495
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      
     
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
     
#DFO restratified to lfa41
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
  

      aout2= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      #DFO restratified to lfa40
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA40'
      p$reweight.strata = T #this subsets 
  

      aout3= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
      

with(aout3, plot(yr,n.yst,xlab='Year',ylab='Abundance per tow', type='l',col='black',pch=16,lwd=2.5,ylim=c(0,60)))
with(aout2, lines(yr,n.yst,xlab='Year',ylab='Abundance per tow', type='l',col='red',pch=16,lwd=2.5))
with(aout, lines(yr,n.yst,xlab='Year',ylab='Abundance per tow', type='l',col='blue',pch=16,lwd=2.5,lty=3))
savePlot('~/tmp/AbundanceTrends.png',type='png')