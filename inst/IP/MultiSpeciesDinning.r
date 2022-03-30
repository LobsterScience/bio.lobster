
require(bio.lobster)
require(bio.utilities)
require(devtools)
load_all('C:/Users/Cooka/Documents/git/bio.survey')
p=list()
p$strat=490:495
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2021)
p$functional.groups = F
p$species = c(10,11,50,300,320,220,2550)

p$vessel.correction = T
p$vessel.correction.fixed = 1.2
p$length.based = F
p$by.sex = p$sex.based = F
p$alpha = 0.05
p$strata.efficiencies=F
p$functional.groups = F
p$bootstrapped.ci=T
p$strata.files.return=F

p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
p$runs = p$runs[order(p$runs$v),]
aout= groundfish.analysis(DS='stratified.estimates.redo',out.dir = 'bio.lobster',p=p)
setwd('C:/users/cooka/Documents/bio_data/bio.lobster/analysis')
j = grep('*.rdata',dir())
k = dir()[j]
oo = list()
for(i in 1:length(k)){
load(k[i])
oo[[i]] = out
}

out = do.call(rbind, oo)

yy = list()
yy[['Skates']] = c(200,201,203,204)
p$species = 'Skates'
p$yy = yy

p$functional.groups = T
p$bootstrapped.ci=T
p$strata.files.return=F

p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
aout= groundfish.analysis(DS='stratified.estimates.redo',out.dir = 'bio.lobster',p=p)
write.csv(aout,file=file.path('BoFSkatesIndex.csv'))


yy = list()
yy[['Hake']] = c(12,13,14)
p$species = 'Hake'
p$yy = yy

p$functional.groups = T
p$bootstrapped.ci=T
p$strata.files.return=F

p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
aout= groundfish.analysis(DS='stratified.estimates.redo',out.dir = 'bio.lobster',p=p)
write.csv(aout,file=file.path('BoFHakeIndex.csv'))
