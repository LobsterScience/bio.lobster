
p = bio.groundfish::load.groundfish.environment("BIOsurvey")
require(bio.survey)
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")

loadfunctions('bio.groundfish')
p$strat=480:490
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2018)
p$functional.groups = T
yy = list()
yy[['LobPred']] = c(10,11,12,13,40,50,200,201,203,204,300)
p$species = 'LobPred'
p$yy = yy

p$vessel.correction = T
p$vessel.correction.fixed = 1.2
p$length.based = F
p$by.sex = p$sex.based = F
p$alpha = 0.05
p$strata.efficiencies=F
#out = groundfish.analysis(DS='ab.redo',p=p)
#MPA functional groups
p$functional.groups = T
p$bootstrapped.ci=T
p$strata.files.return=F

p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
p$runs = p$runs[order(p$runs$v),]
#parallel.run(groundfish.analysis,DS='stratified.estimates.redo',p=p,specific.allocation.to.clusters=T) #silly error arisingexit


aout= groundfish.analysis(DS='stratified.estimates.redo',out.dir = 'bio.lobster',p=p)
write.csv(aout,file=file.path(fp1,'indicators','predatorIndex34.csv'))
                        p$ylim = NULL
              				  p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name =  file.path('LFA3438Framework2019','LobPredatorsabundance34.png')

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

rm(figure.stratified.analysis)
la()
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
              


              				  p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name =  file.path('LFA3438Framework2019','LobPredatorsbiomass34.png')

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                              p$ylim=c(10,160)
rm(figure.stratified.analysis)
la()
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)


##Bof


p = bio.groundfish::load.groundfish.environment("BIOsurvey")
require(bio.survey)
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")

loadfunctions('bio.groundfish')
p$strat=490:495
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2018)
p$functional.groups = T
yy = list()
yy[['LobPred']] = c(10,11,12,13,40,50,200,201,203,204,300)
p$species = 'LobPred'
p$yy = yy

p$vessel.correction = T
p$vessel.correction.fixed = 1.2
p$length.based = F
p$by.sex = p$sex.based = F
p$alpha = 0.05
p$strata.efficiencies=F
#out = groundfish.analysis(DS='ab.redo',p=p)
#MPA functional groups
p$functional.groups = T
p$bootstrapped.ci=T
p$strata.files.return=F

p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
p$runs = p$runs[order(p$runs$v),]
#parallel.run(groundfish.analysis,DS='stratified.estimates.redo',p=p,specific.allocation.to.clusters=T) #silly error arisingexit


aout= groundfish.analysis(DS='stratified.estimates.redo',out.dir = 'bio.lobster',p=p)
write.csv(aout,file=file.path(fp1,'indicators','predatorIndex35-8.csv'))
                        p$ylim = NULL
                        p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name =  file.path('LFA3438Framework2019','LobPredatorsabundance35-38.png')

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

rm(figure.stratified.analysis)
la()
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
              


                        p$add.reference.lines = F
                              p$time.series.start.year = p$years.to.estimate[1]
                              p$time.series.end.year = p$years.to.estimate[length(p$years.to.estimate)]
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name =  file.path('LFA3438Framework2019','LobPredatorsbiomass35-38.png')

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                              p$ylim=c(10,160)
rm(figure.stratified.analysis)
la()
                       ref.out=   figure.stratified.analysis(x=aout,out.dir = 'bio.lobster', p=p)
