#Old reference levels on new indicators

#RV survey

#40% of median catch 1983-1994
#50% median value of 1995-2009

# do this for the survey data and the reproductive potential plots

require(bio.lobster)
load(file.path(project.datadirectory('bio.lobster'),'analysis','stratified.summer.LFA41.restratified.length.all.not.sexed.rdata'))
				p=list()
              p$add.reference.lines = F
              p$user.defined.references=T
                              p$time.series.start.year = 1983
                              p$time.series.end.year = 2015
                              p$metric = 'numbers' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'DFOrestratRefLines.png'
                              p$l.reference.start.year = 1983
                              p$l.reference.end.year = 2015
                              p$lref = 0.4*median(out$n.yst[which(out$yr %in% p$l.reference.start.year:p$l.reference.end.year)])
							  p$u.reference.start.year = 1983
                              p$u.reference.end.year = 2015
                        	  p$uref = median(out$n.yst[which(out$yr %in% 1995:2015)])
							  
                        p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
                                           p$ylim = c(0,10)
                       ref.out=   figure.stratified.analysis(x=out,out.dir = 'bio.lobster', p=p,save=T)

              




fpnes = load('/backup/bio_data/bio.lobster/analysis/stratified.nefsc.spring.LFA41.restratified.length.50-300.not.sexed.rdata')
fpnef = load('/backup/bio_data/bio.lobster/analysis/stratified.nefsc.fall.LFA41.restratified.length.50-300.not.sexed.rdata')

dfofec = read.csv('/backup/bio_data/bio.lobster/analysis/indicators/Fec.maturefemaleLengthFrequenciesLFA41polygonSummerRV.csv')


nsprfec = read.csv('/backup/bio_data/bio.lobster/analysis/indicators/Fec.maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.csv')

nfalfec = read.csv('/backup/bio_data/bio.lobster/analysis/indicators/Fec.maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.csv')