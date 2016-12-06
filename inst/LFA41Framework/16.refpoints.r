#Old reference levels on new indicators

#RV survey

#40% of median catch 1983-1994
#50% median value of 1995-2009

# do this for the survey data and the reproductive potential plots

require(bio.lobster)
load(file.path(project.datadirectory('bio.lobster'),'analysis','stratified.summer.LFA41.restratified.length.all.not.sexed.rdata'))
				p=list()
              p$add.reference.lines = F
              p$user.defined.references=NULL
                              p$time.series.start.year = 1983
                              p$time.series.end.year = 2015
                              p$metric = 'weights' #weights
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
                       ref.out=   figure.stratified.analysis(x=out,out.dir = 'bio.lobster', p=p,save=F)

              
lb = median(subset(out,yr %in% 1983:1994,select=n.yst)[,1])*0.4
ub = median(subset(out,yr %in% 1995:2009,select=n.yst)[,1])*0.5
nub = median(subset(out,yr %in% 1995:2015,select=n.yst)[,1])*0.5

abline(h=lb,col='red',lwd=2)
abline(h=ub,col='green',lwd=2)
abline(h=nub,col='purple',lwd=2)

savePlot('/home/adam/tmp/PezzackRefpointsNewArea.png')


#bayesian change point analysis
require(bcp)
out = subset(out,yr>1981)
b = bcp(out$n.yst,w0 = 0.2, p0 = 0.05)
plot(b,xaxlab=out$yr,xlab='Year')
savePlot('/home/adam/tmp/newareaabundanceBCP.png')  


#big change at 2000

                       ref.out=   figure.stratified.analysis(x=out,out.dir = 'bio.lobster', p=p,save=F)

              
lb = median(subset(out,yr %in% 1983:2000,select=n.yst)[,1])


ub = median(subset(out,yr %in% 2001:2015,select=n.yst)[,1])*0.5
lb = 0.965 # BMSY/2

abline(h=lb,col='red',lwd=2)
abline(h=ub,col='green',lwd=2)

savePlot('/home/adam/tmp/LRPnewareaabundanceNewrefpoints.png')  


fpnes = load('/backup/bio_data/bio.lobster/analysis/stratified.nefsc.spring.LFA41.restratified.length.50-300.not.sexed.rdata')
fpnef = load('/backup/bio_data/bio.lobster/analysis/stratified.nefsc.fall.LFA41.restratified.length.50-300.not.sexed.rdata')

dfofec = read.csv('/backup/bio_data/bio.lobster/analysis/indicators/Fec.maturefemaleLengthFrequenciesLFA41polygonSummerRV.csv')


nsprfec = read.csv('/backup/bio_data/bio.lobster/analysis/indicators/Fec.maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.csv')

nfalfec = read.csv('/backup/bio_data/bio.lobster/analysis/indicators/Fec.maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.csv')



#Removal reference

load(file=file.path(fp,'BiomassLandingsSummer1981-2015.rdata'))
Lm = Lm[which(is.finite(Lm$relF)),]
plot(Lm$yr,Lm$relF,xlab='Year',ylab='Relative F')

  rmean = runmed(Lm$relF,k=3,endrule='median')
lines(Lm$yr,rmean,col='salmon',lwd=2)  
abline(h = median(Lm$relF[which(Lm$yr<2001)]),col='blue',lwd=3)   
abline(h = 1,col='green',lwd=3)   

savePlot('/home/adam/tmp/relativeF.png')