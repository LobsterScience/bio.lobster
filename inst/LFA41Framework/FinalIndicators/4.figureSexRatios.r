#Figure sex ratios
# all sizes combined 
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()


      a = c(file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41polygonSummerRV.rdata  ')	,
            file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41dfogeorges.rdata  '),
            file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCfallrestratified.rdata  '),
            file.path(project.datadirectory('bio.lobster'),'analysis','sexLFA41NEFSCspringrestratified.rdata  ') )


for(i in 1:length(a)) {

	load(a[i])
		ap = aggregate(n.yst~yr,data=aa,FUN=sum)
		af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
		apf = merge(ap,af,by='yr')
		apf$pFem = apf$n.yst.y / apf$n.yst.x

       		                  p$add.reference.lines = F
                              p$time.series.start.year = min(aa$yr)
                              p$time.series.end.year = max(aa$yr)
                              p$metric = 'sexRatio' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$ylim=c(0,1)
                              p$file.name = paste(strsplit(strsplit(a[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
                              print(p$file.name)

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T
            			p$ylim2 = c(0,500)
                        xx = aggregate(ObsLobs~yr,data=aa,FUN=sum)
                        names(xx) =c('x','y')
                       ref.out=   figure.stratified.analysis(x=apf[,c('yr','pFem')],out.dir = 'bio.lobster', x2 = xx, p=p,sampleSizes=T)
										}


####################################################
#Just >95mm


        a = c(file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41polygonSummerRV.rdata  ')	,
              file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41dfogeorges.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCfallrestratified.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturesexLFA41NEFSCspringrestratified.rdata  ') )


for(i in 1:length(a)) {

	load(a[i])
		ap = aggregate(n.yst~yr,data=aa,FUN=sum)
		af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
		apf = merge(ap,af,by='yr')
		apf$pFem = apf$n.yst.y / apf$n.yst.x

       		                  p$add.reference.lines = F
                              p$time.series.start.year = min(aa$yr)
                              p$time.series.end.year = max(aa$yr)
                              p$metric = 'sexRatio' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$ylim=c(0,1)
                              p$file.name = paste(strsplit(strsplit(a[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
                              print(p$file.name)

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                              p$ylim2 = c(0,500)
                              xx = aggregate(ObsLobs~yr,data=aa,FUN=sum)
                              names(xx) =c('x','y')
                       ref.out=   figure.stratified.analysis(x=apf[,c('yr','pFem')],out.dir = 'bio.lobster', x2 = xx, p=p,sampleSizes=T)
						}




####################################################
#Just <95mm


        a = c( file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41polygonSummerRV.rdata  ') ,
               file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41dfogeorges.rdata  '),
               file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCfallrestratified.rdata  '),
               file.path(project.datadirectory('bio.lobster'),'analysis','immaturesexLFA41NEFSCspringrestratified.rdata  '))
         


for(i in 1:length(a)) {

  load(a[i])
    ap = aggregate(n.yst~yr,data=aa,FUN=sum)
    af = aggregate(n.yst~yr,data=subset(aa,sex=='femberr'),FUN=sum)
    apf = merge(ap,af,by='yr')
    apf$pFem = apf$n.yst.y / apf$n.yst.x

                            p$add.reference.lines = F
                              p$time.series.start.year = min(aa$yr)
                              p$time.series.end.year = max(aa$yr)
                              p$metric = 'sexRatio' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$ylim=c(0,1)
                              p$file.name = paste(strsplit(strsplit(a[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
                              print(p$file.name)

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T

                              p$ylim2 = c(0,500)
                              xx = aggregate(ObsLobs~yr,data=aa,FUN=sum)
                              names(xx) =c('x','y')
                       ref.out=   figure.stratified.analysis(x=apf[,c('yr','pFem')],out.dir = 'bio.lobster', x2 = xx, p=p,sampleSizes=T)
            }

