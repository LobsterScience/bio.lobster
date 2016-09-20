#reproductive potential
#combining mature females abundance at length and fecundity at length
require(bio.lobster)
la()


       ff = c(file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41baseSummerRV.rdata  '),
       		  file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41adjacentpolygonSummerRV.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCspringbase.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCspringadjrestratified.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCfallbase.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCfalladjrestratified.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41dfogeorges.rdata  '))
       

       for(i in 1:length(ff)) {
       			load(ff[i])
	yll = max(aa$n.yst)
	af = aggregate(ObsLobs~yr,data=aa,FUN=sum)
	names(af) = c('x','y')
	h = split(aa,f=aa$yr)
	for(j in 1:length(h)) {
			g = h[[j]]
			g$Mat = ifelse(g$FLEN>=95,1,0)
			g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501) / 1000 # campbell and Robinson 1983
			g$Fecl = g$Fec * g$n.ci.yst.l
			g$Fecu = g$Fec * g$n.ci.yst.u
			g$Fecm = g$Fec * g$n.yst

			m=  aggregate(cbind(Fecm,Fecl,Fecu)~yr,data = g, FUN=sum, na.rm=T)
			

			}
			out = as.data.frame(out)
			names(out) = c('yr','medL','medLlower','medLupper','smallCatch','largeCatch')
			print(median(out$medL))
				p=list()
			                  p$add.reference.lines = F
                              p$time.series.start.year = min(aa$yr)
                              p$time.series.end.year = max(aa$yr)
                              p$metric = 'medianL' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = paste('medianL',strsplit(strsplit(a[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
    		                  print(p$file.name)

                        p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
                        		p$ylim = c(60,155)
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=T
                              p$error.bars=F

                              p$ylim2 = c(0,500)
                             
                       figure.stratified.analysis(x=out,out.dir = 'bio.lobster', x2 = af, p=p,sampleSizes=T)
		}

