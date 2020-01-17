#reproductive potential
#combining mature females abundance at length and fecundity at length
require(bio.lobster)
la()


       ff = c(file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata'),
              file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata'),
              file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata'),
              file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41dfogeorges.rdata'))
       

       for(i in 1:length(ff)) {
       			load(ff[i])
	                 yll = max(aa$n.yst)
	                 af = aggregate(ObsLobs~yr,data=aa,FUN=sum)
	                 names(af) = c('x','y')
      	           h = split(aa,f=aa$yr)
      	           out= c()
      	for(j in 1:length(h)) {
                  	g = h[[j]]
              			y = unique(g$yr)
              			#g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
              			g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
                    g$Mat = ifelse(g$FLEN<120, g$Mat/2,g$Mat*(2/3))
                    g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
              			g$Fecl = g$Fec * g$n.ci.Yst.l / 1000000
              			g$Fecu = g$Fec * g$n.ci.Yst.u / 1000000
              			g$Fecm = g$Fec * g$n.Yst / 1000000
                    nn = sum(g$ObsLobs)
              			#n = aggregate(cbind(Fecm,Fecl,Fecu)~yr,data = g, FUN=sum, na.rm=T)
              			n = aggregate(Fecm~yr,data = g, FUN=sum, na.rm=T)
                    out = rbind(out,n)
              			}
 				
        p=list()
  			                  p$add.reference.lines = F
                                p$time.series.start.year = min(aa$yr)
                                p$time.series.end.year = max(aa$yr)
                                p$metric = 'Fec' #weights
                                p$measure = 'stratified.mean' #'stratified.total'
                                p$figure.title = ""
                                p$reference.measure = 'median' # mean, geomean
                                p$file.name = paste('Fec',strsplit(strsplit(ff[i],"/")[[1]][grep("Length",strsplit(a[i],"/")[[1]])],"\\.")[[1]][1],'png',sep=".")
      		                  print(p$file.name)

                          p$y.maximum = NULL # NULL # if ymax is too high for one year
                          p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
                          		p$ylim = c(0,250)
                                  p$legend = FALSE
                                  p$running.median = T
                                  p$running.length = 3
                                  p$running.mean = F #can only have rmedian or rmean
                                 p$error.polygon=F
                                p$error.bars=T

                                p$ylim2 = c(0,500)
        #if(i %in% c(2,5,8,10)) {p$ylim = NULL; p$file.name = paste('NOY',p$file.name,sep="-")}
        
                       figure.stratified.analysis(x=out,out.dir = 'bio.lobster', x2 = af, p=p,sampleSizes=T)
         names(af) = c('yr','ObsLobs')
         out = merge(out,af)

         fnn = file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','indicators',paste('Fec',strsplit(strsplit(ff[i],"/")[[1]][grep("Length",strsplit(a[i],"/")[[1]])],"\\.")[[1]][1],'csv',sep="."))
          write.csv(out, file=fnn)
          out$ObsLobs = NULL

		}

