#reproductive potential
#combining mature females abundance at length and fecundity at length
require(bio.lobster)
require(bcp)
la()
figfp = file.path(project.figuredirectory('bio.lobster'))


       ff = c(
       		  file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata  '),
              file.path(project.datadirectory('bio.lobster'),'analysis','maturefemaleLengthFrequenciesLFA41dfogeorges.rdata  '))
       

#NEFSC Spring survey
  i=2
       			load(ff[i])
          	yll = max(aa$n.yst)
	           h = split(aa,f=aa$yr)
	           out= c()
              	for(j in 1:length(h)) {
              			g = h[[j]]
              			y = unique(g$yr)
              			#g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
              			g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
                    g$Mat = ifelse(g$FLEN<120, g$Mat/2,g$Mat*(2/3))
                    g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
              			g$Fecm = g$Fec * g$n.Yst / 1000000
              			n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
                    out = rbind(out,n)
              			}

      #bayesian change point
        b = bcp(log(out$Fecm),w0 = 0.2, p0 = 0.05)
        plot(b,xaxlab=out$yr,xlab='Year')
        savePlot(file.path(figfp,'BCPRepPotNEFSCSpring.png'))
     
                   							  p=list()
    			                  p$add.reference.lines = F
                                  p$time.series.start.year = min(aa$yr)
                                  p$time.series.end.year = max(aa$yr)
                                  p$metric = 'Fec' #weights
                                  p$measure = 'stratified.mean' #'stratified.total'
                                  p$figure.title = ""
                                  p$reference.measure = 'median' # mean, geomean
                                  p$file.name = paste('Fec',strsplit(strsplit(ff[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
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
                               
                           figure.stratified.analysis(x=out,out.dir = 'bio.lobster', save=F, p=p,sampleSizes=F)
       
		             lb = median(subset(out,yr %in% 1969:2000,select=Fecm)[,1])
                 ub = median(subset(out,yr %in% 2001:2015,select=Fecm)[,1]) * 0.4
                nub = median(subset(out,yr %in% 1969:2015,select=Fecm)[,1])
                 llb = out$Fecm[which(out$Fecm>0)]
                 llb = median(sort(llb)[1:5])
   
                 abline(h=llb,col='orange',lwd=2)
                 abline(h=lb,col='blue',lwd=2)
                 abline(h=ub,col='green',lwd=2)
                 abline(h=nub,col='purple',lwd=2)
                
                savePlot(file.path(figfp,'RefsRepPotNEFSCSpring.png'))
     
#NEFSC Autumn survey
  i=3
            load(ff[i])
            yll = max(aa$n.yst)
             h = split(aa,f=aa$yr)
             out= c()
                for(j in 1:length(h)) {
                    g = h[[j]]
                    y = unique(g$yr)
                    #g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
                    g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
                    g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
                    g$Fecm = g$Fec * g$n.Yst / 1000000
                    n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
                    out = rbind(out,n)
                    }

      #bayesian change point
        b = bcp(log(out$Fecm),w0 = 0.2, p0 = 0.05)
        plot(b,xaxlab=out$yr,xlab='Year')
        savePlot(file.path(figfp,'BCPRepPotNEFSCAutumn.png'))
     
                                  p=list()
                            p$add.reference.lines = F
                                  p$time.series.start.year = min(aa$yr)
                                  p$time.series.end.year = max(aa$yr)
                                  p$metric = 'Fec' #weights
                                  p$measure = 'stratified.mean' #'stratified.total'
                                  p$figure.title = ""
                                  p$reference.measure = 'median' # mean, geomean
                                  p$file.name = paste('Fec',strsplit(strsplit(ff[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
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
                               
                           figure.stratified.analysis(x=out,out.dir = 'bio.lobster', save=F, p=p,sampleSizes=F)
       
                 lb = median(subset(out,yr %in% 1969:1999,select=Fecm)[,1])
                 ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
                nub = median(subset(out,yr %in% 1969:2015,select=Fecm)[,1])
                 llb = out$Fecm[which(out$Fecm>0)]
                 llb = median(sort(llb)[1:5])
   
                 abline(h=llb,col='orange',lwd=2)
                 abline(h=lb,col='blue',lwd=2)
                 abline(h=ub,col='green',lwd=2)
                 abline(h=nub,col='purple',lwd=2)
                
                savePlot(file.path(figfp,'RefsRepPotNEFSCAutumn.png'))
     

#DFO summer
  i=1
            load(ff[i])
            yll = max(aa$n.yst)
             h = split(aa,f=aa$yr)
             out= c()
                for(j in 1:length(h)) {
                    g = h[[j]]
                    y = unique(g$yr)
                    #g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
                    g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
                    g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
                    g$Fecm = g$Fec * g$n.Yst / 1000000
                    n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
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
                                  p$file.name = paste('Fec',strsplit(strsplit(ff[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
                              print(p$file.name)

                            p$y.maximum = NULL # NULL # if ymax is too high for one year
                            p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
                                p$ylim = c(0,75)
                                    p$legend = FALSE
                                    p$running.median = T
                                    p$running.length = 3
                                    p$running.mean = F #can only have rmedian or rmean
                                   p$error.polygon=F
                                  p$error.bars=T
                           figure.stratified.analysis(x=out,out.dir = 'bio.lobster', save=F, p=p,sampleSizes=F)
                          ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
                         abline(h=ub,col='green',lwd=2)
              
  #                          nub = median(subset(out,yr %in% 1999:2015,select=Fecm)[,1])
   #                       llb = out$Fecm[which(out$Fecm>0)]
    #                      llb = median(sort(llb)[1:5])
   
     #            abline(h=llb,col='orange',lwd=2)
      #           abline(h=nub,col='purple',lwd=2)
           

                          savePlot(file.path(figfp,'RefsRepPotDFO.png'))
   
 #  DFO Georges
  i=4
            load(ff[i])
            yll = max(aa$n.yst)
             h = split(aa,f=aa$yr)
             out= c()
                for(j in 1:length(h)) {
                    g = h[[j]]
                    y = unique(g$yr)
                    #g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
                    g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
                    g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
                    g$Fecm = g$Fec * g$n.Yst / 1000000
                    n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
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
                                  p$file.name = paste('Fec',strsplit(strsplit(ff[i],"/")[[1]][6],"\\.")[[1]][1],'png',sep=".")
                              print(p$file.name)

                            p$y.maximum = NULL # NULL # if ymax is too high for one year
                            p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
                                p$ylim = c(0,30)
                                    p$legend = FALSE
                                    p$running.median = T
                                    p$running.length = 3
                                    p$running.mean = F #can only have rmedian or rmean
                                   p$error.polygon=F
                                  p$error.bars=T
                               
                           figure.stratified.analysis(x=out,out.dir = 'bio.lobster', save=F, p=p,sampleSizes=F)
                           nub = median(subset(out,yr %in% 1999:2015,select=Fecm)[,1])
                         # llb = out$Fecm[which(out$Fecm>0)]
                         # llb = median(sort(llb)[1:5])
   
  #                       abline(h=llb,col='orange',lwd=2)
   #                      abline(h=nub,col='purple',lwd=2)
           




                          savePlot(file.path(figfp,'RefsRepPotGeorges.png'))
   