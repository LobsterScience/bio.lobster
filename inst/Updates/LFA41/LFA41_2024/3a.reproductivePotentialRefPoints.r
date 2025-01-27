#reproductive potential
#combining mature females abundance at length and fecundity at length
require(bio.lobster)
la()
figfp = file.path(project.datadirectory('bio.lobster'),'figures', 'Assessment','LFA41')

    #   ff = c(
       #		  file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata'),
        #      file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata'),
        #      file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata'),
        #      file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','maturefemaleLengthFrequenciesLFA41dfogeorges.rdata'))



ff = c(
  
  file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2024','maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata'),
  file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2024','maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata'),
  file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2024','maturefemaleLengthFrequenciesLFA41dfogeorges.rdata'),
  file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2024','maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata'))

       Update.plot=T
       if(Update.plot==T) par(mfrow=c(2,2),las=1,mar = c(2,2,2,2),omi=c(0.5,0.5,0.25,0.25))

#NEFSC Spring survey
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
                    g$Mat = ifelse(g$FLEN<120, g$Mat/2,g$Mat*(2/3))
                    g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
              			g$Fecm = g$Fec * g$n.Yst / 1000000
              			n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
                    out = rbind(out,n)
              			}

      #bayesian change point
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
                            		p$ylim = c(0,275)
                                    p$legend = FALSE
                                    p$running.median = T
                                    p$running.length = 3
                                    p$running.mean = F #can only have rmedian or rmean
                                   p$error.polygon=F
                                  p$error.bars=T
                               
                           figure.stratified.analysis(x=out,out.dir = 'bio.lobster', save=F, p=p,sampleSizes=F)
       
		             ub = median(subset(out,yr %in% 2001:2015,select=Fecm)[,1]) * 0.4
                 llb = out$Fecm[which(out$Fecm>0)]
                 llb = median(sort(llb)[1:5])
   
                 abline(h=llb,col='dodgerblue4',lwd=2)
                 abline(h=ub,col='lightcyan4',lwd=2, lty = 4 )
                
               # if(Update.plot==F) savePlot(file.path(figfp,'RefsRepPotNEFSCSpring.png', type = "png"))
     
#NEFSC Autumn survey
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
                    g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
                    g$Fecm = g$Fec * g$n.Yst / 1000000
                    n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
                    out = rbind(out,n)
                    }

      #bayesian change point
      
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
                                p$ylim = c(0,275)
                                    p$legend = FALSE
                                    p$running.median = T
                                    p$running.length = 3
                                    p$running.mean = F #can only have rmedian or rmean
                                   p$error.polygon=F
                                  p$error.bars=T
                               
                           figure.stratified.analysis(x=out,out.dir = 'bio.lobster', save=F, p=p,sampleSizes=F)
       
                 ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
                 llb = out$Fecm[which(out$Fecm>0)]
                 llb = median(sort(llb)[1:5])
  
                 
                 abline(h=llb,col='dodgerblue4',lwd=2)
                 abline(h=ub,col='lightcyan4',lwd=2,lty = 4)
                
               # if(Update.plot==F) savePlot(file.path(figfp,'RefsRepPotNEFSCAutumn.png', type = "png"))
     

#DFO summer
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
                                p$ylim =NULL
                                    p$legend = FALSE
                                    p$running.median = T
                                    p$running.length = 3
                                    p$running.mean = F #can only have rmedian or rmean
                                   p$error.polygon=F
                                  p$error.bars=T
                           figure.stratified.analysis(x=out,out.dir = 'bio.lobster', save=F, p=p,sampleSizes=F)
                          ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
                         abline(h=ub,col='lightcyan4',lwd=2,lty = 4)
              
  #                          nub = median(subset(out,yr %in% 1999:2016,select=Fecm)[,1])
   #                       llb = out$Fecm[which(out$Fecm>0)]
    #                      llb = median(sort(llb)[1:5])
   
                       #  abline(h=llb,col='dodgerblue3',lwd=2)
                       #  abline(h=ub,col='aquamarine3',lwd=2)
           

                       # if(Update.plot==F)   savePlot(file.path(figfp,'RefsRepPotDFO.png', type = "png"))
   
 #  DFO Georges
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
                                p$ylim = NULL
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
   
                           # abline(h=llb,col='dodgerblue3',lwd=2)
                          # abline(h=ub,col='aquamarine3',lwd=2)




                     #   if(Update.plot==F)   savePlot(file.path(figfp,'RefsRepPotGeorges.png', type = "png"))
                        if(Update.plot==T)  {

                            mtext("Year",1,1,outer=T)
                            mtext("Reproductive Potential",2,1,outer=T,las=0)
                          savePlot(file.path(figfp,'RefsRepPot.png'), type = "png")

                          } 

   