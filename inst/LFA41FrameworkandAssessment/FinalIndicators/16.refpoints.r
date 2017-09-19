#Old reference levels on new indicators

#RV survey

#40% of median catch 1983-1994
#50% median value of 1995-2009


require(bio.lobster)
p = bio.lobster::load.environment()
require(bio.polygons)
p$libs = NULL
require(PBSmapping)
require(bio.lobster)
require(bio.utilities)
la()
 fp = file.path(project.datadirectory('bio.lobster'),'analysis')
figfp = file.path(project.figuredirectory('bio.lobster'))
outref = list()
RR95=list()    
RR75 = list()
#logbook data
    lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

        logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))
        logs41$yr = year(logs41$DATE_FISHED) #2002 to present
        ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
        ziff41$DDLON = ziff41$DDLON * -1
        off41$yr  = year(off41$DATE_FISHED) #1981 to 1994
        logs41$OFFAREA = NULL 

        #oct16-oct15 fishing year until 2005 switch to Jan 1 to Dec 31

        a41 = rbind(off41,ziff41,logs41)
        a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)
        a41 = makePBS(a41,polygon=FALSE)
        a41$ADJCATCH = a41$ADJCATCH / 2.2 #convert to kgs


TOTALLAND = aggregate(ADJCATCH~fishingYear,data=a41,FUN=sum)
TOTALLAND = rename.df(TOTALLAND,'fishingYear','yr')

#boundaries for fishing data

      LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
      LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
      LFA41 = subset(LFA41,SID==1)
      attr(LFA41,'projection') <- 'LL'

#prune landings to SURVEY
        b = find.bio.gis('strat.gf',return.one.match=F)
        b = read.table(b)
        names(b) <- c('X','Y','PID')
        b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
        b = joinPolys(b,operation='UNION')
        b = joinPolys(b,LFA41,'INT')
        b = subset(b,SID %in% c(1,2))
        b = findPolys(completeFun(a41,c('X','Y')),b)$EID
        aS = subset(a41,EID %in% b)

      La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
      La$landings = La$ADJCATCH / 1000
      La = rename.df(La,'fishingYear','yr')

a = merge(TOTALLAND,La,'yr')
RVPropLand = (a[,3]/a[,2])

 ###DFO RV survey   
    load(file.path(fp,'stratified.summer.LFA41.restratified.length.all.not.sexed.rdata'))

      all.out = out
      load(file.path(fp,'stratified.summer.LFA41.restratified.length.83-300.male&female.sexed.rdata'))
      a = merge(all.out,out,by='yr')
      median(a$w.Yst.y/a$w.Yst.x)    #0.876 proportion of total weight that comprises commercial animals....assuming constant over time....
      ao = all.out[,c('yr','w.Yst')]
      ao$w.Yst = ao$w.Yst * 0.876
      ao$w.Yst[ao$yr %in% out$yr] <- out$w.Yst[which(!is.na(out$yr))]

#ao is full time series of biomasses
      require(bcp)
      h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
      b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
      plot(b,xaxlab=h$yr,xlab='Year')
      savePlot(file.path(figfp,'BCPDFORefpointsNewArea.png'))


        p=list()
              p$add.reference.lines = FALSE
              p$user.defined.references=NULL
                              p$time.series.start.year = 1970
                              p$time.series.end.year = 2015
                              p$reference.start.year=1970
                              p$reference.end.year=2015
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.total' #'stratified.total'
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
                              p$add.primary.line=F
                                           p$return.running=T
                                           p$ylim = c(0,3.5)
                       bref=   figure.stratified.analysis(x=ao,out.dir = 'bio.lobster', p=p,save=F)
outbref[[1]] = bref
          #based on bcp              
              lb = median(subset(ao,yr %in% 1970:1999,select=w.Yst)[,1])/1000
              ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1])/1000 * 0.4
              nub = median(subset(ao,yr %in% 1970:2015,select=w.Yst)[,1])/1000
              llb = ao$w.Yst[which(ao$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

      
              abline(h=llb,col='orange',lwd=2)
              abline(h=lb,col='blue',lwd=2)
              abline(h=ub,col='green',lwd=2)
              abline(h=nub,col='purple',lwd=2)

              savePlot(file.path(figfp,'DFORefpointsNewArea.png'))


#relative F DFO summer
          aa = merge(ao,La,by='yr')
          aa$relF = aa$landings / aa$w.Yst
          t = which(is.finite(aa$relF))
          
                                        p$add.reference.lines = FALSE
                                        p$time.series.start.year = 1981
                                        p$time.series.end.year = 2015
                                        p$metric = 'relF' #weights
                                        p$measure = '' #'stratified.total'
                                        p$figure.title = ""
                                        p$reference.measure = 'median' # mean, geomean
                                        p$file.name = 'relFSummerRV.png'
                                        p$ylim = c(0,13)
                                          p$legend = FALSE
                                          p$running.median = T
                                          p$running.length = 3
                                          p$running.mean = F #can only have rmedian or rmean
                                         p$error.polygon=F
                                        p$error.bars=F
                                        p$return.running=T
          ii = which(aa$yr<2000)
          ii = intersect(t,ii)
          ap = median(aa$relF[ii])
          apt = median(aa$relF[t])
          RR75[[1]] = quantile(aa$relF[ii],0.75)
          RR95[[1]] = quantile(aa$relF[ii],0.95)
          rref=   figure.stratified.analysis(x=aa,out.dir = 'bio.lobster', p=p,save=F)
          abline(h=ap,col='blue',lwd=2)
          abline(h=apt,col='purple',lwd=2)
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFDFOSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings / aa$mean/1000
t = which(!is.finite(aa$relF))
aa$mean[t] = 0.001
aa$relF = aa$landings / aa$mean/1000
    
hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=lb,RR=ap,yrs=aa$yr,ylim=c(0,12))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRDataDFOSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=lb,RR=apt,yrs=aa$yr,ylim=c(0,12))

savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRLongTermDataDFOSurvey.png'))

hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=ap,yrs=aa$yr,ylim=c(0,12))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataDFOSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=llb,RR=apt,yrs=aa$yr,ylim=c(0,12))

savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbLongTermDataDFOSurvey.png'))



####################
###Georges
#####################

                      load(file.path(fp,'stratified.georges.Georges.Canada.base.length.all.not.sexed.rdata'))
                        aout = out
                        aout = aout[,c('yr','w.Yst')]
                        load(file.path(fp,'stratified.georges.Georges.Canada.base.length.83-300.male&female.sexed.rdata'))
                        out = subset(out,yr>=2007)
                        out = out[,c('yr','w.Yst')]

                            aa=merge(out,aout,all.x=T,by='yr')
                            sum(aa[,2])/sum(aa[,3]) #0.872 commercial
                        aout$w.Yst = aout$w.Yst * 0.872
                        aout$w.Yst[aout$yr %in% out$yr] <- out$w.Yst

                  ao = aout


                    require(bcp)
                    h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
                    b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
                    plot(b,xaxlab=h$yr,xlab='Year')
                    savePlot(file.path(figfp,'BCPGeorgesRefpointsNewArea.png'))


                          p=list()
                                p$add.reference.lines = FALSE
                                p$user.defined.references=NULL
                                                p$time.series.start.year = 1987
                                                p$time.series.end.year = 2015
                                                p$metric = 'weights' #weights
                                                p$measure = 'stratified.total' #'stratified.total'
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
                                                             p$ylim = c(0,1.2)
                                                             p$return.running=T
outbref[[2]] = bref=   figure.stratified.analysis(x=ao,out.dir = 'bio.lobster', p=p,save=F)

                  #based on bcp              
                  lb = median(subset(ao,yr %in% 1987:1999,select=w.Yst)[,1])/1000
                  ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1])/1000 * 0.4
                  nub = median(subset(ao,yr %in% 1987:2015,select=w.Yst)[,1])/1000
           llb = ao$w.Yst[which(ao$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

              abline(h=llb,col='orange',lwd=2)
       
                  abline(h=lb,col='blue',lwd=2)
                  abline(h=ub,col='green',lwd=2)
                  abline(h=nub,col='purple',lwd=2)

                  savePlot(file.path(figfp,'GeorgesRefpointsNewArea.png'))

#polygon for landings
      LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
      LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
      LFA41 = subset(LFA41,SID==1)
      attr(LFA41,'projection') <- 'LL'

        b = file.path(project.datadirectory('bio.polygons'),'data','Science','PED','GeorgesBankStrata.rdata')
        load(b)
        d = joinPolys(LFA41,out,'INT')
        attr(d,'projection') <- "LL"
        d = joinPolys(d,operation='UNION')
        d = subset(d,SID==1)
    
       b = findPolys(completeFun(a41,c('X','Y')),d)$EID
       aS = subset(a41,EID %in% b)

      La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
      La$landings = La$ADJCATCH / 1000
      La = rename.df(La,'fishingYear','yr')


a = merge(TOTALLAND,La,'yr')
GBPropLand = (a[,3]/a[,2])

      aa = merge(ao,La,by='yr')

###relative F 
         aa$relF = aa$landings / aa$w.Yst
          t = which(is.finite(aa$relF))

                                        p$add.reference.lines = FALSE
                                        p$time.series.start.year = 1987
                                        p$time.series.end.year = 2015
                                        p$metric = 'relF' #weights
                                        p$measure = '' #'stratified.total'
                                        p$figure.title = ""
                                        p$reference.measure = 'median' # mean, geomean
                                        p$file.name = 'relFSummerRV.png'
                                        p$ylim = c(0,6)
                                          p$legend = FALSE
                                          p$running.median = T
                                          p$running.length = 3
                                          p$running.mean = F #can only have rmedian or rmean
                                         p$error.polygon=F
                                        p$error.bars=F
                                        p$return.running=T
          ii = which(aa$yr<2000)
          ii = intersect(t,ii)
          ap = median(aa$relF[ii])
          apt = median(aa$relF[t])
          RR75[[2]] = quantile(aa$relF[ii],0.75)
          RR95[[2]] = quantile(aa$relF[ii],0.95)
          rref=   figure.stratified.analysis(x=aa,out.dir = 'bio.lobster', p=p,save=F)
          abline(h=ap,col='blue',lwd=2)
          abline(h=apt,col='purple',lwd=2)
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFGeorgesSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings / aa$mean/1000
t = which(!is.finite(aa$relF))
aa$mean[t] = 0.001
aa$relF = aa$landings / aa$mean/1000

hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=lb,RR=ap,yrs=aa$yr,ylims=c(0,6))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRDataGeorgesSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=lb,RR=apt,yrs=aa$yr,ylims=c(0,6))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRLongTermDataGeorgesSurvey.png'))

hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=ap,yrs=aa$yr,ylims=c(0,6))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataGeorgesSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=llb,RR=apt,yrs=aa$yr,ylims=c(0,6))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbLongTermDataGeorgesSurvey.png'))


#######################
###Spring 
########################
  require(bcp)
    load(file.path(fp,'stratified.nefsc.spring.LFA41.restratified.length.83-300.male&female.sexed.rdata'))
  ao = out[,c('yr','w.Yst')]
  h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
  b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
  plot(b,xaxlab=h$yr,xlab='Year')
  savePlot(file.path(figfp,'BCPSpringRefpointsNewArea.png'))



        p=list()
              p$add.reference.lines = FALSE
              p$user.defined.references=NULL
                              p$time.series.start.year = 1969
                              p$time.series.end.year = 2015
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.total' #'stratified.total'
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
                                           p$ylim = c(0,38)
                                           p$return.running=T
           outbref[[3]]=     bref=   figure.stratified.analysis(x=ao,out.dir = 'bio.lobster', p=p,save=F)

lb = median(subset(ao,yr %in% 1969:2000,select=w.Yst)[,1])/1000
ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1])/1000 * 0.4
nub = median(subset(ao,yr %in% 1969:2015,select=w.Yst)[,1])/1000
            llb = ao$w.Yst[which(ao$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

              abline(h=llb,col='orange',lwd=2)
    
abline(h=lb,col='blue',lwd=2)
abline(h=ub,col='green',lwd=2)
abline(h=nub,col='purple',lwd=2)

savePlot(file.path(figfp,'SpringRefpointsNewArea.png'))

#polygon for landings
      LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
      LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
      LFA41 = subset(LFA41,SID==1)
      attr(LFA41,'projection') <- 'LL'
      a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
      l = attributes(a)$PolyData[,c('PID','STRATA')]
      a = merge(a,l,by='PID',all.x=T)
   # addPolys(a,border='red')                      
    b = subset(a,STRATA %in% c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1340, 1360))
     b = joinPolys(b,operation='UNION')
      b = joinPolys(b,LFA41,'INT')
     
      b = findPolys(completeFun(a41,c('X','Y')),b)$EID
      aS = subset(a41,EID %in% b)

     La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
      La$landings = La$ADJCATCH / 1000
      La = rename.df(La,'fishingYear','yr')
      aa = merge(ao,La,by='yr')

a = merge(TOTALLAND,La,'yr')
NPropLand = (a[,3]/a[,2])



###relative F 
         aa$relF = aa$landings / aa$w.Yst
          t = which(is.finite(aa$relF))

                                        p$add.reference.lines = F
                                        p$time.series.start.year = 1981
                                        p$time.series.end.year = 2015
                                        p$metric = 'relF' #weights
                                        p$measure = '' #'stratified.total'
                                        p$figure.title = ""
                                        p$reference.measure = 'median' # mean, geomean
                                        p$file.name = 'relFSummerRV.png'
                                        p$ylim = c(0,2)
                                          p$legend = FALSE
                                          p$running.median = T
                                          p$running.length = 3
                                          p$running.mean = F #can only have rmedian or rmean
                                         p$error.polygon=F
                                        p$error.bars=F
                                        p$return.running=T
          ii = which(aa$yr<2001)
          ii = intersect(t,ii)
          ap = median(aa$relF[ii])
          apt = median(aa$relF[t])
          RR75[[3]] = quantile(aa$relF[ii],0.75)
          RR95[[3]] = quantile(aa$relF[ii],0.95)
          rref=   figure.stratified.analysis(x=aa,out.dir = 'bio.lobster', p=p,save=F)
          abline(h=ap,col='blue',lwd=2)
          abline(h=apt,col='purple',lwd=2)
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFSpringSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings/aa$mean/1000
hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=lb,RR=ap,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRDataSpringSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=lb,RR=apt,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRLongTermDataSpringSurvey.png'))

hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=ap,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataSpringSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=llb,RR=apt,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbLongTermDataSpringSurvey.png'))





#########################################################3
#Fall

  load(file.path(fp,'stratified.nefsc.fall.LFA41.restratified.length.83-300.male&female.sexed.rdata'))
  ao = out[,c('yr','w.Yst')]
  h = subset(ao,w.Yst>0) #need to remove the zeros for bcp to function correctly
  b = bcp(log(h$w.Yst),w0 = 0.2, p0 = 0.05)
  plot(b,xaxlab=h$yr,xlab='Year')
  savePlot(file.path(figfp,'BCPAutumnRefpointsNewArea.png'))



        p=list()
              p$add.reference.lines = FALSE
              p$user.defined.references=NULL
                              p$time.series.start.year = 1969
                              p$time.series.end.year = 2015
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.total' #'stratified.total'
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
                                           p$ylim = c(0,20)
                                           p$return.running=T
              outbref[[4]] =  bref=   figure.stratified.analysis(x=ao,out.dir = 'bio.lobster', p=p,save=F)

lb = median(subset(ao,yr %in% 1969:2000,select=w.Yst)[,1])/1000
ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1])/1000 * 0.4
nub = median(subset(ao,yr %in% 1969:2015,select=w.Yst)[,1])/1000
             llb = ao$w.Yst[which(ao$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

              abline(h=llb,col='orange',lwd=2)

abline(h=lb,col='blue',lwd=2)
abline(h=ub,col='green',lwd=2)
abline(h=nub,col='purple',lwd=2)

savePlot(file.path(figfp,'AutumnRefpointsNewArea.png'))

#polygon for landings
      LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
      LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
      LFA41 = subset(LFA41,SID==1)
      attr(LFA41,'projection') <- 'LL'
      a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
      l = attributes(a)$PolyData[,c('PID','STRATA')]
      a = merge(a,l,by='PID',all.x=T)
   # addPolys(a,border='red')                      
    b = subset(a,STRATA %in% c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1340, 1360))
     b = joinPolys(b,operation='UNION')
      b = joinPolys(b,LFA41,'INT')
     
      b = findPolys(completeFun(a41,c('X','Y')),b)$EID
      aS = subset(a41,EID %in% b)

     La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
      La$landings = La$ADJCATCH / 1000
      La = rename.df(La,'fishingYear','yr')
      aa = merge(ao,La,by='yr')

###relative F 
         aa$relF = aa$landings / aa$w.Yst
          t = which(is.finite(aa$relF))


                                p$add.reference.lines = FALSE
                                        p$time.series.start.year = 1981
                                        p$time.series.end.year = 2015
                                        p$metric = 'relF' #weights
                                        p$measure = '' #'stratified.total'
                                        p$figure.title = ""
                                        p$reference.measure = 'median' # mean, geomean
                                        p$file.name = 'relFSummerRV.png'
                                        p$ylim = c(0,2)
                                          p$legend = FALSE
                                          p$running.median = T
                                          p$running.length = 3
                                          p$running.mean = F #can only have rmedian or rmean
                                         p$error.polygon=F
                                        p$error.bars=F
                                        p$return.running=T
          ii = which(aa$yr<2001)
          ii = intersect(t,ii)
          ap = median(aa$relF[ii])
          apt = median(aa$relF[t])
          RR75[[4]] = quantile(aa$relF[ii],0.75)
          RR95[[4]] = quantile(aa$relF[ii],0.95)
          rref=   figure.stratified.analysis(x=aa,out.dir = 'bio.lobster', p=p,save=F)
          abline(h=ap,col='blue',lwd=2)
          abline(h=apt,col='purple',lwd=2)
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFAutumnSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings/aa$mean/1000
hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=lb,RR=ap,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRDataAutumnSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=lb,RR=apt,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRLongTermDataAutumnSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=ap,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataAutumnSurvey.png'))
hcrPlot(B=aa$mean,mF=aa$relF,USR=nub,LRP=llb,RR=apt,yrs=aa$yr)
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbLongTermDataAutumnSurvey.png'))

save(outbref,file=file.path(project.datadirectory('bio.lobster'),'analysis','RunningMedians.Rdata'))




RVPropLand
GBPropLand
NPropLand