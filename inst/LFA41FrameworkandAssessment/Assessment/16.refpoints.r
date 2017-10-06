require(bio.lobster)
p = bio.lobster::load.environment()
require(bio.polygons)
p$libs = NULL
require(PBSmapping)
require(bio.lobster)
require(bio.utilities)
la()


      Update.plot=T
       if(Update.plot==T) par(mfrow=c(2,2),las=1,mar = c(2,2,2,2),omi=c(0.5,0.5,0.25,0.25))

#


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
        off41$DDLON  = off41$DDLON*-1 #1981 to 1994
        
        logs41$OFFAREA = NULL 
 
off41 = subset(off41,  select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','LOB_EST_LBS','ADJ_LOB_LBS','yr'))
ziff41 = subset(ziff41,select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','EST_WEIGHT_LOG_LBS','ADJCATCH','yr'))
logs41 = subset(logs41, select=c('MON_DOC_ID','VR_NUMBER','DATE_FISHED','DDLAT','DDLON','NUM_OF_TRAPS','EST_WEIGHT_LOG_LBS','ADJCATCH','yr'))

off41 = rename.df(off41,c('LOB_EST_LBS','ADJ_LOB_LBS'),c('EST_WEIGHT_LOG_LBS','ADJCATCH'))                          

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
     

        p=list()
              p$add.reference.lines = FALSE
              p$user.defined.references=NULL
                              p$time.series.start.year = 1970
                              p$time.series.end.year = 2016
                              p$reference.start.year=1970
                              p$reference.end.year=2016
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
          #outbref[[1]] = bref
          #based on bcp              
              ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1])/1000 * 0.4
              llb = ao$w.Yst[which(ao$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000

      
              abline(h=llb,col='orange',lwd=2)
              abline(h=ub,col='green',lwd=2)
         
if(Update.plot==F) {

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
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFDFOSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings / aa$mean/1000
t = which(!is.finite(aa$relF))
aa$mean[t] = 0.001
aa$relF = aa$landings / aa$mean/1000
    

hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=NULL,yrs=aa$yr,ylim=c(0,12),labels=c('USI','LRI'))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataDFOSurvey.png'))
}


####################
###Georges
#####################
rm(out)
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


              

                          p=list()
                                p$add.reference.lines = FALSE
                                p$user.defined.references=NULL
                                                p$time.series.start.year = 1987
                                                p$time.series.end.year = 2016
                                                p$metric = 'weights' #weights
                                                p$measure = 'stratified.total' #'stratified.total'
                                                p$figure.title = ""
                                                p$reference.measure = 'median' # mean, geomean
                                                p$file.name = 'DFOrestratRefLines.png'
                                                p$l.reference.start.year = 1983
                                                p$l.reference.end.year = 2016
                                                p$lref = 0.4*median(out$n.yst[which(out$yr %in% p$l.reference.start.year:p$l.reference.end.year)])
                                  p$u.reference.start.year = 1983
                                                p$u.reference.end.year = 2016
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
   figure.stratified.analysis(x=ao,out.dir = 'bio.lobster', p=p,save=F)

                  #based on bcp              
                  ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1])/1000 * 0.4
                  llb = ao$w.Yst[which(ao$w.Yst>0)]
                  llb = median(sort(llb)[1:5])/1000

              abline(h=llb,col='orange',lwd=2)
       
                  abline(h=ub,col='green',lwd=2)

if(Update.plot==F) {

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
                                        p$time.series.end.year = 2016
                                        p$metric = 'relF' #weights
                                        p$measure = '' #'stratified.total'
                                        p$figure.title = ""
                                        p$reference.measure = 'median' # mean, g6omean
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
          RR75[[2]] = quantile(aa$relF[ii],0.75)
          RR95[[2]] = quantile(aa$relF[ii],0.95)
          rref=   figure.stratified.analysis(x=aa,out.dir = 'bio.lobster', p=p,save=F)
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFGeorgesSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings / aa$mean/1000
t = which(!is.finite(aa$relF))
aa$mean[t] = 0.001
aa$relF = aa$landings / aa$mean/1000

hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=NULL,yrs=aa$yr,ylim=c(0,6),labels=c('USI','LRI'))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataGeorgesSurvey.png'))

}

#######################
###Spring 
########################
    load(file.path(fp,'stratified.nefsc.spring.LFA41.restratified.length.83-300.male&female.sexed.rdata'))
  ao = out[,c('yr','w.Yst')]
         p=list()
              p$add.reference.lines = FALSE
              p$user.defined.references=NULL
                              p$time.series.start.year = 1969
                              p$time.series.end.year = 2016
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.total' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'DFOrestratRefLines.png'
                              p$l.reference.start.year = 1983
                              p$l.reference.end.year = 2016
                              p$lref = 0.4*median(out$n.yst[which(out$yr %in% p$l.reference.start.year:p$l.reference.end.year)])
                p$u.reference.start.year = 1983
                              p$u.reference.end.year = 2016
                            p$uref = median(out$n.yst[which(out$yr %in% 1995:2016)])
                
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
       bref=   figure.stratified.analysis(x=ao,out.dir = 'bio.lobster', p=p,save=F)

ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1])/1000 * 0.4
            llb = ao$w.Yst[which(ao$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000
            abline(h=llb,col='orange',lwd=2)
            abline(h=ub,col='green',lwd=2)

if(Update.plot==F) {

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
                                        p$time.series.end.year = 2016
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
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFSpringSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings/aa$mean/1000
hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=NULL,yrs=aa$yr,ylim=c(0,1),labels=c('USI','LRI'))

savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataSpringSurvey.png'))

}



#########################################################3
#Fall

  load(file.path(fp,'stratified.nefsc.fall.LFA41.restratified.length.83-300.male&female.sexed.rdata'))
  ao = out[,c('yr','w.Yst')]



        p=list()
              p$add.reference.lines = FALSE
              p$user.defined.references=NULL
                              p$time.series.start.year = 1969
                              p$time.series.end.year = 2016
                              p$metric = 'weights' #weights
                              p$measure = 'stratified.total' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'DFOrestratRefLines.png'
                              p$l.reference.start.year = 1983
                              p$l.reference.end.year = 2016
                              p$lref = 0.4*median(out$n.yst[which(out$yr %in% p$l.reference.start.year:p$l.reference.end.year)])
                p$u.reference.start.year = 1983
                              p$u.reference.end.year = 2016
                            p$uref = median(out$n.yst[which(out$yr %in% 1995:2016)])
                
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
               bref=   figure.stratified.analysis(x=ao,out.dir = 'bio.lobster', p=p,save=F)

ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1])/1000 * 0.4
             llb = ao$w.Yst[which(ao$w.Yst>0)]
              llb = median(sort(llb)[1:5])/1000
              abline(h=llb,col='orange',lwd=2)
              abline(h=ub,col='green',lwd=2)

if(Update.plot==F) {

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
                                        p$time.series.end.year = 2016
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
          savePlot(file.path(project.figuredirectory('bio.lobster'),'relFAutumnSurvey.png'))

#HCR plots
aa = merge(bref,aa,by='yr')
aa$relF = aa$landings/aa$mean/1000
hcrPlot(B=aa$mean,mF=aa$relF,USR=ub,LRP=llb,RR=NULL,yrs=aa$yr,ylim=c(0,1),labels=c('USI','LRI'))
savePlot(file.path(project.figuredirectory('bio.lobster'),'HCRllbDataAutumnSurvey.png'))
}

                      if(Update.plot==T)  {

                            mtext("Year",1,1,outer=T)
                            mtext("Stratified Total Weight (t)",2,1,outer=T,las=0)
                          savePlot(file.path(figfp,'RefsPrimary.png'))

                          } 

   
