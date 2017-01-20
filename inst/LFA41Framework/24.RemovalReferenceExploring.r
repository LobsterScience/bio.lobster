

require(bio.groundfish)
require(bio.lobster)
require(bio.polygons)
require(PBSmapping)
require(bio.utilities)
require(PBSmapping)
la()


######### Shared v unique areas

      LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
      LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
      LFA41 = subset(LFA41,SID==1)
        attr(LFA41,'projection') <- 'LL'


#RV
        a = find.bio.gis('summer_strata_labels',return.one.match=F)
        a = read.csv(a,header=T)
        names(a)[4] <- 'label'
        b = find.bio.gis('strat.gf',return.one.match=F)
        b = read.table(b)
        names(b) <- c('X','Y','PID')
        b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
        d = joinPolys(LFA41,b,'INT')
        d = joinPolys(d,operation='UNION')
        d = subset(d,SID %in% c(1,4))
        d = d[c(-251,-252),]
        d = within(d,{POS <- ave(SID,list(SID),FUN=seq_along)})
        attr(d,'projection') <- "LL"
        addPolys(d,col= rgb(1,0,0,.15))       
        RV = d

#GB
        b = file.path(project.datadirectory('bio.polygons'),'data','Science','PED','GeorgesBankStrata.rdata')
        load(b)
        d = joinPolys(LFA41,out,'INT')
        attr(d,'projection') <- "LL"
        dd = joinPolys(d,operation='UNION')
        Ge = dd

#NEFSC
      a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
      l = attributes(a)$PolyData[,c('PID','STRATA')]
      a = merge(a,l,by='PID',all.x=T)
      Ne = subset(a,STRATA %in% c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1340, 1360))
      Ne = joinPolys(Ne,operation='UNION')
      Ne = joinPolys(Ne,LFA41,operation='INT')
      attr(Ne,'projection') <- "LL"
  

calcArea(RV) # 14598

calcArea(Ge) #7275

calcArea(Ne) #18386

calcArea(LFA41) # 32686

calcArea(joinPolys(RV,Ne,'INT')) #8384km 
calcArea(joinPolys(RV,Ge,'INT')) #0km
calcArea(joinPolys(Ne,Ge,'INT')) #7275


#8384/ 18386 = prop Ne biomass to average with RV #0.456
#8384 / 14598 = prop RV biomass to average with Ne  #0.574
#7275 / 18386 = prop Ne within GB #0.396

#1-(8384+7275)/18386 #proportion of Ne area not covered by GB and RV # 0.148
#1-8384 / 14598 = prop RV biomass not included elsewhere #0.426


##How much of the logbook info is within survey area
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

##0.982






####################################
       dir.output = file.path(project.datadirectory('bio.lobster'))

load(file=file.path(project.datadirectory('bio.lobster'),'analysis','RunningMedians.Rdata'))

IRV <- subset(outbref[[1]],yr >1986,select='mean')[,1]
INS <- subset(outbref[[3]],yr >1986 ,select='mean')[,1]
INA <- subset(outbref[[4]],yr >1986,select='mean')[,1]
ING <- subset(outbref[[2]], yr >1986,select='mean')[,1]
ab = cbind(IRV,INS,INA,ING)

#RR50
RR50 = c(RRrv = 2.41,RRns = 0.3,RRna = 0.43,RRgb = 2.13)

#RR75
RR75 = c(RRrv = 5.58,RRns = 0.4838,RRna = 0.6059,RRgb = 2.75)

#RR95
RR95 = c(RRrv = 10.13,RRns = 1.38,RRna = 3.53,RRgb = 16.67)


ab = cbind(IRV,INS,INA,ING)

maxTAC = function(x,RR) {
    #x is vector of biomiass
    #RR is vector of removal references
         RX = x*RR

         #combined area with RV and Ne
         x1 = sum(RX[1]*0.574+RX[2]*0.456+RX[3]*0.456)/3
         #combine area with Ne and GB
         x2 = sum(RX[2]*0.396+RX[3]*0.396+RX[4])/3
         #Ne no overlap
         x3 = sum(RX[2]*0.148+RX[3]*0.148)/2
         #RV no overlap
         x4 = RX[1] * 0.426
         return(x1+x2+x3+x4)

}



out50=c()
for(i in 1:nrow(ab)) {
      out50[i] = maxTAC(ab[i,],RR50)
}


out75=c()
for(i in 1:nrow(ab)) {
      out75[i] = maxTAC(ab[i,],RR75)
}


out95=c()
for(i in 1:nrow(ab)) {
      out95[i] = maxTAC(ab[i,],RR95)
}


plot(1987:2015, out50,xlab='Year',ylab='Max TAC (kt)',type='b',pch=16,ylim=c(0,max(out50)))
abline(h=0.720,col='red')

savePlot(file='/home/adam/tmp/RR50.png')

plot(1987:2015, out75,xlab='Year',ylab='Max TAC (kt)',type='b',pch=16,ylim=c(0,max(out75)))
abline(h=0.720,col='red')

savePlot(file='/home/adam/tmp/RR75.png')


plot(1987:2015, out95,xlab='Year',ylab='Max TAC (kt)',type='b',pch=16,ylim=c(0,max(out95)))
abline(h=0.720,col='red')

savePlot(file='/home/adam/tmp/RR95.png')

#8384/ 18386 = prop Ne biomass to average with RV #0.456
#8384 / 14598 = prop RV biomass to average with Ne  #0.574
#7275 / 18386 = prop Ne within GB #0.396


#1-(8384+7275)/18386 #proportion of Ne area not covered by GB and RV # 0.148
#1-8384 / 14598 = prop RV biomass not included elsewhere #0.426
