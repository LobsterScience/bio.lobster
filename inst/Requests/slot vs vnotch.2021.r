
p = bio.lobster::load.environment()

figdir = file.path(project.datadirectory("bio.lobster","requests","vnotch",p$current.assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

#load("C:/bio.data/bio.lobster/data/ODBCDump/atSea.rdata") #Import AtSea data
#lobster.db('atSea.clean.redo')
#lobster.db('atSea.redo')
#lobster.db('atSea.clean.redo')

atSea.clean=lobster.db('atSea.clean')

lfas= c("27", "31A", "31B", "32")
p$lfas=lfas
a=atSea.clean[atSea.clean$LFA %in% lfas,]
a$yr=year(a$STARTDATE)
b=a[a$yr>2009,]
b=b[is.finite(b$CARLENGTH) & is.finite(b$SEX),]

high=124 #define slot
low=115

for (l in lfas){
    c=b[b$LFA==l,]
    big=5*(ceiling(max(c$CARLENGTH)/5)) #upper bound for hist. Needs to be a multiple of 5
    #print(l)
    cu=c[!duplicated(c[,c('TRIPNO')]),]
    
    png(filename=paste0(figdir, "/Yearly Samples LFA",l,".png"))
        yrs=hist(cu$yr, breaks=10, main=paste0("Samples in ",l), xlab="Year", ylab="Number of samples") #shows distribution of samples across time
    dev.off()
    
 
    png(filename=paste0(figdir, "/Weekly Samples LFA",l,".png"))
        weeks=hist(cu$WOS, breaks=10, main=paste0("Samples in ",l), xlab="Week of Season", ylab="Number of Samples") #shows distribution of samples across time
    dev.off()
    
    png(filename=paste0(figdir,"/Yearly Lobster Sampled LFA",l,".png"))    
      all.animals=hist(c$yr, breaks=10, main=paste0("Animals in ",l),xlab="Year", ylab="Number of Lobster") #shows distribution of lobster sizes
    dev.off()
      
    png(filename=paste0(figdir,"/Size Distribution LFA",l,".png"))
      all=hist(c$CARLENGTH, xlim=c(0,big), breaks=seq(0, big, 5), main=paste0("All Lobster LFA",l), col='grey88', xlab="Carapace Length", ylab="Number of Lobster")
      abline(v = 82.5, col="red", lwd=1, lty=2)
    dev.off()
      
  
    png(filename=paste0(figdir,"/Ovigerous.Length.Freq LFA",l,".png"))
        egg=hist(c$CARLENGTH[c$SEX==3], breaks=seq(0, big, 5),plot=F)
        clr2=ifelse(egg$breaks < low | egg$breaks > high, "grey", "red")[-length(egg$breaks)]
        egg=hist(c$CARLENGTH[c$SEX==3], xlim=c(0,big), breaks=seq(0, big, 5), main=paste0("Ovigerous Females LFA",l), col=clr2, xlab="Carapace Length (mm)")
    dev.off()
  
    tot.wt.slot=sum(c$CALWT[c$CARLENGTH>= low & c$CARLENGTH<high & c$SEX=="2"])
    tot.wt=sum(c$CALWT[c$CARLENGTH>= 82.5 & c$SEX %in% c("1","2")])
    perc.by.wt=round(tot.wt.slot/tot.wt*100,2)
    #print(paste0("Com Wt ",low,"-",high, "mm LFA", l, "= ", round(perc.by.wt,2), "%"))
    
    tot.num.slot=length(c$CARLENGTH[c$CARLENGTH>= low & c$CARLENGTH<high & c$SEX=="2"])
    tot.num=length(c$CALWT[c$CARLENGTH>= 82.5 & c$SEX %in% c("1","2")])
    perc.by.num=round(tot.num.slot/tot.num*100,2)
    #print(paste0("Com # ",low,"-",high, "mm LFA", l, "= ", round(perc.by.num,2), "%"))
    
    png(filename=paste0(figdir,"/Non-Ovigerous.Length.Freq LFA",l,".png"))
        noegg=hist(c$CARLENGTH[c$SEX==2], breaks=seq(0, big, 5),plot=F)
        clr=ifelse(noegg$breaks < low | noegg$breaks > high  , "grey", "red")[-length(noegg$breaks)]
        noegg=hist(c$CARLENGTH[c$SEX==2], xlim=c(0,big), breaks=seq(0, big, 5), main=paste0("Non-Ovigerous Females LFA",l), col=clr, xlab="Carapace Length (mm)")
        legend(x = "right", legend = c(paste0("By Number: ",perc.by.num, "%"), paste0("By Weight: ",perc.by.wt, "%")), 
               bty="n", inset=0.05, title=paste0("Percentage of Catch "), title.col="red" )
    dev.off()
}


###----------------------------------------------
#V-Notch
###----------------------------------------------

lfas=c("31B", "32")

#31B
#Licenses in 31B= 70
#v-notch kg= 70 x50= 3500
#Median number per license=41.75 (from FSRS data)
#1.17 kg/animal

#32
#Licenses= 157
#v-notch kg= 157 x50= 7850
#Median number per license=42.85 (from FSRS data)
#1.20 kg/animal

land=lobster.db('annual.landings')
land=land[land$YR%in% c(2017:2022),]
years=sort(as.numeric(unique(land$YR)))
          

for(l in lfas){
  d1 = data.frame(YEAR = land$YR, LANDINGS = land[,paste0("LFA",l)])
  print(l)
     if(l=="32") {wt.notch=7.85}
     if(l=="31B") {wt.notch=3.5}
     if(l=="32") {num.notch=6542}
     if(l=="31B") {num.notch=2991}
    if(l=="32") {avg.wt.ret=1.9}
    if(l=="31B") {avg.wt.ret=2.77}
      
  d1$NUM= 
  d1$Wt.Back=NA
  d1$Wt.Back=wt.notch
  
  d1$perc.back.wt=NA
  d1$perc.back.wt=wt.notch/d1$LANDINGS*100
  d1=d1[order(d1[,1]),]
  print(d1)
  
  png(filename=paste0(figdir,"/V-notch vs. Slot LFA",l,".png"))
  plot(d1$perc.back.wt, type='n', xlab="Year", ylab="% of catch (by wt)", main=paste0("V-Notched Returned LFA", l), ylim=c(0,3), xaxt='n')
  axis(side = 1, at = c(1:length(d1$YEAR)),labels = unique(d1$YEAR))
  points(d1$perc.back.wt, pch=16)
  #abline(h=avg.wt.ret, lty=2, col="red")
  #text(x=4, y=avg.wt.ret+.1, col="red", labels="115-124mm slot")
  dev.off()
} 

lfas= c("27", "31A", "31B", "32")

#Discards

catches=data.frame(matrix(, nrow=length(lfas), ncol=5))
names(catches)=c("LFA","Legal", "Undersize", "Over.Berried", "Under.Berried")
catches$LFA=lfas

for (i in 1:length(lfas)){
      l=lfas[i]
      c=b[b$LFA==l,]
      u=c[c$CARLENGTH<83 & c$SEX %in% c(1,2),]
      u.ber= c[c$SEX=="3" & c$CARLENGTH<83,]
      o.ber=c[c$SEX=="3" & c$CARLENGTH>82,]
      
      
      #discard=rbind(u,berried)
      
      tot=length(c$SEX)
      under=(length(u$SEX))
      o.egg=length(o.ber$SEX)
      u.egg=length(u.ber$SEX)
      dc.num=under+u.egg+o.egg
        
      catches$Legal[catches$LFA==l]=tot-under-o.egg-u.egg
      catches$Undersize[catches$LFA==l]=under
      catches$Over.Berried[catches$LFA==l]=o.egg
      catches$Under.Berried[catches$LFA==l]=u.egg
     }

library(ggplot2)
library(reshape2)
library(EnvStats)

names(catches)=c("LFA", "Legal", "Undersize", ">mls w/eggs", "<mls w/eggs")
catches=catches[,  c("LFA", "Legal", ">mls w/eggs", "<mls w/eggs", "Undersize")]

bar.p=melt(catches, id.vars="LFA")

png(filename=paste0(figdir,"/catch.proportions 27-32.png"))
ggplot(bar.p, aes(x = LFA, y = value, fill = variable)) + geom_col(position="fill")+ scale_y_continuous(labels = scales::percent)
dev.off()


#ggplot(bar.p, aes(x = LFA, y = value, fill = variable)) + geom_col(position="fill")+ scale_y_continuous(labels = scales::percent ) + stat_n_text(size = 4)


#Vents
#Comparing catch of 50mm (1 15/16") vs 44mm (1 3/4") 
#**Number and placement of vents may also differ

l27=b[b$LFA=="27",]
l27=l27[l27$yr>2016,]
big.vent=l27[l27$CAPTAIN %in% c("David Ferguson", "DAVID FERGUSON"),]
big.vent$vent="Big"
small.vent=l27[l27$CAPTAIN %in% c("Raymond Sherwood", "RAYMOND SHERWOOD"),]
small.vent$vent="Small"

escape=rbind(big.vent, small.vent)
biggest=5*(ceiling(max(escape$CARLENGTH)/5))
vents=unique(escape$vent)
escape$size[escape$vent=="Small"]="44mm"
escape$size[escape$vent=="Big"]="49mm"


for (v in vents){

png(filename=paste0(figdir,"/", v, "Vent.png"))
    all=hist(escape$CARLENGTH[escape$vent==v], xlim=c(0,biggest), breaks=seq(0, biggest, 2.5), main=paste0(escape$size[escape$vent==v][1], " Vent"), 
    col='grey88', xlab="Carapace Length", ylab="Number of Lobster")
abline(v = 82.5, col="red", lwd=2, lty=2)
dev.off()
}

#Hist lines
if (plot.hist.line){
  png(filename=paste0(figdir,"/","Vent Comparison.png")) 
    a=hist(escape$CARLENGTH[escape$vent=="Big"],plot=F,  breaks=seq(0, biggest, 2.5)) 
    plot(a$mids,a$density,type='l', xlim=c(0,biggest),col='blue', xlab="Carapace Length", ylab="Proportion of Catch",main="Vent Comparison")
    
    b=hist(escape$CARLENGTH[escape$vent=="Small"],plot=F,  breaks=seq(0, biggest, 2.5)) 
    lines(b$mids,b$density,type='l',col='red')
    
    abline(v = 82.5, col="grey50", lwd=0.5, lty=2)
    legend(x = "topright", legend = c("44mm", "50mm"), inset=0.05,lty = c(1, 1), col = c("red", "blue"), lwd = 2, title="Vent Height") 
  dev.off()
}

vent.catches=data.frame(matrix(, nrow=length(unique(vents)), ncol=5))
names(vent.catches)=c("Vent","Legal", "Undersize", "Over.Berried", "Under.Berried")
vent.catches$Vent=vents

for (i in 1:length(vents)){
  l=vents[i]
  c=escape[escape$vent==l,]
  u=c[c$CARLENGTH<83 & c$SEX %in% c(1,2),]
  u.ber= c[c$SEX=="3" & c$CARLENGTH<83,]
  o.ber=c[c$SEX=="3" & c$CARLENGTH>82,]
  
  #discard=rbind(u,berried)
  
  tot=length(c$SEX)
  under=(length(u$SEX))
  o.egg=length(o.ber$SEX)
  u.egg=length(u.ber$SEX)
  dc.num=under+u.egg+o.egg
  
  vent.catches$Legal[vent.catches$Vent==l]=tot-under-o.egg-u.egg
  vent.catches$Undersize[vent.catches$Vent==l]=under
  vent.catches$Over.Berried[vent.catches$Vent==l]=o.egg
  vent.catches$Under.Berried[vent.catches$Vent==l]=u.egg
  }

names(vent.catches)=c("Vent", "Legal", "Undersize", ">mls w/eggs", "<mls w/eggs")
vent.catches=vent.catches[,  c("Vent", "Legal", ">mls w/eggs", "<mls w/eggs", "Undersize")]
vent.catches$Vent[vent.catches$Vent=="Small"]="44mm"
vent.catches$Vent[vent.catches$Vent=="Big"]="50mm"

bar.v=melt(vent.catches, id.vars="Vent")

ggplot(bar.v, aes(x = Vent, y = value, fill = variable)) + geom_col(position="fill")+ scale_y_continuous(labels = scales::percent)

ggplot(bar.v, aes(x = LFA, y = value, fill = variable)) + geom_col(position="fill")+ scale_y_continuous(labels = scales::percent ) + stat_n_text(size = 4)















#Following code borrowed from: bio/bio.lobster/inst/Frameworks/LFA2733Framework/1.IndicatorEstimation.CohortAnalysis.r
# Unused for September 2021 analusis


require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()



###for cohort analysis and numbers landed
landings.numbers = T
if(landings.numbers){		
  atSea.clean = lobster.db('atSea.clean')
  atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
  g = lobster.db('annual.landings')
  p = lobster.db('seasonal.landings')
  mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
  mls$X = NULL
  load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
  ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
  ad = ad[order(ad[,1],ad[,2]),]
  names(ad) = c('LFA','YEAR')
  names(mls)[1] <- 'YEAR'
  ad = merge(ad,mls)
  ad = subset(ad,YEAR<2020 & LFA<34)
  atsea = atSea.clean
  cG = lobster.db('community.to.grid.historic')		
  cH = lobster.db('community.to.grid.contemporary')		
  
  out = list()
  outN = list()
  outS = list()
  
  for(i in 1:nrow(ad)) {
    print(ad[i,])
    po = ad[i,'LFA']
    yo = ad[i,'YEAR']
    mm = ad[i,'MLS_MM']
    da = atSeaWeightings(atSea = atsea, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F,at.sea.samples=T,mls=mm)
    op = weightedCLF(x=da,returnLF=T,at.sea.samples=T)
    os = op
    os$vec<-NULL
    outS[[i]] <- os
    # brad ran to here to get data for a plot
    #outS[[i]] <- op$vec
    #} 
    #save(outS,file=file.path(project.datadirectory("bio.lobster"),"outputs","atSeaCLF.rdata"))
    
    #Tc is fractional year of catch
    if(po == 27) 	{ll = "LFA27-30"; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
    if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('28',names(DTs))]]; Tc = 0.67}
    if(po == 29) 	{ll = 'LFA29'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
    if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('30',names(DTs))]]; Tc = 0.67}
    if(po == '31A') {ll = 'LFA29';  	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
    if(po == '31B') {ll = 'LFA32';	 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
    if(po == 32) 	{ll = 'LFA32'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}
    if(po == 33) 	{ll = 'LFA33'; 		lle = 'all areas'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR'); dt = DTs[[grep('33W',names(DTs))]]; Tc = 0.3} 
    
    if(!is.null(op)){
      
      vec = mm:250
      oo = op$vec[op$vec>mm & op$vec<250]
      v0 = hist(oo, breaks=vec,plot=F)
      
      v0$wts = lobLW(v0$mids)
      v0$bwts = v0$counts * v0$wts
      
      
      le = subset(lp,YR == yo)[,2] 
      if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] 
      v0$acWt = v0$bwts / sum(v0$bwts) * le
      v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
      outN[[i]]  = data.frame(N = v0$N, Len = v0$mids,LFA = po, Year = yo,MLS=mm)
      
      #newly recruited fraction
      outS[[i]] = c(outS[[i]], new.rec = sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]) / sum(v0$N[v0$mids %in% seq(mm,mm+100,by=0.5)]))
      outS[[i]] = c(outS[[i]], recWt = sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]*v0$wts[v0$mids %in% seq(mm,mm+11,by=0.5)]) / sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]))
      #Production
      iw = v0$mids>mm
      pMature = sum(pMat(lfa=ll,cl=v0$mids[iw]) * v0$N[iw]) /sum(v0$N[iw])
      eggProd = sum(pMat(lfa=ll,cl=v0$mids) * Fecundity(lle,v0$mids) * v0$N * as.numeric(outS[[i]]['prop.female']))
      outS[[i]] = c(outS[[i]], PropMating = pMature,EggProduction = eggProd)			
      
      
      brks = seq(mm,max(as.numeric(names(dt))),by=5)
      dt = dt[which.min(abs(brks[1]-as.numeric(names(dt)))):length(dt)]
      dt =data.frame(dt=dt,brks = as.numeric(names(dt)))
      dt$dt = dt$dt / 365 #* (5 / (as.numeric(names(dt))*0.15))
      dt = dt[1:length(brks),]
      dt$brks = brks
      v0$LCA = brks[findInterval(v0$mids,vec=brks)]
      LCAN = aggregate(v0$N~v0$LCA,FUN=sum)
      
      LCA = merge(LCAN,dt,by.x='v0$LCA',by.y = 'brks')
      k = which(LCA[,2]==0)[1]
      
      LCA = LCA[1:(k-1),]
      ###need to get dts
      
      ca = cohortAnalysis(lens = LCA[,1], N = LCA[,2], dt = LCA[,3]) #annual
      
      LCA$LFA = po
      LCA$Year = yo
      LCA$MLS = mm
      out[[i]] = c(LFA = po, YEAR=yo, MLS=mm, N = sum(v0$N),Land = le,expl =ca$expl, F = ca$wF,M = ca$M,tF = ca$termF)
    }
  }
  out = as.data.frame(do.call(rbind,out))
  out = toNums(out,2:ncol(out))
  #			save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
  load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
  
  outN = as.data.frame(do.call(rbind,outN))
  #			save(outN,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNatSizeLFA27-33.rdata'))
  load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNatSizeLFA27-33.rdata'))
  
  outS = as.data.frame(do.call(rbind,outS))
  outS = toNums(outS,2:ncol(outS))
  #		   save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata'))
  load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata'))
  
  #####three year windowed expls using the data loaded above
  runLCA = list()
  m=0
  lfa = c(27,29,30,'31A','31B',32,33)
  for(i in 1:length(lfa)){
    o = subset(outN,LFA==lfa[i])
    
    #specific years where data was available for ~3 in a row
    
    if(lfa[i]==27) yrs = list(1990:1992, 1991:1993, 1992:1994, 1993:1995, 1994:1997,1995:1999,1997:2000,1999:2001, 2000:2002,2001:2003, 2002:2004,2003:2005,2004:2007,2005:2009,2007:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
    if(lfa[i]==29) yrs = list(1990:1993, 2008:2015)
    if(lfa[i]==30) yrs = list(1999:2001,2000:2002,2001:2003,2002:2004,2003:2005,2004:2007,2005:2008,2007:2009)
    if(lfa[i]=='31A') yrs = list(2001:2003, 2007:2009,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
    if(lfa[i]=='31B') yrs = list(2002:2004,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
    if(lfa[i]=='32') yrs = list(2001:2003,2002:2004,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
    if(lfa[i]=='33') yrs = list(1985:1987,2001:2003,2002:2004,2009:2012,2010:2014)
    
    if(lfa[i] == 27) 	{dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
    if(lfa[i] == 29) 	{dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
    if(lfa[i] == 30) 	{dt = DTs[[grep('30',names(DTs))]]; Tc = 0.67}
    if(lfa[i] == '31A') {dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
    if(lfa[i] == '31B') {dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
    if(lfa[i] == 32) 	{dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}
    if(lfa[i] == 33) 	{dt = DTs[[grep('33W',names(DTs))]]; Tc = 0.3} 
    
    for(j in 1:length(yrs)){
      m =m+1
      print(m)
      p = subset(o,Year %in% yrs[[j]])
      mm = max(unique(p$MLS))
      p = subset(p,Len>=mm)
      brks = seq(mm,max(as.numeric(names(dt))),by=5)
      dt1 = dt[which.min(abs(brks[1]-as.numeric(names(dt)))):length(dt)]
      dt1 =data.frame(dt=dt1,brks = as.numeric(names(dt1)))
      dt1$dt = dt1$dt / 365 #* (5 / (as.numeric(names(dt))*0.15))
      dt1 = dt1[1:length(brks),]
      dt1$brks = brks
      p$LCA = brks[findInterval(p$Len,vec=brks)]
      p = merge(p,dt1,by.x='LCA',by.y = 'brks')
      p$I = 1
      LCAN = aggregate(cbind(N,I,dt)~LCA,data = p,FUN=sum)
      w = which.max(LCAN$LCA[LCAN$I==15])
      LCAN = LCAN[1:w,]
      LCAN$dt = LCAN$dt/LCAN$I
      k = which(LCAN$N==0)[1]
      if(!is.na(k))LCAN = LCAN[1:(k-1),]
      ca = cohortAnalysis(lens = LCAN$LCA, N = LCAN$N, dt = LCAN$dt)
      runLCA[[m]] = c(LFA = lfa[i],Year.min = min(yrs[[j]]),Year.max = max(yrs[[j]]),F = ca$wF, expl = ca$expl)
    }
  }
  
  runLCA = as.data.frame(do.call(rbind,runLCA))
  runLCA = toNums(runLCA,c('Year.min','Year.max','F','expl'))
  #		save(runLCA,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))
  load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsExploitationAggregatedLFA27-33.rdata')	)	
}




#sensitivity analysis
oo = c()
tf = seq(0.1,2,by=0.1)
for(i in tf){
  oo = c(oo,cohortAnalysis(lens = LCAN$LCA, N = LCAN$N, dt = LCAN$dt,termF=i)$wF)
}

plot(tf,oo,xlab='Terminal F',ylab='Estimated Exploitation',type='b',ylim=c(0.8,0.9))
savePlot(file='/backup/bio_data/bio.lobster/figures/CAsensitivityToTermF.png',type='png')

tf = seq(0.1,0.2,by=0.01)
oo = c()
for(i in tf){
  oo = c(oo,cohortAnalysis(lens = LCAN$LCA, N = LCAN$N, dt = LCAN$dt,M=i)$expl)
}
plot(tf,oo,xlab='Natural Mortality',ylab='Estimated Exploitation',type='b',ylim=c(0.5,0.65))
savePlot(file='/backup/bio_data/bio.lobster/figures/CAsensitivityToM.png',type='png')





#Cohort Analysis Plots

load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))

CAplots(ann = out, yr3 = runLCA)


#sample sizes
load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsLFA27-33.rdata'))
ouS = out[,c('LFA','Year','TotalLobsters')]

load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
out$Year = out$YEAR

