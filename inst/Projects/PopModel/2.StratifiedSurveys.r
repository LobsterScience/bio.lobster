2.StratifiedSurveys

#survey data
p$yrs = 1970:2020
p$size.class= c(53,220)
p$length.based = T
p$by.sex = F
p$bootstrapped.ci=T
p1=p

stratifiedAnalyses = function(p=p1,lfa='EGOM',wd1=wd,redo=F,saveIT=T,singleSurvey=F){
  if(redo){
    p$reweight.strata = T
    p$years.to.estimate = p$yrs
    p$strata.files.return=F
    p$strata.efficiencies=F
    p$clusters = c( rep( "localhost", 7) )
    p$define.by.polygons = T
    p$lobster.subunits=F
    p$area = lfa
    p$return.both = NULL
    p = make.list(list(yrs=p$years.to.estimate),Y=p)
    
    p$series =c('summer')
    p$define.by.polygons=F
    p$vessel.correction.fixed=1.2
    Daout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
    if(singleSurvey) {out = Daout[,c('yr','n.Yst')]; return(out)}
        if(saveIT) write.csv(Daout,file=file.path(wd1,paste(lfa,p$size.class[1],p$size.class[2],'DFOtotalabund.csv',sep="-")))
    
    
    if(!singleSurvey){
      p$season =c('spring')
      out = Saout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
      if(saveIT) write.csv(Saout,file=file.path(wd1,paste(lfa,p$size.class[1],p$size.class[2],'NEFSCSpring.csv',sep="-")))
      
      p$season =c('fall')
    Faout= nefsc.analysis(DS='stratified.estimates.redo',p=p)

    if(saveIT) write.csv(Faout,file=file.path(wd1,paste(lfa,p$size.class[1],p$size.class[2],'NEFSCfall.csv',sep="-")))
    
    }
  
  if(!redo) {Saout = read.csv(file.path(wd1,paste(lfa,p$size.class[1],p$size.class[2],'NEFSCSpring.csv',sep="-")))
  Faout = read.csv(file.path(wd1,paste(lfa,p$size.class[1],p$size.class[2],'NEFSCfall.csv',sep="-")))
  Daout = read.csv(file.path(wd1,paste(lfa,p$size.class[1],p$size.class[2],'DFOtotalabund.csv',sep="-")))
  }
  Saout$CV = Saout$n.yst.se / Saout$n.yst
  Faout$CV = Faout$n.yst.se / Faout$n.yst
  Daout$CV = Daout$n.yst.se / Daout$n.yst
  Saout$Surv='Spring'
  Faout$Surv='Fall'
  Daout$Surv='Summer'
  vars = c('yr','n.Yst','CV','df.yst','Surv','Nsets')
  
  out = as.data.frame(rbind(rbind(Saout[,vars],Faout[,vars]),Daout[,vars]))
  return(out)
  }
}

fi = stratifiedAnalyses(redo=F)
fi$id = paste(fi$yr,fi$Surv,sep='-')

##size based
sc1=seq(53,223,by=5)
sc2=seq(57,227,by=5)
p$bootstrapped.ci=F
p1=p

cout = list()
for(i in 1:length(sc1)){
  p1$size.class = c(sc1[i],sc2[i])
  fi1 = stratifiedAnalyses(redo=T,saveIT = F)
  fi1$sc1=sc1[i]
  cout[[i]] = fi1
}

cc = do.call(rbind,cout)
aggs=aggregate(n.Yst~yr+Surv,data=cc,FUN=sum)
names(aggs)[3] <- 'Tots'
tt = merge(cc,aggs)
tt$Prop = tt$n.Yst/tt$Tots
tt = tt[order(tt$Surv,tt$yr,tt$sc1),]
tt$id = paste(tt$yr,tt$Surv,sep='-')
ttw = reshape(tt[,c('id','sc1','Prop')],idvar='id',timevar = 'sc1',direction='wide')
ff = merge(fi,ttw,all=T)
ff = subset(ff,!is.na(yr))

write.csv(fff,file=file.path(wd,paste('EGOM','AllsurvPropAtLength.csv',sep="-")))

##sex based

sc1=seq(53,223,by=5)
sc2=seq(57,227,by=5)
p$bootstrapped.ci=F
sx = list(c(1,2),2)
sxL = c('tot','fem')
p$by.sex=T
cout = list()
p$yrs=1999:2020
p1=p

m=0
cout = list()
for(i in 1:length(sc1)){
  for(j in 1:length(sx)){
 m=m+1
  p1$size.class = c(sc1[i],sc2[i])
  p1$sex = sx[[j]]
  fi1 = stratifiedAnalyses(p=p1,redo=T,singleSurvey = T)
  fi1$sc1=sc1[i]
  fi1$sxL = sxL[j]
  cout[[m]] = fi1
  }
}

cc = do.call(rbind,cout)
ccT = subset(cc,sxL=='tot')
ccF = subset(cc,sxL=='fem')
names(ccF)[2] ='FemNYST'
names(ccT)[2] ='TOTNYST'


ccc = merge(ccT,ccF,by=c('yr','sc1'))
ccc$PF = ccc$FemNYST/ccc$TOTNYST
ccc = ccc[order(ccc$yr,ccc$sc1),]
ccw = reshape(ccc[,c('yr','sc1','PF')],idvar='yr',timevar = 'sc1',direction='wide')

aaa = na.zero(ccw)
write.csv(aaa,file=file.path(wd,paste('EGOM','PropFem.csv',sep="-")))

