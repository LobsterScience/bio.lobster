#2.StratifiedSurveys

#survey data
p$yrs = 1999:2020
p$size.class= c(53,220)
p$length.based = T
p$by.sex = T
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
    p$yrs=1999:2020
    Daout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
    if(singleSurvey) {out = Daout[,c('yr','n.Yst')]; return(out)}
        if(saveIT) write.csv(Daout,file=file.path(wd1,paste(lfa,p$size.class[1],p$size.class[2],p$sex,'DFOtotalabund.csv',sep="-")))
    
    
    if(!singleSurvey){
    p$yrs=1970:2020
    p = make.list(list(yrs=p$yrs),Y=p)
   
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

##sex based



p$bootstrapped.ci=F
sx = list(c(1),c(2,3))
sxL = c('ma','fem')
p$by.sex=T
cout = list()
#p$yrs=1970:2020
p1=p
p$length.based = F

m=0
cout = list()
  for(j in 1:length(sx)){
   m=m+1
  p1$sex = sx[[j]]
  fi1 = stratifiedAnalyses(p=p1,redo=T,saveIT=F,singleSurvey = F)
  fi1$sxL = sxL[j]
  cout[[m]] = fi1
  }

####running in sep R SESSION to here Feb 14 2022
cc = do.call(rbind,cout)
ccT = subset(cc,sxL=='ma')
ccF = subset(cc,sxL=='fem')


ccc = merge(ccT,ccF,by=c('yr','sc1','Surv','Nsets'))
ccc$MaNYST = ccc$TOTNYST - ccc$FemNYST
ccc$ID = paste(ccc$yr,ccc$Surv,sep='-' )
ccc = ccc[order(ccc$yr,ccc$sc1),]
ccd = aggregate(Nsets~ID,data=ccc,FUN=mean)
ccF = reshape(ccc[,c('ID','sc1','FemNYST')],idvar='ID',timevar = 'sc1',direction='wide')
ccF$T = rowSums(ccF[,2:ncol(ccF)])
ccFP = ccF
ccFP[,grepl('FemNYST',names(ccFP))] = ccFP[,grepl('FemNYST',names(ccFP))] / ccFP$T
ccFP = merge(ccFP,ccd,by='ID')

ccM = reshape(ccc[,c('ID','sc1','MaNYST')],idvar='ID',timevar = 'sc1',direction='wide')
ccM$T = rowSums(ccM[,2:ncol(ccM)])
ccMP = ccM
ccMP[,grepl('MaNYST',names(ccMP))] = ccMP[,grepl('MaNYST',names(ccMP))] / ccMP$T
ccMP = merge(ccMP,ccd,by='ID')
ccMP = merge(ccMP,ccd,by='ID')

write.csv(ccMP,file=file.path(wd,paste('EGOM','MALEDFOBYLENGTH.csv',sep="-")))
write.csv(ccFP,file=file.path(wd,paste('EGOM','FEMALEDFOBYLENGTH.csv',sep="-")))




##sex and length

sc1=seq(53,223,by=5)
sc2=seq(57,227,by=5)
p$bootstrapped.ci=F
sx = list(c(1,2),2)
sxL = c('tot','fem')
p$by.sex=T
p$length.based=T
cout = list()
#p$yrs=1970:2020
p1=p

m=0
cout = list()
for(i in 1:length(sc1)){
  for(j in 1:length(sx)){
 m=m+1
  p1$size.class = c(sc1[i],sc2[i])
  p1$sex = sx[[j]]
  fi1 = stratifiedAnalyses(p=p1,redo=T,saveIT=F,singleSurvey = F)
  fi1$sc1=sc1[i]
  fi1$sxL = sxL[j]
  cout[[m]] = fi1
  }
}
####running in sep R SESSION to here Feb 14 2022
cc = do.call(rbind,cout)
ccT = subset(cc,sxL=='tot')
ccF = subset(cc,sxL=='fem')


ccc = merge(ccT,ccF,by=c('yr','sc1','Surv','Nsets'))
ccc$MaNYST = ccc$TOTNYST - ccc$FemNYST
ccc$ID = paste(ccc$yr,ccc$Surv,sep='-' )
ccc = ccc[order(ccc$yr,ccc$sc1),]
ccd = aggregate(Nsets~ID,data=ccc,FUN=mean)
ccF = reshape(ccc[,c('ID','sc1','FemNYST')],idvar='ID',timevar = 'sc1',direction='wide')
ccF$T = rowSums(ccF[,2:ncol(ccF)])
ccFP = ccF
ccFP[,grepl('FemNYST',names(ccFP))] = ccFP[,grepl('FemNYST',names(ccFP))] / ccFP$T
ccFP = merge(ccFP,ccd,by='ID')

ccM = reshape(ccc[,c('ID','sc1','MaNYST')],idvar='ID',timevar = 'sc1',direction='wide')
ccM$T = rowSums(ccM[,2:ncol(ccM)])
ccMP = ccM
ccMP[,grepl('MaNYST',names(ccMP))] = ccMP[,grepl('MaNYST',names(ccMP))] / ccMP$T
ccMP = merge(ccMP,ccd,by='ID')
ccMP = merge(ccMP,ccd,by='ID')

write.csv(ccMP,file=file.path(wd,paste('EGOM','MALEDFOBYLENGTH.csv',sep="-")))
write.csv(ccFP,file=file.path(wd,paste('EGOM','FEMALEDFOBYLENGTH.csv',sep="-")))


aaa = na.zero(ccw)
write.csv(aaa,file=file.path(wd,paste('EGOM','PropFem.csv',sep="-")))

