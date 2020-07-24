require(bio.lobster)
la()
require(lubridate)
require(bio.utilities)
require(PBSmapping)
anc = bnamR(redo=F)


	TempData = read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","sabmpaRECEIVERTEMPERATURE.csv"))
 	TempData$Date = as.Date(TempData$T_DATE)
	TempData$mon = month(TempData$T_DATE)
 	TempData$yr = year(TempData$T_DATE)
 	TempData$EID = 1:nrow(TempData)
 	attr(TempData,'projection') <- "LL"
  TempData = makePBS(TempData,polygon=F)
  TempData$Y = TempData$X
  TempData$X = TempData$LON_DD
  #finding the bounding from temp data and relating to bnam
  
  Xs = range(TempData[,c('X')])+c(-.1,.1)
  Ys = range(TempData[,c('Y')])+c(-.1,.1)
  MPA=expand.grid(Xs,Ys)
  MPA$X=MPA$Var1; MPA$Y=MPA$Var2
  MPA$POS=c(1,4,2,3)
  MPA = MPA[order(MPA$POS),]
  MPA$PID = 1
  plotPolys(MPA)
  
  hg = findPolys(anc$locsP,MPA)
  hg = subset(anc$locsP,EID %in% hg$EID)
  
  ####
  
  
  pp = aggregate(cbind(X,Y)~T_UID,data=TempData,FUN=mean)
  
  out = list()
  for(i in 1:nrow(pp)){
      n = pp[i,]
      n$hg=hg$EID[which.min(calcGCdist(n$X,n$Y,hg$X,hg$Y)$d)]
    out[[i]]=n
  }
  out = do.call(rbind,out)
  
TD = merge(TempData,out,by=c('T_UID','X','Y'))  

UID = unique(TD$hg)
kT = anc$bTs[which(anc$bTs[,1] %in% UID),]

  
		g = TD
 		g$id = paste(g$mon,g$yr,sep='--')
 		sd0 = aggregate(TEMP~Date+hg,data=g,FUN=sd)
 		sd1 = aggregate(TEMP~Date+hg,data=g,FUN=mean)
 		
 		sd2 = aggregate(TEMP~yr+mon+hg,data=g,FUN=mean)
 		sd2$dec_date = sd2$yr+sd2$mon/12
 		
 		#monthly for comps--lm(bnamT~TEMP,data=sd2)
 		sd2$bnamT = NA
 		for(i in 1:nrow(sd2)){
 		sd2$bnamT[i] = kT[which(kT[,1]==sd2$hg[i]),which(anc$timeS==sd2$dec_date[i])]
 		}
 		
 		sd1$mT = mave(sd1$TEMP,c(1,1,1))
 		sd1$decyr = decimal_date(sd1$Date)
		sd1 = sd1[order(sd1$decyr),]
 		
		pdf('Stanns.pdf')
		LobsterMap(27)
		addPoints(hg[,c('EID','X','Y')],col='black',pch=16)
		addPoints(subset(g, id %in% sds$id,select=c(X,Y,EID)),pch=16,col='red')
		
			for(i in 1:length(UID)){
		  plot(anc$timeS,kT[which(kT[,1]==UID[i]),2:ncol(kT)],type='p',xlim=c(2015,2019),pch=16,xlab='Year',ylab='Temperature')
		  with(subset(sd1,hg==UID[i]),points(decyr,TEMP,type='l',col='red'))
		  h = subset(sd2,hg==UID[i])
		  with(h,plot(TEMP,bnamT,pch=16,xlim=c(0,5),ylim=c(0,5)))
		  abline(lm(bnamT~TEMP,data=h),lwd=2)
		  abline(a=0,b=1,col='red',lwd=2)
		}
		
		
		dev.off()
		  