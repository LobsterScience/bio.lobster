require(bio.lobster)

d = function(x=lengths,minTagSize=65,surveySample=200, returnRate=returnRatee, surveyM=0.1, tagLoss = tagLosse, tagM=tagMe, lifeHM = lifeHMe, daysAtLarge=150, annualMigrationRate=annualMigrationRatee,exploitationRate=exploitationRatee ){
      xx = sample(x, size=surveySample,replace = T)  
      xT = xx[which(xx>=minTagSize)]
      
      #assume same selectivity to traps and trawl and no molting between mark and recap
      n =length(xT)
      
      #tagging Mortality
      tM = rweibull(n=1,tagM$shape,tagM$beta)
      nTagM = n *(1-tM)
      
      #tagging loss
      tL = rweibull(n=1,tagLoss$shape,tagLoss$beta)
      nTagML = n *(1-tL)
      
      #survey mortality
      nTagMLS = nTagML *(1-surveyM)
      
      
      #natural mortality
      lifeHM = rweibull(n=1,lifeHM$shape,lifeHM$beta)
      nm = lifeHM * daysAtLarge/365
      nTagMLSM = nTagMLS - nTagMLS * (1-exp(-lhm))
      
      #migration
      annualMigrationRate = rweibull(n=1,annualMigrationRate$shape,annualMigrationRate$beta)
      mig = annualMigrationRate * daysAtLarge/365
      nTagMLSMMi = nTagMLSM - nTagMLSM * (1-exp(-lhm))
      
      #recapture
      exploitationRate = rweibull(n=1,exploitationRate$shape,exploitationRate$beta)
      nTagMLSMMi_C = nTagMLSMMi * (exploitationRate)
      
      #reporting
      returnRate = rweibull(n=1,returnRate$shape,returnRate$beta)
      return(c(nTagMLSMMi_C * returnRate,n) )
      }



###survey to fishery
a = lobster.db('survey')

b = subset(surveyMeasurements,LFA=='L38' & SPECCD_ID==2550)

lengths = b$FISH_LENGTH
#
tagLosse = findMoments(lo=.01,up=.2,dist='weibull')
tagMe = findMoments(lo=.01,up=.2,dist='weibull')
lifeHMe = findMoments(lo=.1,up=.2,dist='weibull')
annualMigrationRatee=findMoments(lo=.05,up=.4,dist='weibull')
exploitationRatee=findMoments(lo=.3,up=.6,dist='weibull')
returnRatee = findMoments(lo=.5,up=.8,dist='weibull')

#change tagging mortality to based on comeau 2003
tagMe = findMoments(lo=.20,up=.40,dist='weibull')

SM =  seq(0,.999, by=.02)
niter=1000
out=matrix(NA,nrow=length(SM), ncol=niter)
for(j in 1:(niter)){
for(i in 1:length(SM)){
           out[i,j] =d(surveyM = SM[i])
  }
}
o = as.data.frame(out)
rownames(o) =paste('X',SM,sep="")
colnames(o) = paste('X',1:1000,sep="")
o$Sur = rownames(o)
#tidyr::pivot_longer((o))
xo = tidyr::pivot_longer(data=o,cols=starts_with('X'), names_prefix = 'X')

xoA=aggregate(value~Sur,data=xo, FUN=function(x) quantile(x,probs=c(0.025,0.5,0.975)))
xoA$Survey_mortality = as.numeric(substr(xoA$Sur,2,nchar(xoA$Sur)))
xoA[,2] = xoA[,2]/177
x1 = xoA[1,2][,1]
x2 = xoA[,3][which.min(abs(x1-xoA[,2][,3]))]
ggplot(data=xoA)+geom_point(aes(x=Survey_mortality,y=value[,2])) + 
  geom_errorbar(aes(x=Survey_mortality,ymin=value[,1],ymax=value[,3])) + scale_x_continuous(n.breaks=10 ) +
  geom_hline(yintercept = x1,col='red') +
  geom_vline(xintercept = x2,col='red') +
  ylab('Recaptures (% of Marked') +
  title('Premoult Tagging')
  

