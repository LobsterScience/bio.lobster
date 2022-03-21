#' @export

biasCorrCPUE <- function(data,by.time=F,min.sample.size=15){
  #following  van Kempen GMP, van Vliet LJ (2000) Mean and variance of ratio estimators used in fluorescence ratio imaging. Cytometry 39:300???305
  start<-Sys.time()
  data<-na.omit(data)
  t<-aggregate(date~time,data=data,FUN=length)
  if(!by.time){
      c<-with(data,sum(catch))	
      e<-with(data,sum(effort))
      Cf<-c/e
      n<-with(data,length(catch))
      Cfp = Cf+(var(data$effort)/(mean(data$effort)^2) * (mean(data$catch)/mean(data$effort))) - (cov(data$effort, data$catch) / (mean(data$effort)^2))
      varCfp = 1/n*((var(data$catch)/(mean(data$effort)^2)) + ((mean(data$catch)^2 * var(data$effort))/(mean(data$effort)^4)) - ((2*mean(data$catch)*cov(data$effort,data$catch))/(mean(data$effort)^3)))
      lam = sqrt(varCfp)*5 #Chebyshev's inequality Tchebichef, P. (1867).Journal de Math Pures et Appl. 2. 12: 177???184.
      cis95 = c(Cfp-lam, Cfp+lam) 
      return(c(CPUE=Cf, unBCPUE = Cfp, unBVar = varCfp, l95=cis95[1],u95=cis95[2]))
      }
  if(by.time){
    t = t[which(t$date>min.sample.size),]
    outlist = list()
    for(i in 1:nrow(t)){
      da = subset(data,time==t[i,1])
      c<-with(da,sum(catch))	
      e<-with(da,sum(effort))
      Cf<-c/e
      n<-with(da,length(catch))
      Cfp = Cf+(var(da$effort)/(mean(da$effort)^2) * (mean(da$catch)/mean(da$effort))) - (cov(da$effort, da$catch) / (mean(da$effort)^2))
      varCfp = 1/n*((var(da$catch)/(mean(da$effort)^2)) + ((mean(da$catch)^2 * var(da$effort))/(mean(da$effort)^4)) - ((2*mean(da$catch)*cov(da$effort,da$catch))/(mean(da$effort)^3)))
      lam = sqrt(varCfp)*5 #Chebyshev's inequality Tchebichef, P. (1867).Journal de Math Pures et Appl. 2. 12: 177???184.
      cis95 = c(Cfp-lam, Cfp+lam) 
      outlist[[i]] = (c(t = t[i,1],CPUE=Cf, unBCPUE = Cfp, unBVar = varCfp, l95=cis95[1],u95=cis95[2]))
    }
    z = (do.call(rbind,outlist))
  }
}