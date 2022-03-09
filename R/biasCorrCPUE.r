#' @export

biasCorrCPUE <- function(data){
  #following  van Kempen GMP, van Vliet LJ (2000) Mean and variance of ratio estimators used in fluorescence ratio imaging. Cytometry 39:300???305
  start<-Sys.time()
  data<-na.omit(data)
  t<-sort(unique(data$time))
  
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