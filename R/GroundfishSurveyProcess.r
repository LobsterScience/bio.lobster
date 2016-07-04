#' @export
GroundfishSurveyProcess<-function(size.range=c(0,220),Sex = NULL, Strata=c(485,490,495),Years=1976:2014,bin.size=5,Lengths=F){

  nbins<-length(seq(size.range[1],size.range[2],bin.size))-1
  p<-list()
  p$init.files = c(loadfunctions( "groundfish", functionname="load.groundfish.environment.r"), loadfunctions('BIOsurvey')) 
  p$strat = Strata
  p$series = c('summer')# p$series =c('4vswcod');p$series =c('georges')
  p$years.to.estimate = Years
  p$species = 2550
  p$vessel.correction = T
  p$vessel.correction.fixed = 1.2
  p$length.based = T
  p$size.class= size.range
  if(!is.null(Sex)) {p$by.sex = T; p$sex = Sex}
  else{p$by.sex = F}
 # browser()
  p$functional.groups = F
  p$alpha = 0.05
  p<-make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)

 grfanal<-groundfish.analysis(DS='stratified.estimates.redo',p=p)
 index<-grfanal$n.yst
 se<-grfanal$n.yst.se

  LF=NULL
  if(Lengths){
    p$length.based = T
    LF<-matrix(NA,length(Years),nbins)
    for(i in 1:nbins){
      p$size.class= c(size.range[1]+bin.size*(i-1),size.range[1]+bin.size*i)
      bout<-groundfish.analysis(DS='stratified.estimates.redo',p=p)
      LF[,i]<-bout$n.yst
    }
  }

  list(index=index,se=se,LF=LF)

}
