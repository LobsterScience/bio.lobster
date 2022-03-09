#' @export 

blockedBootstrap <- function(data,block.center=14,block.range=7,niter=1000,return.samples=F){
  
  start<-Sys.time()
  data<-na.omit(data)
  t<-sort(unique(data$time))
  
  catch<-with(data,tapply(catch,time,sum))	
  effort<-with(data,tapply(effort,time,sum))
  Cf<-catch/effort
  n<-with(data,tapply(effort,time,length))
  sWin = function(x=block.center,y=block.range) sample((x-y):(x+y),1)
  outdata = list()
  cpues = c()
  for(i in 1:niter){
    w = sWin()
    data$b = floor(data$time/w)*w+1
    data = data[order(data$b),]
    b = aggregate(time~b,data=data,FUN=length)
    ll = list()
    for(j in 1:nrow(b)){
        k = which(data$b==b[j,'b'])
        if(length(k)>1) {
          ll[[j]] = data[sample(x=k,replace=T),]
        } else {
          ll[[j]] = data[k,]
          }
    }
    ss = do.call(rbind,ll)
    outdata[[i]] = ss
    cpues = c(cpues,sum(ss$catch)/sum(ss$effort))
  }
  print(Sys.time()-start)
  outlist = list()
  outlist = c(Mean = mean(cpues),median=median(cpues),sd=sd(cpues),l95=as.numeric(quantile(cpues,.025)),u95=as.numeric(quantile(cpues,.975)))
  if(return.samples) outlist = list(outlist,outdata)
  return(outlist)
  
}