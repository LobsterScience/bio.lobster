#' @export
suitabilityIndices <- function(x,niter=100){
  #suitability indices Tanaka and Chen 2015
  #histogram method Freedman and Diaconis 1981
  names(x) = c('ev','wt')
  ev = x$ev
  
  qs = quantile(ev,probs=c(0.001,0.25,0.75,0.999),na.rm=T)  
  i = which(ev<qs[1] | ev>qs[4])
  if(length(i)) x = x[-i,]
  ws = quantile(x$wt,probs=c(0.001,0.25,0.75,0.99),na.rm=T)   #windsorizing the wts
  i = which(x$wt>ws[4])
  wp = max(x$wt[-i])
  x$wt[i] = wp
  ev = x$ev
  
  bW = 2 * (qs[3]-qs[2]) * (length(ev)^(-1/3) ) #bin width
  bins = round((max(ev)-min(ev))/bW)
  if(bins>15) bins=15
  br = classInt::classIntervals(ev,n=bins,style='fisher')
  x$grp = cut(ev,breaks=br$brks,label=F,include.lowest = T)
  gt = aggregate(wt~grp,data=x,FUN=length)
  if(length(which(gt$wt<=5)>0)){
      x = subset(x, grp %ni% gt$grp[which(gt$wt<=5)])
      br = classInt::classIntervals(x$ev,n=bins,style='fisher')
      x$grp = cut(x$ev,breaks=br$brks,label=F,include.lowest = T)
      gt = aggregate(wt~grp,data=x,FUN=length)
      
             }
 bl = length(br$brks)-1
 
   si = bs = matrix(NA,nrow=bl,ncol=niter)
  for(i in 1:niter){
        xp = do.call(rbind,lapply(split(x,x$grp), function(x) x[sample(1:nrow(x),replace = T),]))
        xx = aggregate(wt~grp,data=xp,FUN=mean)
        xxx = xx[,2]
        bs[,i] =xxx
        si[,i] = (xxx-min(xxx)) / (max(xxx)-min(xxx))
  }
  x2=as.data.frame(t(apply(bs,1,function(x) quantile(x,c(0.025,0.5,0.975),na.rm = T))))
  x2$br = br$brks[-bl]
  names(x2)[1:3]= c('Q1','Q2','Q3')
  
  si2=as.data.frame(t(apply(si,1,function(x) quantile(x,c(0.025,0.5,0.975),na.rm = T))))
  si2$br = br$brks[-bl]
  si2$n = gt[,2]
  names(si2)[1:3]= c('Q1','Q2','Q3')
  return(list(data=x2,si=si2))
  }