#### do logbooks and slips follow Benfords law (first two digits of reported weight) #######}

###
require(bio.lobster)
require(devtools)
require(bio.utilities)




gen_benford_empirical <- function(x,n) {
  #theoretical
  begin <- 10^(n - 1)
  ends <- 10^(n) - 1
  d <- begin:ends
  d[d < 0] <- d[d < 0]*(-1)
  prob <- log10(1 + 1/d)
  xx =data.frame(X=d,TheoProb=prob)
  
  #empirical
  d = trunc((10^((floor(log10(x))*-1) + n - 1))*x)
  d1 = (table(d))
  d1=data.frame(d1)
  d1$EmpProb = d1$Freq/sum(d1$Freq)
  names(d1) = c('X','EmpDensity','EmpProb')
  
  return(merge(xx,d1))
  }

x = lobster.db('process.logs.unfiltered')
s = lobster.db('slips')
s = subset(s,SPECIES_CODE==700)
s$WEIGHT_KG = s$SLIP_WEIGHT_LBS/2.204
s$SYEAR = year(s$DATE_LANDED)
s$SYEAR = ifelse(s$LFA %in% 33:38 & month(s$DATE_LANDED) %in% c(10,11,12),s$SYEAR+1,s$SYEAR)


#all LFAs all years

o = list()
m=0
u = unique(x$LFA)
for(i in 1:length(u)){
  g = subset(x,LFA==u[i])  
  h = subset(s,LFA==u[i])  
  y = unique(g$SYEAR)
  for(j in 1:length(y)){
    
    gg = subset(g,SYEAR==y[j])  
    hh = subset(h,SYEAR==y[j])  
    if(nrow(gg)>500 & nrow(hh)>200){
      m=m+1
      junk = gen_benford_empirical(gg$WEIGHT_LBS,n=1)
    sjunk = gen_benford_empirical(hh$SLIP_WEIGHT_LBS,n=1)
    
    junk$CTheoProb = cumsum(junk$TheoProb)
    junk$CEmpProb = cumsum(junk$EmpProb)
    
    sjunk$CTheoProb = cumsum(sjunk$TheoProb)
    sjunk$CEmpProb = cumsum(sjunk$EmpProb)
    
  #  plot(junk$X,junk$CTheoProb,col='red',type='l')
  #  lines(junk$X,junk$CEmpProb,col='blue',type='l')
    
  #  plot(sjunk$X,sjunk$CTheoProb,col='red',type='l')
  #  lines(sjunk$X,sjunk$CEmpProb,col='blue',type='l')
    
    junk$d = junk$EmpProb-junk$TheoProb
    
    sjunk$d = sjunk$EmpProb-sjunk$TheoProb
    o[[m]]=c(LFA=u[i],SYEAR=y[j],SlipD=sum(abs(sjunk$d)), LogD= sum(abs(junk$d)))
    }
}
}
ben = as.data.frame(do.call(rbind,o))
ben = toNums(ben,2:4)

require(ggplot2)

ggplot(subset(ben,SYEAR>2005)) +geom_line(aes(x=SYEAR,y=SlipD,color='red'))+ geom_line(aes(x=SYEAR,y=LogD,color='blue'))+facet_wrap(~LFA)

w = lobster.db('process.logs')
fw =aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=w,FUN=sum)
fw$CPUE = fw$WEIGHT_KG/fw$NUM_OF_TRAPS

fwb=merge(ben,fw)



ggplot(subset(fwb,SYEAR>2005)) +geom_line(aes(x=WEIGHT_KG,y=SlipD,color='red'))+ geom_line(aes(x=WEIGHT_KG,y=LogD,color='blue'))+facet_wrap(~LFA,scales='free_x')

