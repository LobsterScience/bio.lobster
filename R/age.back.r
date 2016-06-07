# source("Y:\\Assessment\\2010\\r\\fn\\age.back.r")

age.back<-function(ht,LVB=list(linf=145.4,k=0.38,t0=1),age.dif=-1,plot=F){

	
	a <- LVB$t0-log(1-ht/LVB$linf)/LVB$k

	a2<-a+age.dif 

	ht2 <- LVB$linf*(1-exp(-LVB$k*(a2-LVB$t0)))
	
	sug <-(ceiling(ht2/5)*5)
	
	if(plot){
		windows()
		plot(a2,ht2,xlab='age (years)',ylab='shell height (mm)',type='b',pch=16)
		points(a2,sug,pch='-')
	}
		

	list(input.age=a,input.height=ht,result.age=a2,result.height=ht2,suggested.bin=sug)
	
}                   