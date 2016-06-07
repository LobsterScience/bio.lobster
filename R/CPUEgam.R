
CPUEgam<-function(data,mf){

	RLibrary("mgcv")
	model<-gam(mf,data=data)

	pred.data<-subset(data,!is.na(CPUE))
	tmp.data<-subset(data,SYEAR==2014&!is.na(CPUE))
	tmp.data$SYEAR<-2015
	tmp.data$DATE<-tmp.data$DATE+365
	pred.data<-rbind(pred.data,tmp.data)

	out<-predict( model, newdata=pred.data, type="response", se.fit=T ) 

	pred.data$predCPUE<-out$fit
	pred.data$predCPUE.SE<-out$se.fit

	out.data<-merge(data,pred.data,all=T)

	plot(CPUE~DATE,out.data,pch='.',xlim=c(min(DATE),max(DATE)+365))
	lines(predCPUE~DATE,out.data)


}

