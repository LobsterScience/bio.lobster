#' CPUEgam
#' @param data is the data frame for fitting a generalized additive catch rate model. Data frame must contain columns of CPUE, SYEAR and DATE
#' @param mf is the gam model to be fitted
#' @author Brad Hubley
#' @return returns a plot of CPUE data by date along with the predicted line as well as a data.frame with raw data and predicted data
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
	return(out.data)

}

