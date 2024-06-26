#' @export

jackknife<-function(data,run=T){
	#From Smith CJFAs 1980
	start<-Sys.time()
	data<-na.omit(data)
	t<-sort(unique(data$time))
	
	catch<-with(data,tapply(catch,time,sum))	
	effort<-with(data,tapply(effort,time,sum))
	Cf<-catch/effort
	n<-with(data,tapply(effort,time,length))

	out.dat<-data.frame(time=t,n,catch,effort,cpue=Cf)
	
	if(run){
		for (i in 1:length(t)){
			Rj<-Cf[i]
			if(n[i]>1){
				Rj<-c()
				for (j in 1:n[i]){
					Rj[j]<-n[i]*Cf[i] - (n[i]-1)*(sum(data$catch[data$time==t[i]][-j])/sum(data$effort[data$time==t[i]][-j]))
				}
			
			out.dat$cpue.jack[i]<-mean(Rj)
			out.dat$cpue.var[i]<-1/(n[i]*(n[i]-1))*sum((Rj-mean(Rj))^2)
			out.dat$cpue.se[i] <- abs(out.dat$cpue.jack[i]-out.dat$cpue[i]) / (sqrt(out.dat$cpue.var[i]))
			}
		}
	}
	print(Sys.time()-start)
	return(out.dat)
	
}
	
	
	
	
	
	
	
	