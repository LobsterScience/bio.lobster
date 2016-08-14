figure.habitat.associations <- function(data,p,out.dir='bio.groundfish',f.name,plot=T) {
	print('2014 data not included temp and sal not in database')
	options(stringsAsFactors=F)
out.d = out.s = out.t = list()
for(i in 1:(length(data)-2)) {

			survey.dat = Prepare.strata.data(data[[c(i,2)]])
			strata.dat = Prepare.strata.file(data[[c(i,1)]])

			yr= as.numeric(unique(substr(survey.dat$mission,4,7)))
			st = survey.dat[which(survey.dat$bottom.temperature> -5 | !is.na(survey.dat$bottom.temperature)),]
		out<-Association.test(st,strata.group=strata.dat,hydro='bottom.temperature',species='totno',nreps=1000)
		out.t[[i]]<-c(out[[6]],out[[7]],out[[8]],out[[9]],out[[10]],yr)
		print('t')
		st = survey.dat[which(survey.dat$bottom.salinity> -5 | !is.na(survey.dat$bottom.salinity)),]
		out<-Association.test(st,strata.group=strata.dat,hydro='bottom.salinity',species='totno',nreps=1000)
		out.s[[i]]<-c(out[[6]],out[[7]],out[[8]],out[[9]],out[[10]],yr)
		print('s')
		st = survey.dat[which(survey.dat$z> -5 | !is.na(survey.dat$z)),]
		out<-Association.test(st,strata.group=strata.dat,hydro='z',species='totno',nreps=1000)
		out.d[[i]]<-c(out[[6]],out[[7]],out[[8]],out[[9]],out[[10]],yr)
		print('d')
		print(yr)
		}
		ss = as.data.frame(rbind(do.call(rbind,out.s),do.call(rbind,out.t),do.call(rbind,out.d)))
		names(ss)<-c('VAL','P','YST','MIN.50','MED.50','MAX.50','MIN.95.HAB','MED.HAB','MAX.95.HAB','YEAR')
		ss$VAR<-rep(c('SALINITY','TEMPERATURE','DEPTH'),each=nrow(ss)/3)
		ss$SYM<-ifelse(ss$P<=0.05,19,1)
		for(i in c(1:10,12)) {
			ss[,i]<-as.numeric(ss[,i])
		}
		if(plot){
		pdf(file=file.path(project.datadirectory(out.dir),"figures",paste(f.name,'.pdf',sep="")))
		vars<-unique(ss$VAR)
		par(mfcol=c(3,1))
		par(mar=c(0,4,4,1))
		a3<-subset(ss,ss$VAR==vars[1])
		pps<-with(a3,data.frame(x=c(YEAR, rev(YEAR)),y=c(MIN.95.HAB,rev(MAX.95.HAB))))
		with(a3,plot(VAL~YEAR,type='n',ylab=unique(VAR),xlim=c(min(a3$YEAR),max(a3$YEAR)), ylim=c(min(a3$MIN.95.HAB), max(a3$MAX.95.HAB)), xaxt='n'))
		with(pps,polygon(x,y,col= "#C1C1C1",border=F))
		with(a3,lines(VAL~YEAR,lty=1,lwd=2))
		with(a3,lines(MED.50~YEAR,lwd=0.8,col='red'))
		with(a3,points(VAL~YEAR,pch=SYM, cex=1.5))
		with(a3,lines(MED.HAB~YEAR,col='blue'))
	par(mar=c(2,4,2,1))
		a3<-subset(ss,ss$VAR==vars[2])
		pps<-with(a3,data.frame(x=c(YEAR, rev(YEAR)),y=c(MIN.95.HAB,rev(MAX.95.HAB))))
		with(a3,plot(VAL~YEAR,type='n',ylab=unique(VAR),xlim=c(min(a3$YEAR),max(a3$YEAR)), ylim=c(min(a3$MIN.95.HAB), max(a3$MAX.95.HAB)), xaxt='n'))
		with(pps,polygon(x,y,col= "#C1C1C1",border=F))
		with(a3,lines(VAL~YEAR,lty=1,lwd=2))
		with(a3,lines(MED.50~YEAR,lwd=0.8,col='red'))
		with(a3,points(VAL~YEAR,pch=SYM, cex=1.5))
		with(a3,lines(MED.HAB~YEAR,col='blue'))

	par(mar=c(4,4,0,1))
		a3<-subset(ss,ss$VAR==vars[3])
		pps<-with(a3,data.frame(x=c(YEAR, rev(YEAR)),y=c(MIN.95.HAB,rev(MAX.95.HAB))))
		with(a3,plot(VAL~YEAR,type='n',ylab=unique(VAR),xlim=c(min(a3$YEAR),max(a3$YEAR)), ylim=c(min(a3$MIN.95.HAB), max(a3$MAX.95.HAB)), xlab='Year'))
		with(pps,polygon(x,y,col= "#C1C1C1",border=F))
		with(a3,lines(VAL~YEAR,lty=1,lwd=2))
		with(a3,lines(MED.50~YEAR,lwd=0.8,col='red'))
		with(a3,points(VAL~YEAR,pch=SYM, cex=1.5))
		with(a3,lines(MED.HAB~YEAR,col='blue'))
		dev.off()
		}
		return(ss)

		}

