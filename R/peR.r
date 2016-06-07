peR <- function(model, input, priors, years, pe, n = 80000, burn = 40000, thin = 10, graphic="R",run=T,TAC=100,plot=1,full.ests=F,proj.from="BUGS",lab='',g2,gR2,debug=F,dtable=T,path='',wd=''){
	
	y1<-years
	source("_Rfunctions/delayBUGS.r")
	source("_Rfunctions/projections.r")
	if(run){
		
		out<-list(NULL)
		
		input.dat<-data.frame(Year=years,input[!names(input)%in%c("NY","NC","C.p")])
		
		
		for(i in 1:length(pe)){
				
			yrs<-years[1]:pe[i]
			input.lst<-as.list(subset(input.dat,Year%in%yrs,-1))
			input.lst$NY <- length(input.lst$C)
			
			
			if(!missing(g2))input.lst$g[input.lst$NY]<-g2[input.lst$NY]
			if(!missing(gR2))input.lst$gR[input.lst$NY]<-gR2[input.lst$NY]
		
			# Run model		
			#browser()
 			DD<-delayBUGS(model, input.lst, priors, yrs, n = n, burn = burn, thin = thin,debug=debug,pe=T,parameters=c(names(priors),'K','P','B','R','mu'),wd=wd)
 			DD<-projections(DD,C.p = c(0,input.lst$C[input.lst$NY]))

 			#browser()
			# Save medians
			out[[i]]<-list(DD$median,DD$sims.list$B.p[,2])
		}
	dump('out',paste(model,lab,'peOutList.R',sep=''))
	}
	
	if(!run)source(paste(model,lab,'peOutList.R',sep=''))
	
	if(1%in%plot){
		#browser()
		if(length(pe)!=length(out))out<-out[1:length(pe)]		
		n <- length(out)
		if(proj.from=="BUGS")preds <- sapply(1:length(out), function(i){out[[i]][[1]]$B.p[2]})
		#if(proj.from=="R")preds <- sapply(1:length(out), function(i){out[[i]][[1]]$pB})
		ests <- sapply(1:length(out), function(i){rev(out[[i]][[1]]$B)[1]})
		years<-pe
		 
		if(graphic=='pdf')pdf(paste(path,"pe1",model,lab,".pdf",sep=''), width=6.5, height=5)
		if(graphic=='R')windows(6.5,5)
		
		par(omi=c(0.1,0.1,0.1,0.1))
		plot(years+1, preds/1000, type = 'b', ylim = c(0, max(c(preds,ests))*0.0011), xlim=c(min(years)-1,max(years)+2), las=1, mgp = c(1,0.5,0), tcl=-0.3, pch=16, ylab="", xlab="")
		points(years, ests/1000, type = 'b', pch = 1, lty=1)
		points(years, rev(out[[1]]$B[(length(out[[1]]$B)-n+1):length(out[[1]]$B)])/1000, pch=3, cex=0.8)
		axis(4, lab = F, tcl=-0.3)
		mtext("Fully recruited biomass ('000 t)", 2, 2.5, cex=1.25)
		legend('topright', c("prediction", "estimate", "estimate (full)"), pch = c(16, 1, 3), bty = "n")
		if(graphic=='pdf')dev.off()
	}
	
	
	if(2%in%plot){
		if(length(pe)!=length(out))out<-out[1:length(pe)]		
		N <- length(out)
		if(proj.from=="BUGS")preds <- sapply(1:N, function(i){out[[i]][[1]]$B.p[2]})
		#if(proj.from=="R")preds <- sapply(1:N, function(i){out[[i]][[1]]$pB})
		ests <- sapply(1:N, function(i){rev(out[[i]][[1]]$B)[1]})
		Biomass<-sapply(1:length(out),function(i){out[[i]][[1]]$B[(length(out[[1]]$B)-length(years)+1):length(out[[i]][[1]]$B)]})
		 
		if(graphic=='pdf')pdf(paste(path,"pe1",model,lab,".pdf",sep=''), width=6.5, height=5)
		if(graphic=='R')windows(6.5,5)

		par(omi=c(0.1,0.1,0.1,0.1))
		plot(years,years, type = 'n', ylim = c(0, (max(c(ests,preds))+min(c(ests,preds)))/1000), xlim=c(min(years)-1,max(years)+2), las=1, mgp = c(1,0.5,0), tcl=-0.3, pch=16, ylab="", xlab="")
		for(i in 1:N){
			points(years[1:length(Biomass[[i]])], Biomass[[i]]/1000, type = 'b', pch = 1, lty=1)
			points(c(pe[i],pe[i]+1), c(ests[i],preds[i])/1000, pch=3, cex=0.8,lty=3, type = 'b')
		}
		axis(4, lab = F, tcl=-0.3)
		mtext("Fully recruited biomass ('000 t)", 2, 2.5, cex=1.25)
		if(graphic=='pdf')dev.off()
	}
	
	if(3%in%plot){
		if(length(pe)!=length(out))out<-out[1:length(pe)]		
		n <- length(out)
		if(proj.from=="BUGS")preds <- sapply(1:length(out), function(i){out[[i]][[1]]$B.p[2]})
		#if(proj.from=="R")preds <- sapply(1:length(out), function(i){out[[i]][[1]]$pB})
		ests <- sapply(1:length(out), function(i){rev(out[[i]][[1]]$B)[1]})
		years<-pe
		ymax<-max(c(preds,ests,sapply(1:length(out),function(i){quantile(out[[i]][[2]],0.9)})))*0.0011
		 
		#browser()
		if(graphic=='pdf')pdf(paste(path,"pe1",model,lab,".pdf",sep=''), width=6.5, height=5)
		if(graphic=='R')windows(6.5,5)
		
		par(omi=c(0.1,0.1,0.1,0.1))
		plot(years+1, preds/1000, ylim = c(0, ymax), xlim=c(min(years),max(years)+2), las=1, mgp = c(1,0.5,0), tcl=-0.3, pch=16, ylab="", xlab="")
		for(i in 1:length(out)){
			pB<-out[[i]][[2]]/1000
			pB.box<-pB[pB>quantile(pB,0.1)&pB<quantile(pB,0.9)]
			boxplot(pB.box,add=T,at=years[i]+1,axes=F,range=0,lty=1)
		}
		points(years[-length(years)], ests[-length(years)]/1000, pch = 16, col='red',cex=1.2)
		#points(years, rev(out[[1]]$B[(length(out[[1]]$B)-n+1):length(out[[1]]$B)])/1000, pch=3, cex=0.8)
		axis(4, lab = F, tcl=-0.3)
		mtext("Fully recruited biomass (kt)", 2, 2.5, cex=1.25)
		#legend('topright', c("prediction", "estimate", "estimate (full)"), pch = c(16, 1, 3), bty = "n")
		if(graphic=='pdf')dev.off()
	}
	if(dtable){
		mu<-c()
		B.change<-c()
		p.mu<-c()
		p.B.change<-c()
		#browser()
		catch=rev(input$C[y1%in%pe])[-1]
		for(i in 1:(length(pe)-1)){
			mu[i]<-out[[i]][[1]]$mu[length(out[[i]][[1]]$mu)]
			B.change[i]<-(out[[i]][[1]]$B[length(out[[i]][[1]]$B)]/out[[i]][[1]]$B[length(out[[i]][[1]]$B)-1]-1)*100
			p.mu[i]<-out[[i+1]][[1]]$mu.p[2]
			p.B.change[i]<-out[[i+1]][[1]]$B.change[2]
		}
		tab2<-data.frame(Year=pe[-length(pe)],Catch=catch,p.mu=p.mu,p.B.change=p.B.change,mu=mu,B.change=B.change)
	}
		
		
		
#	cbind(data.frame(Catch=model.in$Catch,mu=model.out$median$mu.p,B.change=model.out$median$B.change),p.decline=sapply(1:model.in$NC,function(x){mean(model.out$sims.list$B[,model.in$NY]>model.out$sims.list$B.nextyear.1[,x])})
	
	if(sum(plot)>0){
		if(full.ests)ests<-rev(out[[1]]$B)[1:length(pe)]
		resids<-rev(preds[-1]-ests[-length(ests)])
		prec<-rev(preds[-1]/ests[-length(ests)])
		print(paste("Mean over-projection",round(mean(prec[prec>1]),2),"times estimate"))
		print(paste("Mean under-projection",round(mean(prec[prec<1]),2),"times estimate"))
		print(paste("Mean projection",round(mean(prec),2),"times estimate"))
		print(paste("Mean residual =",round(mean(abs(resids))/mean(ests)*100,2),"%"))
		tab1<-data.frame(Year=rev(pe)[-1], Residual=resids, Proportion=prec)
	}
	if(sum(plot)>0)list(tab1,tab2)
}