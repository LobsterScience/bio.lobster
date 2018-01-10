#' @export
simBubPlot = function(rlist,fn='',...){

	p = rlist$plist[[1]]
	lfas  = names(rlist[[1]])
	bins = c(p$lens[1]-2.5,p$lens-2.5)
	yrs = seq(p$timestep,p$nt*p$timestep,p$timestep)/365
	
	for(i in 1:length(rlist[[1]])){
		
		y = round(rlist$mlist[[i]]$finalPop)
		x = round(rlist$flist[[i]]$finalPop)
		z = round(rlist$flist[[i]]$finalBerried)
		BubblePlotCLF(list(y),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'males',fn),prop=F,LS=p$LS,inch=0.2,bg=rgb(0,0,1,0.1),...)
		BubblePlotCLF(list(x),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'females',fn),prop=F,LS=p$LS,inch=0.2,bg=rgb(1,0,0,0.1),...)
		BubblePlotCLF(list(z),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'berried',fn),prop=F,LS=p$LS,inch=0.2,bg=rgb(1,0,1,0.1),...)
	}

}


