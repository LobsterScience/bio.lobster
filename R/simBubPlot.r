#' @export
simBubPlot = function(rlist,fn='',...){

	p = rlist$plist[[1]]
	lfas  = names(rlist[[1]])
	bins = c(p$lens,max(p$lens)+5)
	yrs = seq(p$timestep,p$nt*p$timestep,p$timestep)/365
	
	for(i in 1:length(rlist[[1]])){
		
		y = round(rlist$mlist[[i]]$finalPop)
		x = round(rlist$flist[[i]]$finalPop)
		z = round(rlist$flist[[i]]$finalBerried)
		e = round(rlist$flist[[i]]$totalEggs/1000)
		r = round(rlist$flist[[i]]$totalRemovals + rlist$mlist[[i]]$totalRemovals)
		m = round(rlist$flist[[i]]$totalMolts + rlist$mlist[[i]]$totalMolts)
		print(i)
		#browser()
		BubblePlotCLF(list(y),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'males',fn),prop=F,LS=p$LS,window=p$window,inch=0.2,bg=rgb(0,0,1,0.1),label='males',...)
		BubblePlotCLF(list(x),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'females',fn),prop=F,LS=p$LS,window=p$window,inch=0.2,bg=rgb(1,0,0,0.1),label='females',...)
		BubblePlotCLF(list(z),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'berried',fn),prop=F,LS=p$LS,window=p$window,inch=0.2,bg=rgb(1,0,1,0.1),label='berried',...)
		BubblePlotCLF(list(e),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'eggs',fn),prop=F,LS=p$LS,window=p$window,inch=0.2,bg=rgb(0,0.9,0.3,0.1),label='eggs',...)
		BubblePlotCLF(list(r),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'removals',fn),prop=F,LS=p$LS,window=p$window,inch=0.2,bg=rgb(1,0.5,0,0.1),label='removals',...)
		BubblePlotCLF(list(m),bins=bins,yrs=yrs,log.trans=T,filen=paste0(lfas[i],'molts',fn),prop=F,LS=p$LS,window=p$window,inch=0.2,bg=rgb(0.5,1,0.3,0.1),label='molts',...)
	}

}


	