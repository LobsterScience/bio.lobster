#LFA 37

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
require(devtools)
require(SpatialHub)
la()
p = list()
p$lfas = 36
 logsInSeason<-lobster.db('process.logs.unfiltered')

   
catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = 2014:2020
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		saveRDS(catchgrids,file=file.path(fd,paste('Figure3',yrs[i],'.rds')))
#		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")))
		LobsterMap('37',poly.lst=catchgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
#	    dev.off()
	}
