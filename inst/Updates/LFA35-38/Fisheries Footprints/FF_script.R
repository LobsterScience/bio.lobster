figdir = file.path("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38/Fisheries Footprints")
logs = lobster.db('process.logs')
p = bio.lobster::load.environment()
la()
assessment.year = 2022 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year




###LFA 36
logfoot = subset(logs,LFA==36)
p = bio.lobster::load.environment()
la()
require(devtools)

## Fishery Footprint - Landings
catchLevels = c(0,50000,100000,200000,300000,400000,500000,600000)
yrs = 2012:2022    ###CHECK YEARS
catchgrids.lst=list()
for(i in 1:length(yrs)){
  print(i)
  catchgrids.lst[[i]] = lobGridPlot(subset(logfoot,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
  pdf(file.path(figdir,paste0("FisheryFootprint36",yrs[i],".pdf")),5,5)
  LobsterMap('36',poly.lst=catchgrids.lst[[i]])
  text(x=c(-65.5,-65.5,-64.5),y=c(43.1,42.7,42.7),labels=c(34,40,41),col=rgb(0,0,0,0.8),cex=1.5)
  title(yrs[i],line=-3,cex.main=2,adj=0.3)
  SpatialHub::contLegend('bottomright',lvls=catchgrids.lst[[i]]$lvls/1000,Cont.data=catchgrids.lst[[i]],title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
  dev.off()
  #pdf2png(file.path(figdir,paste0("FisheryFootprint",yrs[i])))
}



##LFA 38

logfoot = subset(logs,LFA==38)
p = bio.lobster::load.environment()
la()
require(devtools)

## Fishery Footprint - Landingskg 
catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
yrs = 2014:2022    ###CHECK YEARS
catchgrids.lst=list()
for(i in 1:length(yrs)){
  print(i)
  catchgrids.lst[[i]] = lobGridPlot(subset(logfoot,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
  pdf(file.path(figdir,paste0("FisheryFootprint38",yrs[i],".pdf")),5,5)
  LobsterMap('38',poly.lst=catchgrids.lst[[i]])
  text(x=c(-65.5,-65.5,-64.5),y=c(43.1,42.7,42.7),labels=c(34,40,41),col=rgb(0,0,0,0.8),cex=1.5)
  title(yrs[i],line=-3,cex.main=2,adj=0)
  SpatialHub::contLegend('bottomright',lvls=catchgrids.lst[[i]]$lvls/1000,Cont.data=catchgrids.lst[[i]],title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
  dev.off()
  #pdf2png(file.path(figdir,paste0("FisheryFootprint",yrs[i])))
}
