lobGridPlot <- function(Data,lvls,bcol="YlGnBu",border=1,FUN=mean,place=0) {  

	require(PBSmapping)
	require(RColorBrewer)
	
	names(Data)[1:3]<-c("LFA","GRID","Z")
	Data$LFA[Data$LFA=="31A"]<-311
	Data$LFA[Data$LFA=="31B"]<-312
	lfa<-unique(Data$LFA)
	Data$ID<-paste(Data$LFA,Data$GRID,sep='.')
	tmp<-with(Data,tapply(Z,ID,FUN))
	
	#browser()
	pdata  <- merge(subset(Data,!duplicated(ID),c("ID","LFA","GRID")), data.frame(ID=names(tmp),Z=tmp),all=T)
	pdata  <- with(pdata,data.frame(PID=as.numeric(LFA),SID=as.numeric(GRID),Z=Z))

	if(missing(lvls))lvls<-round(seq(min(pdata$Z),max(pdata$Z),l=9),place)
	cols   <- brewer.pal(length(lvls),bcol) 
	lvls<-c(lvls,max(lvls)*100)
	browser()
	pdata  <- makeProps(pdata, lvls, "col", cols) 
	pdata$border<-border
	

	grid<-read.csv(file.path( project.datadirectory('bio.lobster'), "data","maps","GridPolys.csv"))
	
	list(grid=grid,pdata=pdata,lvls=lvls[-length(lvls)],col=cols)
	
}