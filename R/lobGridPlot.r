#' @export
lobGridPlot <- function(Data,lvls,bcol="YlGnBu",border=1,FUN=mean,place=2,rev=F,cuts=F) {  

	require(PBSmapping)
	require(RColorBrewer)
#	if(FUN=='LU') {lu = function(x) length(unique(x)); FUN=lu}

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
	if(rev) cols = rev(cols)
	lvls<-c(lvls,max(lvls)*100)
	if(any(is.na(pdata$SID))) pdata = subset(pdata, !is.na(SID))
	pdata  <- makeProps(pdata, lvls, "col", cols) 
	l = lvls[-1]
	if(cuts) pdata$cuts = l[match(pdata$col,cols)]
	pdata$border<-border
	

	grid<-read.csv(file.path( project.datadirectory('bio.lobster'), "data","maps","GridPolys.csv"))
	if(all(Data$LFA=='36')) {
	    i = which(grid$PID=='37')
	  r = unique(grid$SID[i])
	    for(j in 1:length(r)){
	        k = subset(grid,SID==r[j])
	        k$SID=1
	        k = joinPolys(k,operation = 'UNION')
	        k$PID = 36
	        k$SID=r[j]
	        grid = subset(grid, SID %ni% r[j])
	        grid = as.data.frame(rbind(grid,k))
	       }
	   
	  grid$PID[i] = 36
	  
	}
	if(all(Data$LFA=='38')) {
	  i = which(grid$PID==37)
	  grid$PID[i] = 38
	  
	}
	list(grid=grid,pdata=pdata,lvls=lvls[-length(lvls)],col=cols)
	
}