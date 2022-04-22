#' @export
lobLFAAttr <- function(Data,bcol="YlGnBu",border=1,lv=NULL) {  

	require(PBSmapping)
	require(RColorBrewer)
	names(Data)[1:2]<-c("LFA","Z")
	Data$LFA[Data$LFA=="31A"]<-311
	Data$LFA[Data$LFA=="31B"]<-312
	if(is.null(lv)) lv<-unique(Data$Z)
	Data$PID = as.numeric(Data$LFA)
	lv = lv[order(lv)]
	cols   <- brewer.pal(length(lv),bcol) 
	pdata = Data
	pdata$col  <- cols[match(Data$Z, lv) ]

		pdata$border<-border
	

	LL <-read.csv(file.path( project.datadirectory('bio.lobster'), "data","maps","LFAPolys.csv"))
	list(grid=LL,pdata=pdata,lvls=lv,col=cols,polys='LFA')
	
}