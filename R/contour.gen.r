contour.gen<-function(contour.dat,lvls,Poly,col="YlGn",colorAdj){

	require(RColorBrewer)
	require(PBSmapping)

	CL <- contourLines(contour.dat,levels=lvls)
	CP <- convCP(CL)
	if(missing(Poly)) Cont.poly <- CP$PolySet
	else Cont.poly <- joinPolys(CP$PolySet,Poly)
	if(missing(colorAdj))color<-brewer.pal(length(lvls),col)
	else color<-brewer.pal(length(lvls)+colorAdj,col)[-colorAdj]
	Cont.data<- data.frame(PID=1:length(lvls),col=color,border=NA)

	list(Cont.poly=Cont.poly,Cont.data=Cont.data)	
}
