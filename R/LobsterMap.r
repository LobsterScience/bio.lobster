#' LobsterMap
#' @param area = 'custom' where xlim & ylim are specified or select from area list below
#' @param mapRes = coastline detail ('LR' = low resolution, 'MR' = medium resolution, 'HR' = high resolution, 'UR' = ultra resolution)
#' @param title = plot title
#' @param boundaries = for ploting specific management boundaries
#' @param isobath = plots bathymetry lines for specified depths from topex data 
#' @param bathcol = isobath line color, default is transparent blue
#' @param topolines = plots topographic lines for specified elevations from topex data 
#' @param bathcol = topolines line color, default is transparent brown
#' @param points.lst = points to overlay on map in PBSmapping format - list with 2 elements: 1st element is eventSet (EID, POS, X, Y), 2nd element is eventData (EID, pch, col, etc.) 
#' @param lines.lst = lines to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, lty, col, etc.) 
#' @param poly.lst = polygons to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, border, col, etc.) 
#' @param contours = plots overlaping polygons as contours (same format as poly.lst)
#' @param image.lst = image to overlay on map - list with 3 elements (x, y, z), 'bathymetry' produces image from bathymetry data 
#' @param color.fun = color function for image
#' @param zlim = zlim for image
#' @param grid = size of grid in degrees, default is no grid
#' @param stippling = adds stippling to land (purely for visual effect)
#' @param lol = adds water colored border to coastline (purely for visual effect)
#' @author Brad Hubley
#' @examples
#' LobsterMap(area='34')
#' catchgrids <-lobGridPlot(subset(logsInSeason,SYEAR==2007,c("LFA","GRID_NUM","WEIGHT_KG")),lvls=c(100,50000,100000,200000,400000,600000,800000,1000000),FUN=sum,border=NA)
#' LobsterMap('all',poly.lst=catchgrids[1:2],title="2007 Lobster Catch")
#' ContLegend("bottomright",lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (t)",inset=0.02,cex=0.8,bg='white')
#' @export
LobsterMap<-function(area='custom',ylim=c(40,52),xlim=c(-74,-47),save=F, fname = 'LobsterMap',mapRes='HR',land.col='wheat',title='',nafo=NULL,boundaries='LFAs',bathy.source='topex',
	isobaths=seq(100,1000,100),bathcol=rgb(0,0,1,0.1),topolines=NULL,topocol=rgb(0.8,0.5,0,0.2),points.lst=NULL,pt.cex=1,lines.lst=NULL,poly.lst=NULL,contours=NULL,image.lst=NULL,color.fun=tim.colors,zlim,grid=NULL,stippling=F,lol=F,labels='lfa',labcex=1.5,LT=T,plot.rivers=T,
	addGridGroups=F, addSummerStrata=F,polylstend=F, addsubareas=F,subsetSummerStrata=NULL, addGeorgesStrata=F, addAmericanStrata=F,addGrids=T,land.only=F,grid.labcex=1,special.labels=NULL,...){

options(stringsAsFactors=F)		
	require(PBSmapping)|| stop("Install PBSmapping Package")
	require(fields)|| stop("Install fields Package")
	# Custom area
	if(area=='custom')	{ ylim=ylim; 			xlim=xlim			}
	
	## Area List
	if(area=='all')		{ ylim=c(41.1,48); 		xlim=c(-67.8,-57.8)	}
	if(area=='27-38')	{ ylim=c(42.5,48); 		xlim=c(-67.4,-57.8)	}
	if(area=='27-33')	{ ylim=c(42.5,48); 		xlim=c(-66.6,-57.8)	}
	if(area=='27-32')	{ ylim=c(43.5,48); 		xlim=c(-63.6,-57.8)	}
  if(area=='27-32.Crop')	{ ylim=c(44,47.6); 		xlim=c(-63.6,-58.5)	}
	if(area=='34-38')	{ ylim=c(42.5,46); 		xlim=c(-67.8,-63.5)	}
	if(area=='35-36')	{ ylim=c(44.5,46);	 	xlim=c(-67.2,-63.2)	}
	if(area=='west')	{ ylim=c(42.5,46); 		xlim=c(-67.8,-64)	}
	if(area=='27')		{ ylim=c(44.9,47.9); 	xlim=c(-61,-57.8)	}
  if(area=='27.Crop')		{ ylim=c(45.4,47.6); 	xlim=c(-61.1,-58.8)	}
	if(area=='28')		{ ylim=c(45.3,46);	 	xlim=c(-61.6,-60.3)	}
	if(area=='29')		{ ylim=c(45.3,46); 		xlim=c(-61.6,-60.3)	}
	if(area=='30')		{ ylim=c(44.6,45.9); 	xlim=c(-60.8,-59.6)	}
  if(area=='30.Crop')		{ ylim=c(45.2,46); 	xlim=c(-61,-59.7)	}
	if(area=='31A')		{ ylim=c(44.4,45.7); 	xlim=c(-61.8,-60)	}
  if(area=='31A.Crop')		{ ylim=c(44.8,45.8); 	xlim=c(-61.6,-60.2)	}
  if(area=='31B')		{ ylim=c(44.1,45.3); 	xlim=c(-62.2,-60.5)	}
  if(area=='31B.Crop')		{ ylim=c(44.6,45.6); 	xlim=c(-62.35,-60.5)	}
	if(area=='32')		{ ylim=c(43.8,45);	 	xlim=c(-63.5,-61.5)	}
  if(area=='32.Crop')		{ ylim=c(44.2,45.2);	 	xlim=c(-63.5,-61.5)	}
	if(area=='33')		{ ylim=c(42.5,44.8); 	xlim=c(-65.8,-62.2)	}
	if(area=='34')		{ ylim=c(42.5,45);	 	xlim=c(-67.8,-65)	}
	if(area=='35')		{ ylim=c(44.5,46);	 	xlim=c(-66,-63.2)	}
	if(area=='36')		{ ylim=c(44.5,45.7); 	xlim=c(-67.2,-65)	}
	if(area=='37')		{ ylim=c(44.5,45.2);	xlim=c(-67.2,-66) }
	if(area=='38')		{ ylim=c(43.8,45);		xlim=c(-67.5,-66.2) }
	if(area=='40')		{ ylim=c(42.25,43);		xlim=c(-66.5,-65.25)}
	if(area=='41')		{ ylim=c(41.1,44); 		xlim=c(-68,-63.5)	}
	if(area=='SWN')		{ ylim=c(42.5,45); 		xlim=c(-67.8,-62.2)	}
	if(area=='BoF')		{ ylim=c(43.75,46); 	xlim=c(-67.8,-63.2)	}
	if(area=='33-35')	{ ylim=c(42.5,46); 		xlim=c(-67.8,-63.2)	}
  if(area=='33-34')	{ ylim=c(42.5,45); 		xlim=c(-67.5,-62.2)	}	
  if(area=='ENS')	  { ylim=c(43.8,45.7); 	xlim=c(-63.5,-59.8)	}
  
	coast<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","gshhs",paste0("shoreline",mapRes,".csv")))
	rivers<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","gshhs",paste0("rivers",mapRes,".csv")))
	attr(coast,"projection")<-"LL"
	if(land.only){		addPolys(coast,col=land.col,...); return(1)}	

		# comment


if(save) {
	print('pdf and png saved')
	pdf(file.path(project.figuredirectory("bio.lobster"),paste0(fname,'.pdf')), width=6.5, height=5)
	}
	#par(...)
	plotMap(coast,xlim=xlim,ylim=ylim,border=NA,...)
	#addLines(rivers)
	
	if(lol)addPolys(coast,border=bathcol,lwd=6)
	
	# Image
	if(!is.null(image.lst)){
		if(missing(zlim))zlim<-range(image.lst$z,na.rm=T)
		image(image.lst,add=T,col=color.fun(100),zlim=zlim)
	}

	# plot polygons
	if(!is.null(contours)){
		contours[[2]]<-subset(contours[[2]],PID%in%contours[[1]]$PID)
		junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))
		for(i in unique(contours[[2]]$PID)){
			addPolys(joinPolys(subset(contours[[1]],PID==i),junk,operation="DIFF"),polyProps=contours[[2]])
		}
	}
	if(!is.null(poly.lst)){
	  
		addPolys(poly.lst[[1]],polyProps=poly.lst[[2]])

		if('density'%in%names(poly.lst[[2]]) & any(!is.na(poly.lst[[2]]$density))){
			x=subset(poly.lst[[2]],!is.na(density))
			addPolys(subset(poly.lst[[1]],SID==x$SID),density=x$density,angle=x$angle,col=x$linecol,border=x$border)
		}
	}
	
	
	
	# NAFO
	if(!is.null(nafo)){
		
        nafo.xy<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","nafo.csv"))
        if(nafo=='all')nafo<-unique(nafo.xy$label)
        nafo.sel<-subset(nafo.xy,label%in%nafo)
        nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
        nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"
        
		addPolys(nafo.xy,border='grey',col=NULL)
		addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2)
	}
	
	
#groundfish survey summer strata	
	if(addSummerStrata) {
			  a = file.path( project.datadirectory("bio.lobster"), "data","maps","summer_strata_labels.csv")
			  a = read.csv(a,header=T)
			  names(a)[4] <- 'label'
			  b = file.path( project.datadirectory("bio.lobster"), "data","maps","summerstrata.csv")
			  b = read.csv(b)
			  names(b) <- c('PID','X','Y')
			  
		if(!is.null(subsetSummerStrata)) {
			  b = b[which(b$PID %in% c(subsetSummerStrata)),]
				}
			  b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
			  attr(b,'projection') <- "LL"
			  addPolys(b,lty=1,border='blue',col=adjustcolor('white',alpha.f=1))
			 # addLabels(a,cex=0.6)
			}
    if(addGeorgesStrata) {
			  require('bio.polygons')
			  #b = file.path(project.datadirectory('bio.polygons'),'data','Science','PED','GeorgesBankStrata.rdata')
			  b = file.path( project.datadirectory("bio.lobster"), "data","maps",'GeorgesBankStrata.rdata')
			  load(b)
				  addPolys(out,lty=1,border='red',col=adjustcolor('white',alpha.f=1))
				
				}
    if(addAmericanStrata) {
			   b = importShapefile(file.path( project.datadirectory("bio.lobster"), "data","maps","BTS_Strata"), readDBF=F)
			   attr(b,'projection') <- "LL"
			  addPolys(b,lty=1,border='blue',col=adjustcolor('white',alpha.f=0))
				}

  # 
    if(addGridGroups){
        io = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA_33_TO_ 38_GRID_GROUPS_NAD83.csv"))
        addPolys(io)
    }
  # Boundries
	
	if(boundaries=='LFAs'){
		
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		LFA41<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
		subareas<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA2733subareas.csv"))

		if(area=='31a')area<-311
		if(area=='31b')area<-312
		if(addsubareas)addPolys(subset(subareas,SID==1),lty=3)
	#	if(area=='41') addPolys(LFA41,border='blue')
		lfa<-as.numeric(area)
			
		if(lfa%in%LFAgrid$PID){
			if(!is.na(lfa)){
				grids<-subset(LFAgrid,PID==lfa)
				#browser()
				if(addGrids=='lfaonly')addPolys(grids,border=rgb(0,0,0,0.2),col=NULL)
				if(any(grep('grid',labels))){
					grids$label<-grids$SID
	        		
					grids.dat<-merge(calcCentroid(grids),grids[c("PID","SID","label")])

				addLabels(subset(grids.dat,!duplicated(label)),col=rgb(0.5,0.5,0.5,0.8),cex=1)
				}
			}
			else {
				addPolys(LFAgrid,border=rgb(0,0,0,0.2),col=NULL)
				LFAgrid$label<-LFAgrid$SID
	        	grids.dat<-merge(calcCentroid(LFAgrid),LFAgrid[c("PID","SID","label")])

			}
		}
			#browser()
		addPolys(LFAs, lwd=2,col=NULL)
		if('lfa'%in% labels){
			LFAgrid$label<-LFAgrid$PID
			LFAgrid$label[LFAgrid$label==311]<-'31A'
			LFAgrid$label[LFAgrid$label==312]<-'31B'
    		LFAgrid.dat<-merge(calcCentroid(LFAgrid,1),LFAgrid[c("PID","label")])
    		LFAgrid.dat <- subset(LFAgrid.dat,!duplicated(label))
    		il = which(LFAgrid.dat$label==36)
    		LFAgrid.dat$Y[il] = 45.02
    		il = which(LFAgrid.dat$label==35)
			LFAgrid.dat$Y[il] = 45.23
			
					
    		LFAgrid.dat = as.data.frame(rbind(LFAgrid.dat,c(41,-66,41.9,41)))	#add in lfa41 label		
			#addLabels(subset(LFAgrid.dat,!duplicated(label)),col=rgb(0,0,0,0.8),cex=labcex)
		}

	}
	if(addGrids==T){
			LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		addPolys(LFAgrid,border=rgb(0,0,0,0.2),col=NULL)
		addLines(LFAs, lwd=2) #Overlays Thicker lfa lines over grid lines 
		
	}
	if(boundaries=='scallop'){
		SFA<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","SFA.csv"))
		addLines(SFA)
		SPA<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","SPA.csv"))
		addPolys(SPA,col=NULL)
	}
	
	# Bathymetry
	sn<-ifelse(sum(isobaths/10)==sum(round(isobaths/10)),"",1)
	
	if(!is.null(isobaths)){
	  bath.lst<-list()
	  for(i in unique(ceiling(isobaths/1000))){
	    load(file.path( project.datadirectory("bio.lobster"), "data","maps", bathy.source, paste0("bathy",sn,"Poly",i,".rdata")))
	    bath.lst[[i]]<-bathy.poly
	  }
	  bathy.poly<-do.call(rbind,bath.lst)
	  #browser()
	  bathy.poly<-subset(bathy.poly,Z%in%isobaths)
	  attr(bathy.poly,"projection") <- "LL"
	  addLines(bathy.poly,polyProps=data.frame(PID=unique(bathy.poly$PID),col=bathcol))
	  #browser()
	}
	
	#EEZ<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","EEZ.csv"))
	#addLines(EEZ,lty=4,lwd=2)
	
	
	if(area=="33") { #Changes label positions adjacent to LFA 33  for map
	  LFAgrid.dat$Y[LFAgrid.dat$PID %in% c(41, 40)] = 42.65 
	  LFAgrid.dat$Y[LFAgrid.dat$PID=="34"] = 43.1 
	  LFAgrid.dat$X[LFAgrid.dat$PID %in% c(34, 40)] = -65.5
	  LFAgrid.dat$X[LFAgrid.dat$PID=="41"] = -64
	}
	
	# plots land
	if(LT){
		addPolys(coast,col=land.col,...)
		if(plot.rivers)addLines(rivers,...)
	
	  # add LFA labels
	 # if (area %in% c('27-38', 'all')) {labcex=0.9} #can copy / modify this line if you want label size to be associated with area chosen
		if(area=='27-32') { #Can adjust size and colour of LFA labels based on "area of interest"
		  if(boundaries=='LFAs') if('lfa'%in% labels)	addLabels(subset(LFAgrid.dat,!duplicated(label)),col='white',cex=1.3*(labcex))
		  if(boundaries=='LFAs') if('lfa'%in% labels)	addLabels(subset(LFAgrid.dat,!duplicated(label)),col='firebrick3',cex=(labcex))
     } else {
	  if(boundaries=='LFAs') if('lfa'%in% labels)	addLabels(subset(LFAgrid.dat,!duplicated(label)),col=rgb(0,0,0,0.8),cex=labcex)
     }
	}
	  
	if(stippling)addStipples (coast, pch='.')
	
	
	# Topography
	
		if(!is.null(topolines)){
			topo.lst<-list()
			for(i in unique(ceiling(topolines/1000))){
	 			load(file.path( project.datadirectory("bio.lobster"), "data", "maps","topex",paste0("topoPoly",i,".rdata")))
	 			topo.lst[[i]]<-topo.poly
	 		}
 			topo.poly<-do.call(rbind,topo.lst)
 			topo.poly<-subset(topo.poly,Z%in%topolines)
			attr(topo.poly,"projection") <- "LL"
			addLines(topo.poly,polyProps=data.frame(PID=unique(topo.poly$PID),col=topocol))
		}
	


	# plot points
	if(!is.null(points.lst)){
		addPoints(points.lst[[1]],polyProps=points.lst[[2]],cex=pt.cex)
	}

	# plot lines
	if(!is.null(lines.lst)){
		addLines(lines.lst[[1]],polyProps=lines.lst[[2]])
	}
	
	# add grid lines
	if(!is.null(grid)){
		x<-seq(floor(xlim[1]),ceiling(xlim[2]),grid)
		y<-seq(floor(ylim[1]),ceiling(ylim[2]),grid)
		gridlines<-makeGrid(x,y,byrow=TRUE,addSID=TRUE,projection="LL",zone=NULL)
		addLines(gridlines,col='grey80',lwd=1)
	}
	if('grid'%in%labels) {
	  LFAgrid$label<-LFAgrid$SID
	  grids.dat<-merge(calcCentroid(LFAgrid),LFAgrid[c("PID","SID","label")])
	  #if (area %in% grids.dat$PID){
	    #grids.dat=subset(grids.dat, PID==area)}
	  addLabels(subset(grids.dat,!duplicated(label)),col=rgb(0.5,0.5,0.5,0.5),cex=grid.labcex)
	}
	  if('subarea'%in%labels) addLabels(subset(grids.dat,!duplicated(label)),col=rgb(0.5,0.5,0.5,0.5),cex=labcex)
	if(is.list(labels)) addLabels(labels[[1]],polyProps=labels[[2]])

	if(polylstend==T & !is.null(poly.lst)){
	
	    addPolys(poly.lst[[1]],polyProps=poly.lst[[2]])
	  if(is.null(special.labels))  addLabels(subset(LFAgrid.dat,!duplicated(label)),col=rgb(0,0,0,0.8),cex=labcex)
	  if(!is.null(special.labels)) {
	    re = subset(LFAgrid.dat,!duplicated(label))
	    re = merge(re,special.labels,by.y='LFA',by.x='PID')
	    re$label = re$labs
	    addLabels(re,col=rgb(0,0,0,0.8),cex=labcex)
	  }
	}
	
	
	box(lwd=2)
	

	title(main=title)
if(save){
	dev.off()
	pdf2png(file.path(project.figuredirectory("bio.lobster"),fname))
	
}	

}

