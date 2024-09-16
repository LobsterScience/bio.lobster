# LFA 34 Assessment Script
#	______
#	`>___;'______       /3
#	  ---~<.     ))))))) 3
#	 _____ `,-----%%%%% \3
#	 `>___;- |}}}	 
#           
 require(bio.lobster)
 require(devtools)
 require(lubridate)
 require(bio.utilities)
 require(SpatialHub)
	p = bio.lobster::load.environment()
	la()

	assessment.year = 2020 ########### check the year ############### !!!!!!!!!!!


    p$syr = 1989
    p$yrs = p$syr:assessment.year



	    # define place for figures to go
	    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA34Update")

	    p$lfas = "34" # specify lfa
    	p$subareas = c("34") # specify subareas for data summary
	    
	 
# Map ################

	#	x11(width=5, height=5)
	#	LobsterMap('34')
	#	text(x=c(-65.2,-65.7,-67.4),y=c(43.4,44.9,43.1),labels=c(33,35,41),col=rgb(0,0,0,0.8),cex=1.5)

	#	savePlot(file.path(figdir,'LFA34map.png'),type='png')


# CPUE ###############
		
		logs=lobster.db("process.logs")
		#2020 going with raw CPUES
gg = 	aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~LFA+SYEAR, data=subset(logs,LFA==34),FUN=sum)
gg$CPUE = gg$WEIGHT_KG/gg$NUM_OF_TRAPS	
gM = rmed(gg$SYEAR, gg$CPUE)
png(file=file.path(fpf1,'CPUELFA342023.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(gg,plot(SYEAR,CPUE,xlab='Year',ylab='CPUE', pch=c(rep(16,times=(nrow(gg)-1)),17)))
lines(gM$yr, gM$x, lwd=2, col='salmon')
cpueData=gg
dev.off()	



# Landings and Effort ############

	 	land = lobster.db('seasonal.landings')


		land$YEAR = as.numeric(substr(land$SYEAR,6,9))
		land$LANDINGS = land$LFA34
		cpueData$YEAR=cpueData$SYEAR
		cpueData$mu=cpueData$CPUE
		fishData = merge(cpueData,land[,c("YEAR","LANDINGS")]) 
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$mu



	# plot
	png(file=file.path(fpf1,'LandingsEffortLFA342023.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
	FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = 34,fd=figdir, preliminary=nrow(fishData), units='kt')
	dev.off()
