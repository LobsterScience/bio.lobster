#' calcTrackDist
#' @export
calcAreaSwept = function(sensors="Marport",GearSpreadRange=list("NEST"=c(5,15),"280 BALLOON"=c(5,25))){
	
	lobster.db('survey')
	require(CircStats)
    ILTS2016Tracks = ILTS2016Tracks[order(ILTS2016Tracks$TTIME),]

    ILTS2016Tracks$lat = convert.dd.dddd(ILTS2016Tracks$LATITUDE)
    ILTS2016Tracks$lon = convert.dd.dddd(ILTS2016Tracks$LONGITUDE) * -1
	aspr=1/cos(rad(mean(ILTS2016Tracks$lat,na.rm=T)))


	if(sensors=="Marport"){

		tows = sort(unique(ILTS2016Tracks$TOW) )
		dist=c()
		width=c()
		gear=c()

		pdf(file.path( project.figuredirectory("bio.lobster"), "ILTS2016Tracks.pdf"),6,6)
		for(i in 1:length(tows)){


			plot(lat~lon,subset(ILTS2016Tracks,TOW==tows[i]),type='l',main=tows[i],asp=aspr)
			smoothline = loess(lat~lon,subset(ILTS2016Tracks,TOW==tows[i]))
			with(smoothline,lines(x,fitted,col=rgb(1,0,0,0.7)))
			line = data.frame(PID=1,POS=1:length(smoothline$x),X=smoothline$x,Y=smoothline$fitted)
			names(line)
			X = seq(smoothline$x[1],smoothline$x[length(smoothline$x)],l=100)
			Y = predict(smoothline,newdata=data.frame(lon=X))
			line = data.frame(PID=1,POS=1:100,X=X,Y=Y)
			attr(line,"projection")="LL"
			dist[i] = calcLength(line)$length

			width[i] = mean(subset(ILTS2016TowSpread,TOW==tows[i])$DISTANCE_M,na.rm=T)


		}
		dev.off()

		out = merge(subset(surveyCatch,!duplicated(SET_ID)&YEAR==2016,c("YEAR","SET_NO","SET_ID","STATION","GEAR")),data.frame(YEAR=2016,SET_NO=tows,DIST_KM=dist,WING_SPREAD=width))

		for(i in unique(out$GEAR)){
			out$WING_SPREAD[out$GEAR==i&is.na(out$WING_SPREAD)] = mean(out$WING_SPREAD[out$GEAR==i],na.rm=T)
			out$WING_SPREAD[out$GEAR==i&out$WING_SPREAD>GearSpreadRange[[i]][2]] = mean(out$WING_SPREAD[out$GEAR==i],na.rm=T)
			out$WING_SPREAD[out$GEAR==i&out$WING_SPREAD<GearSpreadRange[[i]][1]] = mean(out$WING_SPREAD[out$GEAR==i],na.rm=T)
		}

		return(out)


	}




}