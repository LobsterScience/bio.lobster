FSRSTempModel = function(areas = c("27N","27S","28","29","30","31A","31B","32","33E","33W")){


	require(mgcv)        

	# read in FSRS data
	lobster.db('fsrs')
	fsrs$SYEAR<-fsrs$HAUL_YEAR
	fsrs$HAUL_DATE<-as.Date(fsrs$HAUL_DATE)
	fsrs$SYEAR[fsrs$LFA%in%c("33","34")]<-as.numeric(substr(fsrs$S_LABEL[fsrs$LFA%in%c("33","34")],6,9))
	fsrs = assignSubArea2733(fsrs)
	fsrs = subset(fsrs,subarea%in%areas)

	# Remove NAs and duplicate records
	fsrsT =  subset(fsrs,TEMP>-90)
	fsrsT$Dloc = paste(fsrsT$HAUL_DATE,fsrsT$LATITUDE,fsrsT$LONGITUDE)
	fsrsT = subset(fsrsT,!duplicated(Dloc))

	# filter by year and depth
	fsrsT = subset(fsrsT,DEPTH<20&SYEAR>1999)

	# create time and harmonic variables
	fsrsT$y = decimal_date(fsrsT$HAUL_DATE)
	fsrsT$cos.y = cos(2*pi*fsrsT$y)
	fsrsT$sin.y = sin(2*pi*fsrsT$y)

	# fit GAM model 
	TempModel <- gam(TEMP ~ s(sin.y,k=3,bs="ts")+s(cos.y,k=3,bs="ts")+s(y)+as.factor(subarea),data=fsrsT)
	#TempModel <- gam(TEMP ~ sin.y+cos.y+s(y)+as.factor(subarea),data=fsrsT)
	print(summary(TempModel))

	return(list(Model=TempModel,Data=fsrsT))
}