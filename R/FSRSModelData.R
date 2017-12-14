#' @export
FSRSModelData = function(trap.type='recruitment'){

	if(trap.type=='recruitment'){

		#LATEST DATA EXPORT FROM FSRS DATABASE:
		#lobster.db("fsrs.redo")
		lobster.db("fsrs")
		#recruitment.trap.db('raw.redo',p=p)

		FSRS.dat<-fsrs
		FSRS.dat$VES_DATE<-paste(FSRS.dat$VESSEL_CD,FSRS.dat$HAUL_DATE,sep='.')
		FSRS.dat$SYEAR<-FSRS.dat$HAUL_YEAR
		FSRS.dat$HAUL_DATE<-as.Date(FSRS.dat$HAUL_DATE)
		FSRS.dat$SYEAR[FSRS.dat$LFA%in%c("33","34")]<-as.numeric(substr(FSRS.dat$S_LABEL[FSRS.dat$LFA%in%c("33","34")],6,9))

		FSRS.dat<-subset(FSRS.dat,SOAK_DAYS<6)	# Remove soak days greater than 5,  do not iclude berried females
		FSRS.dat$HAUL_DATE<-as.Date(FSRS.dat$HAUL_DATE)


		# this section is to deal with the fact that there are uneven binning going on for the different size categories
		# it creates a pseudo CL (the mid point of each size category)
		scd<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FSRS_SIZE_CODES.csv"))
		scd$LENGTH<-rowMeans(scd[c("MIN_S","MAX_S")])
		FSRS.dat<-merge(FSRS.dat,scd[c("SIZE_CD","LENGTH")])

		wa<-c(0.000608, 0.001413, 0.00482)
		wb<-c(3.058, 2.875, 2.638)

		FSRS.dat$WEIGHT<-NA
		for(i in 1:3){
			FSRS.dat$WEIGHT[FSRS.dat$SEX==i]<-FSRS.dat$LENGTH[FSRS.dat$SEX==i]^wb[i]*wa[i]
		}



		## Aggregate by unique vessal and day, summerizing total traps, legals and shorts

		trap.lst<-lapply(with(FSRS.dat,tapply(TRAP_NO,VES_DATE,unique)),length)
		trap.dat<-data.frame(VES_DATE=names(trap.lst),TOTAL_TRAPS=as.vector(unlist(trap.lst)))

		short.lst<-with(subset(FSRS.dat,SHORT==1),tapply(TRAP_NO,VES_DATE,length)) 
		short.dat<-data.frame(VES_DATE=names(short.lst),SHORTS=as.numeric(short.lst))

		legal.lst<-with(subset(FSRS.dat,SHORT==0),tapply(TRAP_NO,VES_DATE,length)) 
		legal.dat<-data.frame(VES_DATE=names(legal.lst),LEGALS=as.numeric(legal.lst))

		recruit.lst<-with(subset(FSRS.dat,SHORT==1&SIZE_CD>7),tapply(TRAP_NO,VES_DATE,length)) 
		recruit.dat<-data.frame(VES_DATE=names(recruit.lst),RECRUITS=as.numeric(recruit.lst))

		recbm.lst<-with(subset(FSRS.dat,SHORT==1&SIZE_CD>7),tapply(WEIGHT,VES_DATE,sum)) 
		recbm.dat<-data.frame(VES_DATE=names(recbm.lst),RECMASS=as.numeric(recbm.lst))

		legalbm.lst<-with(subset(FSRS.dat,SHORT==0),tapply(WEIGHT,VES_DATE,sum)) 
		legalbm.dat<-data.frame(VES_DATE=names(legalbm.lst),BIOMASS=as.numeric(legalbm.lst))

		FSRS_1.dat <- aggregate(cbind(VESSEL_CD, HAUL_DATE, DEPTH, LFA, LFA_GRID, TEMP, LAT_DD, LONG_DD, HAUL_YEAR, SYEAR)~VES_DATE,data=FSRS.dat,mean,na.rm=T)
		FSRS_2.dat<-merge(trap.dat,merge(recruit.dat,merge(legalbm.dat,merge(short.dat,legal.dat,all=T),all=T),all=T),all=T)
		FSRS_2.dat$SHORTS[is.na(FSRS_2.dat$RECRUITS)]<-0
		FSRS_2.dat$SHORTS[is.na(FSRS_2.dat$SHORTS)]<-0
		FSRS_2.dat$LEGALS[is.na(FSRS_2.dat$LEGALS)]<-0
		FSRSvesday<-merge(FSRS_1.dat,FSRS_2.dat,all.x=T)


			# Create column for week and day of season (WOS, DOS)
			lfas<-unique(FSRSvesday$LFA[!is.na(FSRSvesday$LFA)])
			FSRSvesday$WOS<-NA
			FSRSvesday$DOS<-NA
			for(a in 1:length(lfas)){
				season<-sort(unique(FSRSvesday$SYEAR[FSRSvesday$LFA==lfas[a]]))
				for(i in 1:length(season)){
					FSRSvesday$WOS[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]<-floor((FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]-min(FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]))/7)+1
					FSRSvesday$DOS[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]<-FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]-min(FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]])+1
				}
			}
		
		FSRSvesday<-assignSubArea2733(FSRSvesday)

		write.csv(FSRSvesday,file.path( project.datadirectory("bio.lobster"), "data","products","FSRSrectraps.csv"),row.names=F)

		return(FSRSvesday)
	}
	if(trap.type=='commercial'){
	#names(fsrs.comm)
	# [1] "Record.Number"  "ID"             "Trap.Number"    "Lobster.Number" "Sex"            "Size"           "Short"          "Berried"        "V.Notched"      "Recaptured"    
	#[11] "EID"            "TR.ID"          "Vessel.Code"    "Soak.Days"      "SDATE"          "Temp"           "LFA"            "X"              "Y"              "GRID_NUM"      
	#[21] "SYEAR"          "WOS"           
	#> 
        lobster.db( DS="fsrs.commercial.samples")


		FSRScom.dat<-fsrs.comm
		FSRScom.dat$VES_DATE<-paste(FSRScom.dat$Vessel.Code,FSRScom.dat$SDATE,sep='.')
		FSRScom.dat$SDATE<-as.Date(FSRScom.dat$SDATE)

		FSRScom.dat<-subset(FSRScom.dat,Soak.Days<6)	# Remove soak days greater than 5,  do not iclude berried females
		FSRScom.dat$SDATE<-as.Date(FSRScom.dat$SDATE)
		FSRScom.dat$Temp[FSRScom.dat$Temp>30] = NA

		# this section is to deal with the fact that there are uneven binning going on for the different size categories
		# it creates a pseudo CL (the mid point of each size category)
		scd<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FSRS_SIZE_CODES.csv"))
		scd$LENGTH<-rowMeans(scd[c("MIN_S","MAX_S")])
		names(scd)[1]="Size"
		FSRScom.dat<-merge(FSRScom.dat,scd[c("Size","LENGTH")])

		wa<-c(0.000608, 0.001413, 0.00482)
		wb<-c(3.058, 2.875, 2.638)

		FSRScom.dat$WEIGHT<-NA
		for(i in 1:3){
			FSRScom.dat$WEIGHT[FSRScom.dat$Sex==i]<-FSRScom.dat$LENGTH[FSRScom.dat$Sex==i]^wb[i]*wa[i]
		}

		## Aggregate by unique vessal and day, summerizing total traps, legals and shorts

		trap.lst<-lapply(with(FSRScom.dat,tapply(Trap.Number,VES_DATE,unique)),length)
		trap.dat<-data.frame(VES_DATE=names(trap.lst),TOTAL_TRAPS=as.vector(unlist(trap.lst)))

		short.lst<-with(subset(FSRScom.dat,Short==1),tapply(Trap.Number,VES_DATE,length)) 
		short.dat<-data.frame(VES_DATE=names(short.lst),SHORTS=short.lst)

		legal.lst<-with(subset(FSRScom.dat,Short==0),tapply(Trap.Number,VES_DATE,length)) 
		legal.dat<-data.frame(VES_DATE=names(legal.lst),LEGALS=legal.lst)

		recruit.lst<-with(subset(FSRScom.dat,Short==1&Size>7),tapply(WEIGHT,VES_DATE,sum)) 
		recruit.dat<-data.frame(VES_DATE=names(recruit.lst),RECRUITS=recruit.lst)

		legalbm.lst<-with(subset(FSRScom.dat,Short==0),tapply(WEIGHT,VES_DATE,sum)) 
		legalbm.dat<-data.frame(VES_DATE=names(legalbm.lst),BIOMASS=legalbm.lst)

		FSRS_1.dat <- aggregate(cbind(Vessel.Code, SDATE, LFA, GRID_NUM, Temp, Y, X, SYEAR)~VES_DATE,data=FSRScom.dat,mean,na.rm=T)
		FSRS_2.dat<-merge(trap.dat,merge(recruit.dat,merge(legalbm.dat,merge(short.dat,legal.dat,all=T),all=T),all=T),all=T)
		FSRS_2.dat$SHORTS[is.na(FSRS_2.dat$SHORTS)]<-0
		FSRS_2.dat$LEGALS[is.na(FSRS_2.dat$LEGALS)]<-0
		FSRSvesday<-merge(FSRS_1.dat,FSRS_2.dat,all.x=T)


			# Create column for week and day of season (WOS, DOS)
			lfas<-unique(FSRSvesday$LFA[!is.na(FSRSvesday$LFA)])
			FSRSvesday$WOS<-NA
			FSRSvesday$DOS<-NA
			for(a in 1:length(lfas)){
				season<-sort(unique(FSRSvesday$SYEAR[FSRSvesday$LFA==lfas[a]]))
				for(i in 1:length(season)){
					FSRSvesday$WOS[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]<-floor((FSRSvesday$SDATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]-min(FSRSvesday$SDATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]))/7)+1
					FSRSvesday$DOS[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]<-FSRSvesday$SDATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]-min(FSRSvesday$SDATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]])+1
				}
			}
		
		FSRSvesday<-assignSubArea2733(FSRSvesday)


		write.csv(FSRSvesday,file.path( project.datadirectory("bio.lobster"), "data","products","FSRSrectraps.csv"),row.names=F)

		return(FSRSvesday)
	}


}

