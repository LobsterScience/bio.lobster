#' @export
FSRSModelData = function(trap.type='recruitment',TempModelling){

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
    
		#2020 Data does not include temp as of Sept 2020
		#Will use avg temp for that week for the last three years as a proxy until temps are available
		#determine mean temp by week for last 3 years
		#recent=FSRS.dat[FSRS.dat$SYEAR %in% c(2017:2019),]
		#recent$week=week(recent$HAUL_DATE)
		#week.temp=aggregate(TEMP~week+LFA, dat=recent[recent$TEMP>-4 & recent$TEMP <25,], FUN="mean") #removes obvious extraneous temps
		
		#FSRS.old=FSRS.dat[FSRS.dat$SYEAR<2020,]
	#	FSRS.new=FSRS.dat[FSRS.dat$SYEAR==2020,]
		#FSRS.new$week=week(FSRS.new$HAUL_DATE)
		
		#test=merge(FSRS.new, week.temp, by=c("week","LFA"), all=T)
		#test=subset(test,is.finite(test$HAUL_DATE)) #removes empty records created by merge
		#test$TEMP=test$TEMP.y
		#test=subset(test, select=-c(TEMP.x, TEMP.y, week))
		#FSRS.dat=rbind(FSRS.old, test)
		#FSRS.dat=subset(FSRS.dat,is.finite(FSRS.dat$HAUL_DATE))
		
		# this section is to deal with the fact that there are uneven binning going on for the different size categories
		# it creates a pseudo CL (the mid point of each size category)
		#FSRS moved to 5mm bins in 2020. So pre-2020 and post are separated for determination of sizes, then re-combined
		
		
		fsrs.old=subset(FSRS.dat, SYEAR<=2019)
		scd<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FSRS_SIZE_CODES.csv"))
		scd$LENGTH<-rowMeans(scd[c("MIN_S","MAX_S")])
		fsrs.old<-merge(fsrs.old,scd[c("SIZE_CD","LENGTH")])
		fsrs.old$CODES_VERSION="old"
		
	  fsrs.new=subset(FSRS.dat, SYEAR>2019)
		scd.new<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FSRS_SIZE_CODES_NEW2020.csv"))
		scd.new$LENGTH<-rowMeans(scd.new[c("MIN_S","MAX_S")])
		fsrs.new<-merge(fsrs.new,scd.new[c("SIZE_CD","LENGTH")])
		fsrs.new$CODES_VERSION="new"
		 
		FSRS.dat=rbind(fsrs.old, fsrs.new)
	
		wa<-c(0.000608, 0.001413, 0.00482)
		wb<-c(3.058, 2.875, 2.638)

		FSRS.dat$WEIGHT<-NA
		FSRS.dat=subset(FSRS.dat, SEX %in% c(1:3))
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

		
		# Depends on FSRS sizes codes so is year dependent (coded above as CODEs_VERSION)
		#recruit.lst<-with(subset(FSRS.dat,SHORT==1&SIZE_CD>7),tapply(TRAP_NO,VES_DATE,length)) 
		#recruit.dat<-data.frame(VES_DATE=names(recruit.lst),RECRUITS=as.numeric(recruit.lst))
		recruit.lst.old<-with(subset(FSRS.dat,SHORT==1 & SIZE_CD>7 & CODES_VERSION=="old"),tapply(TRAP_NO,VES_DATE,length)) 
		recruit.dat.old<-data.frame(VES_DATE=names(recruit.lst.old),RECRUITS=as.numeric(recruit.lst.old))
		
		recruit.lst.new<-with(subset(FSRS.dat,SHORT==1 & SIZE_CD> 14 & CODES_VERSION=="new"),tapply(TRAP_NO,VES_DATE,length)) 
		recruit.dat.new<-data.frame(VES_DATE=names(recruit.lst.new),RECRUITS=as.numeric(recruit.lst.new))
		
		recruit.dat=rbind(recruit.dat.old, recruit.dat.new)
		rm(recruit.dat.new, recruit.dat.old)
		
		# Depends on FSRS sizes codes so is year dependent
		#recbm.lst<-with(subset(FSRS.dat,SHORT==1&SIZE_CD>7),tapply(WEIGHT,VES_DATE,sum)) 
		#recbm.dat<-data.frame(VES_DATE=names(recbm.lst),RECMASS=as.numeric(recbm.lst))
		recbm.lst.old<-with(subset(FSRS.dat,SHORT==1 & SIZE_CD>7 & CODES_VERSION=="old"),tapply(WEIGHT,VES_DATE,sum)) 
		recbm.dat.old<-data.frame(VES_DATE=names(recbm.lst.old),RECMASS=as.numeric(recbm.lst.old))
		recbm.lst.new<-with(subset(FSRS.dat,SHORT==1 & SIZE_CD>14 & CODES_VERSION=="new"),tapply(WEIGHT,VES_DATE,sum)) 
		recbm.dat.new<-data.frame(VES_DATE=names(recbm.lst.new),RECMASS=as.numeric(recbm.lst.new))

		recbm.dat=rbind(recbm.dat.old, recbm.dat.new)
		rm(recbm.dat.old, recbm.dat.new)
		
		legalbm.lst<-with(subset(FSRS.dat,SHORT==0),tapply(WEIGHT,VES_DATE,sum)) 
		legalbm.dat<-data.frame(VES_DATE=names(legalbm.lst),BIOMASS=as.numeric(legalbm.lst))

		FSRS_1.dat <- aggregate(cbind(VESSEL_CD, HAUL_DATE, DEPTH, LFA, LFA_GRID, TEMP, LAT_DD, LONG_DD, HAUL_YEAR, SYEAR)~VES_DATE,data=FSRS.dat,mean,na.rm=T)
		FSRS_2.dat<-merge(trap.dat,merge(recruit.dat,merge(legalbm.dat,merge(short.dat,legal.dat,all=T),all=T),all=T),all=T)
		FSRS_2.dat$RECRUITS[is.na(FSRS_2.dat$RECRUITS)]<-0
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
		FSRSvesday$LFA[FSRSvesday$LFA==31.1] = "31A"
		FSRSvesday$LFA[FSRSvesday$LFA==31.2] = "31B"

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

		#FSRScom.dat<-subset(FSRScom.dat,Soak.Days<6)	# Remove soak days greater than 5,  do not iclude berried females
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
		short.dat<-data.frame(VES_DATE=names(short.lst),SHORTS=as.numeric(short.lst))

		legal.lst<-with(subset(FSRScom.dat,Short==0),tapply(Trap.Number,VES_DATE,length)) 
		legal.dat<-data.frame(VES_DATE=names(legal.lst),LEGALS=as.numeric(legal.lst))

		recruit.lst<-with(subset(FSRScom.dat,Short==1&Size>7),tapply(Trap.Number,VES_DATE,length)) 
		recruit.dat<-data.frame(VES_DATE=names(recruit.lst),RECRUITS=as.numeric(recruit.lst))

		recbm.lst<-with(subset(FSRScom.dat,Short==1&Size>7),tapply(WEIGHT,VES_DATE,sum)) 
		recbm.dat<-data.frame(VES_DATE=names(recbm.lst),RECMASS=as.numeric(recbm.lst))

		legalbm.lst<-with(subset(FSRScom.dat,Short==0),tapply(WEIGHT,VES_DATE,sum)) 
		legalbm.dat<-data.frame(VES_DATE=names(legalbm.lst),BIOMASS=as.numeric(legalbm.lst))
		

		FSRS_1.dat <- aggregate(cbind(Vessel.Code, SDATE, LFA, GRID_NUM, Temp, Y, X, SYEAR)~VES_DATE,data=FSRScom.dat,mean,na.rm=T,na.action=NULL)
		FSRS_2.dat<-merge(trap.dat,merge(recruit.dat,merge(legalbm.dat,merge(short.dat,legal.dat,all=T),all=T),all=T),all=T)
		FSRS_2.dat$SHORTS[is.na(FSRS_2.dat$SHORTS)]<-0
		FSRS_2.dat$LEGALS[is.na(FSRS_2.dat$LEGALS)]<-0
		FSRS_2.dat$RECRUITS[is.na(FSRS_2.dat$RECRUITS)]<-0
		FSRSvesday<-merge(FSRS_1.dat,FSRS_2.dat,all.x=T)
		FSRSvesday<-assignSubArea2733(FSRSvesday)
		FSRSvesday$SDATE=as.Date(FSRSvesday$SDATE, origin = "1970-01-01")


		### get temps where missing
		library(bio.spacetime)
		library(bio.bathymetry)

		grids.dat = with(FSRSvesday,data.frame(VES_DATE=VES_DATE,y=decimal_date(SDATE),subarea=subarea,X=X,Y=Y))
		grids.dat$subarea[grids.dat$subarea==33] <- "33W"

#browser()

			p = spatial_parameters( type = "canada.east" ) 
			grids.dat = lonlat2planar(grids.dat, input_names=c("X", "Y"),proj.type = p$internal.projection)
			Complete = bathymetry.db(p=p, DS="complete")
 
		 	# identify locations of data relative to baseline for envionmental data
			 locsmap = match( 
			  lbm::array_map( "xy->1", grids.dat[,c("plon","plat")], gridparams=p$gridparams ), 
			  lbm::array_map( "xy->1", Complete[,c("plon","plat")], gridparams=p$gridparams ) )

		 	grids.dat$DEPTH = Complete$z[locsmap]
			if(missing(TempModelling)) TempModelling = TempModel(annual.by.area=F)

	    newdata = with(grids.dat,data.frame(y=y, cos.y=cos(2*pi*y), sin.y=sin(2*pi*y), DEPTH=DEPTH, area=subarea))
		grids.dat$Temp2 = predict(TempModelling$Model, newdata, type='response')
		grids.dat = na.omit(subset(grids.dat,Temp2>0&DEPTH<500&DEPTH>0))
		FSRSvesday<-merge(FSRSvesday,na.omit(grids.dat[c("VES_DATE","Temp2")]),all.x=T)
		FSRSvesday$Temp[is.na(FSRSvesday$Temp)]<-FSRSvesday$Temp2[is.na(FSRSvesday$Temp)]

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
		
		FSRSvesday$GRID_NUM[FSRSvesday$subarea==33] = NA
		FSRSvesday$subarea[FSRSvesday$subarea==33] = "33W"
		FSRSvesday <- rename.df(FSRSvesday,n0=c('Temp','Vessel.Code'),n1=c('TEMP','VESSEl_CD'))
		
		FSRSvesday = subset(FSRSvesday,SYEAR>2005)

		write.csv(FSRSvesday,file.path( project.datadirectory("bio.lobster"), "data","products","FSRSrectraps.csv"),row.names=F)

		return(FSRSvesday)
	}


}

