#fall bay of fundy ILTS comparative survey
options(stringsAsFactors=F)
require(bio.utilities)
require(lubridate)
require(PBSmapping)
require(mgcv)
require(bio.lobster)
la()

ca = read.csv(file.path(project.datadirectory('bio.lobster'),'data','ILTS_Sep2019_lobster_catch.csv'))
lf = read.csv(file.path(project.datadirectory('bio.lobster'),'data','ILTS_Sep2019_lobster_morphs.csv'))




attr(ca,'projection') <- "LL"
ca$Dist = calcGCdist(ca$SET_LON*-1,ca$SET_LAT, ca$HAUL_LON*-1, ca$HAUL_LAT )$d
ca$YEAR = year(ca$SET_DATE)
ca$MON = month(ca$SET_DATE)

ca = subset(ca,Dist<3 & Dist>.4)

			ca$NUM_CAUGHT[which(is.na(ca$NUM_CAUGHT))]<-0

#Correct by tow distance

ca$cNC = round(ca$NUM_CAUGHT/ca$Dist)





if(LengthBasedCoversion){
			##Length based 
			 lf$ID=paste(lf$TRIP_ID,lf$SET_NO,sep='-')
			 ca$ID=paste(ca$TRIP_ID, ca$SET_NO,sep='-')

			lf1 =merge(lf,ca[,c('ID','GEAR','YEAR','MON','STATION', 'NUM_CAUGHT')],by='ID')

			#no comparative data for 2016--we did not measure winter flounder but we did in 2019 BoF

			lfb = subset(lf1,GEAR=='280 BALLOON' &YEAR==2019)
			lfn = subset(lf1,GEAR=='NEST'&YEAR==2019)

			#check for subsampling
				ab = aggregate(NUM_AT_LENGTH~ID+NUM_CAUGHT+STATION,data=lfb,FUN=sum) #no subsampling all good
				an = aggregate(NUM_AT_LENGTH~ID+NUM_CAUGHT,data=lfn,FUN=sum) #subsampling in one set

			#only a few stations where there were actually >10 fish caught, lets examine those before moving forward
				i = unique(ab$STATION[ab$NUM_CAUGHT>10])

			#building a data set for length based vessel comps
					lb = subset(lfb,STATION %in% i)
					ln = subset(lfn,STATION %in% i)

					lb$b3 = floor(lb$FISH_LENGTH/3)*3
					ln$b3= floor(ln$FISH_LENGTH/3)*3


					lb$b5 = floor(lb$FISH_LENGTH/5)*5
					ln$b5= floor(ln$FISH_LENGTH/5)*5


					lb5 = aggregate(NUM_AT_LENGTH ~ STATION+b5, data=lb,FUN=sum)
					ln5 = aggregate(NUM_AT_LENGTH ~ STATION+b5, data=ln,FUN=sum)

					lb3 = aggregate(NUM_AT_LENGTH ~ STATION+b3, data=lb,FUN=sum)
					ln3 = aggregate(NUM_AT_LENGTH ~ STATION+b3, data=ln,FUN=sum)

					names(lb3)[3] = 'Ball'
					names(ln3)[3] = 'Nest'

					lbn3 = merge(lb3, ln3, by=c('STATION', 'b3'), all=T)
					lbn3 = na.zero(lbn3)
			#Length based conversion Coeffs
					require(gamlss)
					lbn3$C = lbn3$Nest / lbn3$Ball
					out = gamlss(cbind(Nest,Ball)~cs(b3,df=3),data=lbn3,family=BB())
					newd = data.frame(b3=13:45)
					mu = predict(out,what='mu',type='response',newdata=newd)
					rho = mu / (1-mu)

					with(lbn3,plot(b3,C))
					lines(newd[,1],rho)
				