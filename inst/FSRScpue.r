loadfunctions("bio.lobster")

RLibrary("plyr","lattice","glmmADMB","ggplot2")


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
scd<-read.csv(file.path( project.datadirectory("lobster"), "data","inputs","FSRS_SIZE_CODES.csv"))
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
short.dat<-data.frame(VES_DATE=names(short.lst),SHORTS=short.lst)

legal.lst<-with(subset(FSRS.dat,SHORT==0),tapply(TRAP_NO,VES_DATE,length)) 
legal.dat<-data.frame(VES_DATE=names(legal.lst),LEGALS=legal.lst)

recruit.lst<-with(subset(FSRS.dat,SHORT==1&SIZE_CD>7),tapply(WEIGHT,VES_DATE,sum)) 
recruit.dat<-data.frame(VES_DATE=names(recruit.lst),RECRUITS=recruit.lst)

legalbm.lst<-with(subset(FSRS.dat,SHORT==0),tapply(WEIGHT,VES_DATE,sum)) 
legalbm.dat<-data.frame(VES_DATE=names(legalbm.lst),BIOMASS=legalbm.lst)

FSRS_1.dat <- aggregate(cbind(VESSEL_CD, HAUL_DATE, DEPTH, LFA, LFA_GRID, TEMP, LAT_DD, LONG_DD, HAUL_YEAR, SYEAR)~VES_DATE,data=FSRS.dat,mean,na.rm=T)
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
			FSRSvesday$WOS[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]<-floor((FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]-min(FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]))/7)+1
			FSRSvesday$DOS[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]<-FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]]-min(FSRSvesday$HAUL_DATE[FSRSvesday$SYEAR==season[i]&FSRSvesday$LFA==lfas[a]])+1
		}
	}


str(FSRSvesday)


subareas<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","LFA2733subarea.csv"))
FSRSvesday<-merge(FSRSvesday,subareas,all.x=T)
write.csv(FSRSvesday,file.path( project.datadirectory("bio.lobster"), "data","products","FSRSrectraps.csv"),row.names=F)

FSRSvesday<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","FSRSrectraps.csv"))


#plot raw data on shorts
b = aggregate(cbind(SHORTS,TOTAL_TRAPS)~LFA+SYEAR,data=FSRSvesday,FUN=sum)
b$CPUE=  b$SHORTS / b$TOTAL_TRAPS

p <- ggplot()
p <- p + geom_point(data = b, 
                    aes(y = CPUE, x = SYEAR),
                    shape = 16, 
                    size = 3) + xlab("Year") + ylab("Lobsters / Trap") + theme(text = element_text(size=15)) + 
					theme_bw() + geom_line(data = b,  aes(x = SYEAR, y = CPUE), colour = "black")+ 
					facet_wrap(  ~LFA, ncol=2,scales = "fixed")
p

#plot temperature across years
FSRSvesday$TEMP[FSRSvesday$TEMP==-99] <- NA
  

b = aggregate(TEMP~LFA+SYEAR+DOS,data=FSRSvesday,FUN=mean)

lf = unique(b$LFA)

for(i in lf) {
	j = subset(b,LFA==i)
 plot(1,1,type='n',xlim=c(min(j$DOS),max(j$DOS)),ylim=c(min(j$TEMP),max(j$TEMP)),main=i)

 jj = unique(j$SYEAR)
 m=0
 gg = rev(shadesOfGrey(length(jj)))
 	for(l in jj) {
 		m=m+1
 		with(subset(j,SYEAR==l),lines(DOS,TEMP,col=gg[m]))
 	}
x11()
}





#FSRSvesday$HAUL_DATE<-as.Date(FSRSvesday$HAUL_DATE)

### Checking frequency of missing temp data
#missing.temps.percent<-lapply(lapply(sapply(2001:2014,function(x){1-with(subset(FSRSvesday,SYEAR==x&TEMP>(-5)),tapply(SYEAR,LFA,length))/with(subset(FSRSvesday,SYEAR==x),tapply(SYEAR,LFA,length))}),'*',100),round)
#names(missing.temps.percent)<-2001:2014
#missing.temps.percent



#-----------------------------------LFA27

	#------------------------------North
	LFA27north<-subset(FSRSvesday,subarea=='27 North')

	LFA27n.sm<-FSRSmodel(LFA27north, response="SHORTS")
	#LFA27n.lm<-FSRSmodel(LFA27north, response="LEGALS")

	#------------------------------South
	LFA27south<-subset(FSRSvesday,subarea=='27 South')

	LFA27s.sm<-FSRSmodel(LFA27south, response="SHORTS")
	#LFA27s.lm<-FSRSmodel(LFA27south, response="LEGALS")


#-------------------------------LFA28

	LFA28<-subset(FSRSvesday,LFA==28)

	LFA28.sm<-FSRSmodel(LFA28, response="SHORTS")
	#LFA28.lm<-FSRSmodel(LFA28, response="LEGALS")


#-------------------------------LFA29

	LFA29<-subset(FSRSvesday,LFA==29)

	LFA29.sm<-FSRSmodel(LFA29, response="SHORTS")
	#LFA29.lm<-FSRSmodel(LFA29, response="LEGALS")


#-------------------------------LFA30

	LFA30<-subset(FSRSvesday,LFA==30)

	LFA30.sm<-FSRSmodel(LFA30, response="SHORTS")
	#LFA30.lm<-FSRSmodel(LFA30, response="LEGALS")


#-------------------------------LFA31A

	LFA31A<-subset(FSRSvesday,LFA==31.1)

	LFA31A.sm<-FSRSmodel(LFA31A, response="SHORTS")
	#LFA31A.lm<-FSRSmodel(LFA31A, response="LEGALS")


#-------------------------------LFA31B

	LFA31B<-subset(FSRSvesday,LFA==31.2)

	LFA31B.sm<-FSRSmodel(LFA31B, response="SHORTS")
	#LFA31B.lm<-FSRSmodel(LFA31B, response="LEGALS")


#-------------------------------LFA32

	LFA32<-subset(FSRSvesday,LFA==32)

	LFA32.sm<-FSRSmodel(LFA32, response="SHORTS")
	#LFA32.lm<-FSRSmodel(LFA32, response="LEGALS")


#-----------------------------------LFA33

	#------------------------------East
	LFA33east<-subset(FSRSvesday,subarea=='33 East')

	LFA33e.sm<-FSRSmodel(LFA33east, response="SHORTS",interaction=T)
	#LFA33e.lm<-FSRSmodel(LFA33east, response="LEGALS",interaction=T)

	#------------------------------West
	LFA33west<-subset(FSRSvesday,subarea=='33 West')

	LFA33w.sm<-FSRSmodel(LFA33west, response="SHORTS",interaction=T)
	#LFA33w.lm<-FSRSmodel(LFA33west, response="LEGALS",interaction=T)


#-------------------------------LFA34

	LFA34<-subset(FSRSvesday,LFA==34)

	LFA34.sm<-FSRSmodel(LFA34, response="SHORTS",interaction=T)
	#LFA34.lm<-FSRSmodel(LFA34, response="LEGALS",interaction=T)

#________________________




	#------------------------------load previous runs
#	FSRSvesday<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","FSRSrectraps.csv"))

	LFA27north<-subset(FSRSvesday,subarea=='27 North')
	LFA27south<-subset(FSRSvesday,subarea=='27 South')
	LFA28<-subset(FSRSvesday,LFA==28)
	LFA29<-subset(FSRSvesday,LFA==29)
	LFA30<-subset(FSRSvesday,LFA==30)
	LFA31A<-subset(FSRSvesday,LFA==31.1)
	LFA31B<-subset(FSRSvesday,LFA==31.2)
	LFA32<-subset(FSRSvesday,LFA==32)
	LFA33east<-subset(FSRSvesday,subarea=='33 East')
	LFA33west<-subset(FSRSvesday,subarea=='33 West')


	LFA27n.sm<-FSRSmodel(LFA27north, response="SHORTS",redo=F)
	#LFA27n.lm<-FSRSmodel(LFA27north, response="LEGALS",redo=F)
	LFA27s.sm<-FSRSmodel(LFA27south, response="SHORTS",redo=F)
	#LFA27s.lm<-FSRSmodel(LFA27south, response="LEGALS",redo=F)
	LFA28.sm<-FSRSmodel(LFA28, response="SHORTS",redo=F)
	#LFA28.lm<-FSRSmodel(LFA28, response="LEGALS",redo=F)
	LFA29.sm<-FSRSmodel(LFA29, response="SHORTS",redo=F)
	#LFA29.lm<-FSRSmodel(LFA29, response="LEGALS",redo=F)
	LFA30.sm<-FSRSmodel(LFA30, response="SHORTS",redo=F)
	#LFA30.lm<-FSRSmodel(LFA30, response="LEGALS",redo=F)
	LFA31A.sm<-FSRSmodel(LFA31A, response="SHORTS",redo=F)
	#LFA31A.lm<-FSRSmodel(LFA31A, response="LEGALS",redo=F)
	LFA31B.sm<-FSRSmodel(LFA31B, response="SHORTS",redo=F)
	#LFA31B.lm<-FSRSmodel(LFA31B, response="LEGALS",redo=F)
	LFA32.sm<-FSRSmodel(LFA32, response="SHORTS",redo=F)
	#LFA32.lm<-FSRSmodel(LFA32, response="LEGALS",redo=F)
	LFA33e.sm<-FSRSmodel(LFA33east, response="SHORTS",redo=F)
	#LFA33e.lm<-FSRSmodel(LFA33east, response="LEGALS",redo=F)
	LFA33w.sm<-FSRSmodel(LFA33west, response="SHORTS",redo=F)
	#LFA33w.lm<-FSRSmodel(LFA33west, response="LEGALS",redo=F)
	#LFA34.sm<-FSRSmodel(LFA34, response="SHORTS",redo=F)
	#LFA34.lm<-FSRSmodel(LFA34, response="LEGALS",redo=F)

	# plot model fit in most recent year
	FSRSmodel.3dplot(LFA27n.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA27s.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA28.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA29.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA30.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA31A.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA31B.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA32.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA33e.sm$model,response="SHORTS")
	FSRSmodel.3dplot(LFA33w.sm$model,response="SHORTS")

# compile results
LFA27n.sm$pData$Area<-'27N'
LFA27s.sm$pData$Area<-'27S'
LFA28.sm$pData$Area<-'28'
LFA29.sm$pData$Area<-'29'
LFA30.sm$pData$Area<-'30'
LFA31A.sm$pData$Area<-'31A'
LFA31B.sm$pData$Area<-'31B'
LFA32.sm$pData$Area<-'32'
LFA33e.sm$pData$Area<-'33E'
LFA33w.sm$pData$Area<-'33W'

shorts<-rbind(LFA27n.sm$pData,LFA27s.sm$pData,LFA28.sm$pData,LFA29.sm$pData,LFA30.sm$pData,LFA31A.sm$pData,LFA31B.sm$pData,LFA32.sm$pData,LFA33e.sm$pData,LFA33w.sm$pData)
write.csv(shorts,file.path( project.datadirectory("bio.lobster"), "data","products","FSRSmodelresultsSHORT.csv"),row.names=F)

shorts<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","FSRSmodelresultsSHORT.csv"))
shorts.2829<-subset(shorts,Area %in% c("28","29"))

# shorts
	pdf(file.path(project.figuredirectory('bio.lobster'),'FSRS.LFA27-33.2017.pdf'),width=8,height=10)


p <- ggplot()
p <- p + geom_point(data = shorts.2829, 
                    aes(y = mu, x = YEAR),
                    shape = 16, 
                    size = 3)
p <- p + xlab("Year") + ylab("Lobsters / Trap")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = shorts.2829, 
                   aes(x = YEAR, y = mu), 
                   colour = "black")

p <- p + geom_ribbon(data = shorts.2829, 
                     aes(x = YEAR, 
                         ymax = ub, 
                         ymin = lb ),
                     alpha = 0.5)
p <- p + facet_wrap(  ~Area, ncol=1,scales = "fixed")
print(p)
dev.off()
