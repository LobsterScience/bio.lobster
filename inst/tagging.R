p = bio.lobster::load.environment()
la()

	tagging1.dat = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','Tagging','CapeBretonTaggingData.csv'))

	tagging1.dat$TagLat1 = convert.dd.dddd(tagging1.dat$TagLat)
	tagging1.dat$TagLon1 = convert.dd.dddd(tagging1.dat$TagLon)*-1
	tagging1.dat$TagDate = as.Date(tagging1.dat$TagDate,"%d-%b-%y")
	tagging1.dat$CapDate = as.Date(tagging1.dat$CapDate,"%d-%b-%y")
	tagging1.dat$TagCL = tagging1.dat$TagSize
	tagging1.dat$CapCL = tagging1.dat$CapSize


	tagging2.dat = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','Tagging','crtags.csv'))

	tagging2.dat$Tag = tagging2.dat$TAGNUM
	tagging2.dat$TagLat = tagging2.dat$RLAT
	tagging2.dat$TagLon = tagging2.dat$RLON * -1
	tagging2.dat$CapLat = tagging2.dat$CLAT
	tagging2.dat$CapLon = tagging2.dat$CLON * -1
	tagging2.dat$TagDate = as.Date(tagging2.dat$RELDATE)
	tagging2.dat$CapDate = as.Date(tagging2.dat$CAPDATE)
	tagging2.dat$TagCL = tagging2.dat$RCLMMS
	tagging2.dat$CapCL = tagging2.dat$CAPCLMMS
	tagging2.dat$TagSex = tagging2.dat$RSEX
	tagging2.dat$CapSex = tagging2.dat$CAPSEX

	tagging.data = rbind(tagging1.dat[c("Tag","TagDate","TagLon","TagLat","TagCL","TagSex","CapDate","CapLon","CapLat","CapCL","CapSex")],tagging2.dat[c("Tag","TagDate","TagLon","TagLat","TagCL","TagSex","CapDate","CapLon","CapLat","CapCL","CapSex")])


	tagging.data$DAL = tagging.data$CapDate-tagging.data$TagDate
	tagging.data$SizeDiff = tagging.data$CapCL-tagging.data$TagCL
	tagging.data$MoltIncr = tagging.data$SizeDiff/tagging.data$TagCL

	tagging.data$Molted = ifelse(tagging.data$MoltIncr<0.04,0,1)


	# clean data some
	tagging.data = subset(tagging.data,!is.na(SizeDiff)&MoltIncr> -0.1&MoltIncr< 2)

	badsex = with(tagging.data,c(which(TagSex==1 & CapSex%in%(2:3)),which(CapSex==1 & TagSex%in%(2:3))))
	tagging.data = tagging.data[-badsex,]

	tagging.data = subset(tagging.data,TagLon< -10&CapLon< -10&TagLat>0&CapLat>0 )
	tagging.data = subset(tagging.data,as.Date(CapDate)>=as.Date(TagDate))


	# add Depth
	tagging.data = assignDepth(tagging.data, input_names = c("TagLon","TagLat"))
	tagging.data$TagDepth = tagging.data$z
	tagging.data = assignDepth(tagging.data, input_names = c("CapLon","CapLat"))
	tagging.data$CapDepth = tagging.data$z
	tagging.data$z = rowMeans(tagging.data[,c('TagDepth','CapDepth')],na.rm=T)

	tagging1.data = assignArea(tagging.data, coords= c("TagLon","TagLat"))
	tagging1.data = assignSubArea2733(tagging1.data)

	tagging2.data = assignArea(tagging.data, coords= c("CapLon","CapLat"))
	tagging2.data = assignSubArea2733(tagging2.data)
	tag.subareas = rbind(tagging2.data[,c("EID","subarea")],subset(tagging1.data,!EID%in%tagging2.data$EID,c("EID","subarea")))
	tag.subareas = subset(tag.subareas,!duplicated(EID))
	tagging.data$EID = 1:nrow(tagging.data)
	tagging.data = merge(tagging.data,tag.subareas,all.x=T)
	

	write.csv(tagging.data,file.path(project.datadirectory('bio.lobster'),'data','inputs','Tagging','tagging.csv'),row.names=F)
	tagging.data = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','Tagging','tagging.csv'))


	tagging.data$CL = tagging.data$TagCL

	moltPrModel = glm(Molted ~ degreedays + CL , data = tagging.data, family = binomial(link = "logit"))
	print(summary(moltPrModel))


	moltIncrModel = glm(MoltIncr ~ CL , data = subset(tagging.data,Molted==1&MoltIncr<0.4))
	print(summary(moltIncrModel))



pdf(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018','TaggingMap.pdf'))
png(file.path(project.datadirectory('bio.lobster'),'figures','LFA2733Framework2018',"TaggingMap.png"),width=7,height=7,units='in',res=200)
LobsterMap('all')

#bioMap("SS")
with(tagging.data,segments(TagLon,TagLat,CapLon,CapLat,col=rgb(1,0,1,0.2)).lwd=0.5)

with(tagging.data,points(TagLon,TagLat,pch=16,cex=0.3,col=rgb(1,0,0,0.2)))
with(tagging.data,points(CapLon,CapLat,pch=16,cex=0.3,col=rgb(0,0,1,0.2)))

dev.off()

pdf('TaggingHist.pdf')

hist(decimal_date(as.Date(tagging.data$TagDate)),main="histogram of tagging data",xlab="Year",col='grey')
dev.off()

######################### get degree days
	RLibrary("bio.bathymetry","bio.indicators","bio.temperature")
	p = spatial_parameters( type = "canada.east" )  
	p$yrs = 1978:2016
    p$nw = 10 # number of intervals in time within a year
    p$dyears = (c(1:p$nw)-1)  / p$nw # intervals of decimal years... fractional year breaks

	tagging.data$EID = 1:nrow(tagging.data)
    
    td = list()
    x=c()
    for (i in 1:nrow(tagging.data)){
		d = round(decimal_date(seq(ymd(tagging.data$TagDate[i]),ymd(tagging.data$CapDate[i]),by='day')),1)
		d2 = d[!duplicated(d)]
		x[i] = length(d)/length(d2)

		td[[i]] = data.frame(tagging.data[i,c("EID","TagLon","TagLat","CapLon","CapLat")],Date=date_decimal(d2))
	}
	td = do.call("rbind",td)


	td1 = td[c("EID","Date","TagLon","TagLat")]
	names(td1)[3:4] = c("Lon","Lat")
	td2 = td[c("EID","Date","CapLon","CapLat")]
	names(td2)[3:4] = c("Lon","Lat")
	tag.dat = rbind(td1,td2)
	tag.dat = lonlat2planar(tag.dat, input_names=c("Lon", "Lat"),proj.type = p$internal.projection)

 	# baseline grid use as a predictive surface
	baseLine = bathymetry.db(p=p, DS="baseline")
   
    # identify locations of data relative to baseline for envionmental data
    locsmap = match( 
        lbm::array_map( "xy->1", tag.dat[,c("plon","plat")], gridparams=p$gridparams ), 
        lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams ) )
      
    # assign environmental data based on space-time coordinates
    btemp = indicators.lookup( p=p, DS="spatial.annual.seasonal", locsmap=locsmap, timestamp=as.Date(tag.dat$Date), varnames="tmean" )
	tag.dat$temp=btemp
	tagging.data$DegreeDays=with(tag.dat,tapply(temp,EID,sum))*x/2

#coldestday = ifelse(yday(tagging.dat$TagDate)<yday(as.Date("2017-02-23")),as.Date(paste0(year(tagging.dat$TagDate),"-02-23"))-tagging.dat$TagDate,as.Date(paste0(year(tagging.dat$TagDate)+1,"-02-23"))-tagging.dat$TagDate)
#ndays = tagging.dat$CapDate - tagging.dat$TagDate 
#degreedays=c()
#for(i in 1:length(ndays)){
#	degreedays[i] = sum(rDailyTemps(x=1:ndays[i],b=10,m=10,s=coldestday[i]))
#}

#tagging.dat$degreedays = degreedays

# Molt Probability
require(mgcv)
Model1 <- gam(Molted ~ as.numeric(DAL) + TagCL , data = subset(tagging.data,DAL<670&TagSex==1),
                    family = binomial(link = "logit"))
summary(Model1)
plot(Model1)


require(mgcv)
Model2 <- glm(Molted ~ DegreeDays + TagCL , data = subset(tagging.data,DAL<670&TagSex==1),
                    family = binomial(link = "logit"))
summary(Model2)
plot(Model2)



lens = c(50,70,90,110,130,150)
plot(c(1,6700),c(0,1),type='n')

for(i in 1:length(lens)){
	y=predict(Model2,data.frame(DegreeDays=1:6700,TagCL=lens[i]),type='response')
	lines(1:6700,y)
}


# Molt Increment

hist(tagging.dat$MoltIncr[tagging.dat$MoltIncr<0.5],breaks=150,ylim=c(0,400))












landings = read.csv(file.path(project.datadirectory('bio.lobster'),'data','AnnualSlipLand.csv'))


plot(landings$YEAR,landings$YEAR,type='n',ylim=c(0,5000),ylab="Landings (t)",xlab="Year")
for(i in 2:ncol(landings))lines(landings$YEAR,landings[,i],col=i)
legend('topleft',names(landings[2:ncol(landings)]),col=2:ncol(landings),lty=1)

