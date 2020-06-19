#install.packages("lubridate") # if necessary
options(stringsAsFactors=FALSE) # this should really be a R default setting but its not, it often causes unexpected errors when set to TRUE

require("lubridate")

# read data
#larval<-read.csv("C:/Users/Brad/Dropbox/Lobster/Data/Larval.csv")
larval<-read.csv(file.path(project.datadirectory("lobster"),"data","Larval.csv"))

# add Year column
larval$Year<-year(larval$Date)

# missing values mean zero I assume
larval$Stage.I[is.na(larval$Stage.I)]<-0
larval$Stage.II[is.na(larval$Stage.II)]<-0
larval$Stage.III[is.na(larval$Stage.III)]<-0
larval$Stage.IV[is.na(larval$Stage.IV)]<-0


## Calculate Relative Volume

	larval$Volume<-larval$Meter.End-larval$Meter.Start

	# Assume meter rolls over at one million
	larval$Volume2<-larval$Volume
	larval$Volume2[larval$Volume<0&!is.na(larval$Volume)]<-larval$Meter.End[larval$Volume<0&!is.na(larval$Volume)]+10^6-larval$Meter.Start[larval$Volume<0&!is.na(larval$Volume)]
	
	# Remove outliers (assumed to be data errors)
	hist(larval$Volume2[larval$Volume2<100000],breaks=40)
	#abline(v=quantile(larval$Volume2,c(0.025,0.975),na.rm=T),col='red')
	abline(v=median(larval$Volume2,na.rm=T)+c(-20000,20000),col='blue') # this is a bit arbitrary but the point is to exclude points we think might be errors
	larval$Volume2[larval$Volume2<(median(larval$Volume2,na.rm=T)-20000)|larval$Volume2>(median(larval$Volume2,na.rm=T)+20000)]<-NA

	# Create standard volume column
	larval$Std.Volume<-larval$Volume2/median(larval$Volume2,na.rm=T)

	hist(larval$Std.Volume)
	# we see from this histogram that the volume often varies +/- 20% and sometimes by as much as  +/- 50% so it is 
	# introducing significant variablity to the larval counts which should be standardized by dividing by this column

	larval$Std.Stage.I<-larval$Stage.I/larval$Std.Volume
	larval$Std.Stage.II<-larval$Stage.II/larval$Std.Volume
	larval$Std.Stage.III<-larval$Stage.III/larval$Std.Volume
	larval$Std.Stage.IV<-larval$Stage.IV/larval$Std.Volume

	sites<-unique(larval$Site)
	
	s1<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.I,Year,mean))})
	s2<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.II,Year,mean))})
	s3<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.III,Year,mean))})
	s4<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.IV,Year,mean))})

	s1.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.I,Year,sd))})
	s2.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.II,Year,sd))})
	s3.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.III,Year,sd))})
	s4.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.IV,Year,sd))})

	ss1<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.I,Year,mean,na.rm=T))})
	ss2<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.II,Year,mean,na.rm=T))})
	ss3<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.III,Year,mean,na.rm=T))})
	ss4<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.IV,Year,mean,na.rm=T))})




