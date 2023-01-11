p = bio.lobster::load.environment()

require(bio.utilities)
require(ggplot2)
require(plyr)
require(rmarkdown)
require(tint)
require(rmarkdown)
require(rio)
require(knitr)


study.yr="2022"
figdir = file.path(project.datadirectory("bio.lobster","requests","vent",study.yr))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
fishdir= file.path(figdir,"fishers")
dir.create( fishdir, recursive = TRUE, showWarnings = FALSE )
lfadir= file.path(figdir,"lfas")
dir.create( lfadir, recursive = TRUE, showWarnings = FALSE )
 
vent=read.csv("C:/bio.data/bio.lobster/data/vent/Trap.csv")
names(vent)=c("fisher", "license","lfa", "port","grid","depth.unit","trap.type", "trap.style","unique.trap", "date", "depth", "vent.sz", "vent.num", "trap.num","s14","s15","s16","s17a","s17b","s18","s19","s20")

#sort dates
vent$date=strptime(vent$date, "%m/%d/%Y %H:%M:%S")
vent$yr=factor(lubridate::year(vent$date))


#take all depth to meters (depth.m)
vent$depth.m=NA
feet=vent[which(vent$depth.unit=="Feet"),]
feet$depth.m=feet$depth/6
fathom=vent[which(vent$depth.unit=="Fathom"),]
fathom$depth.m=fathom$depth/1.83
vent=rbind(feet,fathom)
rm(fathom, feet)

vent$trap.id=paste(vent$fisher, vent$date, vent$trap.num, sep=":") 
vent$config=paste(vent$vent.sz,"x", vent$vent.num, sep="") #creates a field that captures vent size and number

#----------------------------------------------------------
#db error checking
#----------------------------------------------------------

data.check=F
#data.check=T

if (data.check){
#NA's in vent.sz
nulls=vent[is.na(vent$vent.sz),]
if(length(nulls$fisher)>0) {View(nulls)}else {print("No NA's in Vent Sizes")}


#only paired data should be in db, for each date, fisher, trap number should be a pair
trap.pairs=aggregate(unique.trap~as.character(date)+fisher+trap.id, data=vent, FUN=length)
probs=trap.pairs[trap.pairs$unique.trap>2,]
probs=probs[probs$fisher!="Dave Ferguson",] #2022, Dave Ferguson tried 3 configurations.
if(length(probs$fisher)>0) {View(probs)}else {print("All traps are paired")}

#confirm a paired vent configuration (i.e. 1 44mm, 1 47mm) for each trap.id
test=aggregate(unique.trap~as.character(date)+fisher+vent.sz, data=vent, FUN=length)
test44=test[test$vent.sz=="44",]
test44=test44[,-3]
names(test44)=c("date", "fisher", "sz.44")
test47=test[test$vent.sz=="47",]
test47=test47[,-3]
names(test47)=c("date", "fisher", "sz.47")
data.check=merge(test44, test47)
data.check$diff=data.check$sz.44-data.check$sz.47
probs=data.check[data.check$diff!="0",]
if(length(probs$date)>0) {View(probs)}else {print("Same # of 44 & 47 mm vents for each date for each fisher")}

rm(test44, test47, probs, data.check, nulls, test)
}

#----------------------------------------------------------
# Data Prep for figure / results creation
#----------------------------------------------------------

#Convert fields to factors as needed
cols=c("fisher", "lfa", "trap.type", "vent.sz", "vent.num")
vent[cols]=lapply(vent[cols], factor)

#determine counts of shorts / legals for each trap
vent$shorts=NA
vent$legals=NA
for (i in 1:length(vent$fisher)){
vent$shorts[i]=sum(vent$s14[i], vent$s15[i], vent$s16[i], vent$s17a[i])
vent$legals[i]=sum(vent$s17b[i], vent$s18[i], vent$s19[i], vent$s20[i])
}

#determine proportion of shorts / legals in each trap
vent$prop.short=(vent$shorts/ (vent$shorts+vent$legals))
vent$prop.legals=(vent$legals/ (vent$shorts+vent$legals))


#Establish a column with a configuration name to reference for labels, etc

vent$config.name[vent$config=="44x1"] = "One 44mm Vent"
vent$config.name[vent$config=="4x2"] = "Two 44mm Vents"
vent$config.name[vent$config=="44x3"] = "Three 44mm Vents"
vent$config.name[vent$config=="47x1"] = "One 47mm Vent"
vent$config.name[vent$config=="47x2"] = "Two 47mm Vents"
vent$config.name[vent$config=="47x3"] = "Three 47mm Vents"
vent$config.name[vent$config=="49x1"] = "One 49mm Vent"
vent$config.name[vent$config=="49x2"] = "Two 49mm Vents"
vent$config.name[vent$config=="49x3"] = "Three 49mm Vents"
vent$config.name=factor(vent$config.name)

#identifies traps that have more legals than shorts (vent$more.legal=1)
vent$more.legal=ifelse(vent$prop.short>0.5, 0,1) #NAs created when there is no catch in the trap. 
a=aggregate(more.legal~vent.sz+lfa, data=vent, FUN=sum)
b=aggregate(trap.id~vent.sz+lfa, data=vent, FUN=length)
ab=merge(a,b)
ab=ab[order(ab$lfa),] 

rm(a,b)

# Combines the paired trap hauls to have number of shorts and legals in each vent size
#Will need to eventually do this for all possible configurations

vent.44x1=vent[vent$config=="44x1",]
vent.47x1=vent[vent$config=="47x1",]
vent.47x2=vent[vent$config=="47x2",]
vent.49x2=vent[vent$config=="49x2",]

vent.44x1=rename.df(vent.44x1, c("shorts", "legals"), c("shorts.44x1", "legals.44x1"))
vent.47x1=rename.df(vent.47x1, c("shorts", "legals"), c("shorts.47x1", "legals.47x1"))
vent.47x2=rename.df(vent.47x2, c("shorts", "legals"), c("shorts.47x2", "legals.47x2"))
vent.49x2=rename.df(vent.49x2, c("shorts", "legals"), c("shorts.49x2", "legals.49x2"))

#following two lines potentially merge multiple df's but not yet working. PLaceholder for future consideration (lappy ot tapply, maybe?)
#config.list= list(vent.44x1, vent.47x1, vent.47x2, vent.49x1) #add to list as appropriate
#vent3=Reduce(function(x, y) merge(x, y, all=TRUE), config.list)

#can potentially simplify merging data sets but it works!
vent2=merge(vent.44x1[, c("fisher", "lfa", "port", "yr", "trap.type", "trap.id", "shorts.44x1", "legals.44x1")], vent.47x1[, c("trap.id", "shorts.47x1", "legals.47x1")],all.x=TRUE )
vent2=merge(vent2,vent.47x2[, c("trap.id", "shorts.47x2", "legals.47x2")], all.x=T )
vent2=merge(vent2,vent.49x2[, c("trap.id", "shorts.49x2", "legals.49x2")], all.x=T )

#----------------------------------------------------------
#Functions Block
#----------------------------------------------------------
config.comparison=  function(f="", plot.t=titl){ #Graphic Visualization of fisher summary  
  prop.sums=data.frame(matrix(NA, nrow = 6, ncol = 3))
  names(prop.sums)=c("Comparison","Size", "Value")  
  prop.sums[1,]=c("Less", "Shorts",less.shorts)
  prop.sums[2,]=c("Same", "Shorts",same.shorts)
  prop.sums[3,]=c("More", "Shorts",more.shorts)
  prop.sums[4,]=c("Less", "Legals",less.legals)
  prop.sums[5,]=c("Same", "Legals",same.legals)
  prop.sums[6,]=c("More", "Legals",more.legals)
  
  prop.sums$Value=as.numeric(prop.sums$Value)
  prop.sums$Value=round(prop.sums$Value,3)
  prop.sums$Comparison=factor(prop.sums$Comparison, levels=c("More", "Same", "Less"))
  prop.sums$Size=factor(prop.sums$Size, levels=c("Shorts", "Legals"))
  
  #establish titles and better worded config references
  #plot.t=paste(f, "LFA",as.character(vent$lfa[vent$fisher==f][1]))
  vent.config1=paste(substr(config1, 1, 2),    "mm with ",    substr(config1, 4, 4), " vent(s)", sep="")
  vent.config1[vent.config1=="44mm with 1 vent(s)"]="one 44mm vent"
  vent.config1[vent.config1=="47mm with 1 vent(s)"]="one 47mm vent"
  vent.config1[vent.config1=="47mm with 2 vent(s)"]="two 47mm vents"
  
  vent.config2=paste(substr(config2, 1, 2),    "mm with ",    substr(config2, 4, 4), " vent(s)", sep="")
  vent.config2[vent.config2=="44mm with 1 vent(s)"]="one 44mm vent"
  vent.config2[vent.config2=="47mm with 1 vent(s)"]="one 47mm vent"
  vent.config2[vent.config2=="47mm with 2 vent(s)"]="two 47mm vents"
  
  #text description
  l.s=paste ("Traps with ", vent.config1, " had less shorts ", round(less.shorts*100, 1), "% of the time compared to ",vent.config2, sep="" )
  s.s=paste ("Traps with ",vent.config1, " had same number of shorts ", round(same.shorts*100, 1), "% of the time compared to ",vent.config2, sep="" )
  m.s=paste ("Traps with ",vent.config1, " had more shorts ", round(more.shorts*100, 1), "% of the time compared to ",vent.config2, sep="" )
  l.l=paste ("Traps with ",vent.config1, " had less legals ", round(less.legals*100, 1), "% of the time compared to ",vent.config2, sep="" )
  s.l=paste ("Traps with ",vent.config1, " had same number of legals ", round(same.legals*100, 1), "% of the time compared to ",vent.config2, sep="" )
  m.l=paste ("Traps with ",vent.config1, " had more legals ", round(more.legals*100, 1), "% of the time compared to ",vent.config2, sep="" )  
  
  plot.sub= paste("Traps with",vent.config1, "as compared to", vent.config2)

  print(
  ggplot(prop.sums, aes(x = Size, y = Value, fill = Comparison,label = paste(Value*100))) + 
    geom_bar(position="fill", stat="identity", width=0.6) +
    geom_text(size = 4.5, position = position_stack(vjust = 0.5), color="white") +
    labs(y= "% of Trap Hauls", x="") +
    labs(caption= paste("Trap hauls included in study: ", trap.tot,"\n","\n","Shorts","\n", m.s,"\n", s.s,"\n", l.s,"\n","\n","Legal-Sized","\n",m.l,"\n", s.l,"\n",l.l,"\n", sep="")) +
    theme(plot.caption = element_text(hjust=0, size =9)) +
    ggtitle(label=plot.t, subtitle=plot.sub) +
    theme(axis.text.x = element_text(size=12, face="bold")) +  
    theme(plot.title = element_text( size=14, face="bold")) +
    theme(axis.title = element_text(size=12)) +
    theme(plot.subtitle = element_text(size=9)) +
    theme( axis.text.y=element_blank(), ) +
    scale_y_continuous(expand = c(0.05,0))
)
  } 

perc.short=function(plot.t=titl){ #Compare occurrence of percent shorts by trap 
  print(
    ggplot(data = ventf, aes(x=prop.short*100)) +
    geom_histogram(colour = 'black', fill='white', binwidth=05,) +
    facet_grid(config.name~.)+
    theme(strip.text = element_text(size = 12, color = "dark green")) +
    geom_vline(data=mu, aes(xintercept=grp.mean*100, color="red"),linetype="dashed", show.legend = F) +
    #geom_label(data=mu, aes(x=0.5, y=Inf, label=paste("Average ",round(grp.mean*100,0), "%", sep="")), vjust=4, hjust=-.35, color="red") +
    labs( x="Percent of Shorts in Traps", y= "Number of Trap Hauls") +
    labs(caption= paste("\n","- Traps with ", mu$config.name[1]," had on average ",round((mu$grp.mean[1])*100,0),"% short lobsters in the trap", "\n","\n", "- Traps with ", mu$config.name[2]," had on average ",round((mu$grp.mean[2])*100,0),"% short lobsters in the trap", sep="")) +
    theme(plot.caption = element_text(hjust=0, size =9)) +
    ggtitle(plot.t)+
    theme(plot.title = element_text(size=14, face="bold")) +
    theme(axis.line = element_line(colour = "black"))+
    theme(panel.background = element_blank()) +
    scale_y_continuous(expand = c(0,0))
  )
  }

size.class.histogram=function(plot.t=titl){#plot the occurrence of each size class by config          
  library(reshape2)
  library(EnvStats)
  
  if (as.character(ventf$lfa[1])=="29"){ventf2=ventf[,c("s14","s15","s16", "s17a", "s18", "s19", "s20", "config.name")]} else { ventf2=ventf[,c("s14","s15","s16", "s17a", "s17b", "s18", "s19", "s20", "config.name")]  }
  
  if (as.character(ventf$lfa[1])=="29"){names(ventf2)=c("0-70","70-75","75-80", "80-84", "84-90", "90-95", "95+", "Vent.Configuration")} else{ names(ventf2)=c("0-70","70-75","75-80", "80-82.5", "82.5-85", "85-90", "90-95", "95+", "Vent.Configuration")}  
  
 # if (as.character(ventf$lfa[1])=="29"){mls=4.5}else{mls=5.5}
  mls=4.5
  bar.p=melt(ventf2, id.vars="Vent.Configuration")
  
  
 print(
   
 ggplot(data=bar.p, aes(x=variable, weight=value, fill=Vent.Configuration))+
    geom_bar(stat="count", position="dodge", width=0.7)+
    #facet_grid(Vent.Configuration~.) +
    theme(strip.text = element_text(size = 12, color = "dark green")) +
    #geom_text(aes(x=1, y=Inf, label=paste("Red line is MLS")), vjust=5, hjust=0, color="red") +
    geom_vline(xintercept = mls, linetype="dotted", color = "red", size=1.0) +
    
    labs( x="Carapace Length (mm)", y= "Number of Lobster",caption= "*Dotted line is Minimum Legal Size") +
    ggtitle(plot.t)+
    theme(plot.title = element_text(size=14, face="bold")) +
    theme(plot.caption = element_text(size=10, color="red", hjust=0)) +
    theme(legend.title=element_blank()) +
    theme(axis.line = element_line(colour = "black"))+
    theme(panel.background = element_blank()) +
    scale_y_continuous(expand = c(0,0))  
 ) 
}

#determine whether to focus on individual fishers, lfas, or mls groups.
## place holder


# Individual Fisher Reports


#create a dataframe that contains all fishers and their "numbers"
all.fishers=as.data.frame(matrix(nrow=0, ncol=10))
names(all.fishers)=c("fisher", "lfa", "config1", "config2", "less.shorts", "same.shorts", "more.shorts", "less.legals", "same.legals", "more.legals")

fishers=as.character(unique(vent2$fisher))
yr=study.yr


#----------------------------------------------------------
# By Fisher
#----------------------------------------------------------

for (i in 1:length(fishers)){
  f=fishers[i]
  a=vent2[vent2$fisher==f & vent2$yr==yr,] 
  
  
  trap.tot=length(unique(a$trap.id))*2 #Number of sampled traps included in the study for this year
  
  #determine which configs that fisher tested in that year
  b=a[,colSums(is.na(a))<nrow(a)]
  config.subset=names(b)[grepl("shorts", names(b))]
  config.subset=gsub("shorts.","",config.subset)
  config1=config.subset[1]
  config2=config.subset[2]
  
  
  #Compare two vent configs
  short1=paste("shorts.",config1, sep="")
  short2=paste("shorts.",config2, sep="")
  
  legal1=paste("legals.",config1, sep="")
  legal2=paste("legals.",config2, sep="")
  
  less.shorts=length(a$trap.id[a[,short1]<a[,short2]]) / length(a$trap.id)
  same.shorts=length(a$trap.id[a[,short1]==a[,short2]]) / length(a$trap.id)
  more.shorts= length(a$trap.id[a[,short1]>a[,short2]]) / length(a$trap.id)
  
  less.legals=length(a$trap.id[a[,legal1]<a[,legal2]]) / length(a$trap.id)
  same.legals=length(a$trap.id[a[,legal1]==a[,legal2]]) / length(a$trap.id)
  more.legals= length(a$trap.id[a[,legal1]>a[,legal2]]) / length(a$trap.id)
  

 #create a dataframe with all fishers (when running through in loop)
ind=as.data.frame(matrix(nrow=1, ncol=10))
names(ind)=c("fisher", "lfa", "config1", "config2", "less.shorts", "same.shorts", "more.shorts", "less.legals", "same.legals", "more.legals")
    ind[1,1]=f
    ind[1,2]=as.character(vent$lfa[vent$fisher==f][1])
    ind[1,3]=config1
    ind[1,4]=config2
    ind[1,5]=less.shorts  
    ind[1,6]=same.shorts
    ind[1,7]=more.shorts
    ind[1,8]=less.legals
    ind[1,9]=same.legals
    ind[1,10]=more.legals
  
all.fishers=rbind(all.fishers, ind)    

titl=paste(f, "LFA",as.character(vent$lfa[vent$fisher==f][1]), yr)


#Save Figures
png(filename=paste0(fishdir,"/",f, ".", yr, ".config.comparison.png"),width = 160, height = 150, units='mm', res = 300)
config.comparison(plot.t=titl)
dev.off()
#config.comparison()

ventf=vent[vent$fisher==f & is.finite(vent$prop.short),] #removes NA's for proportions for empty traps, can't compare shorts and legals with no catches
mu <- ddply(ventf, "config.name", summarise, grp.mean=mean(prop.short)) #Determines means by config

png(filename=paste0(fishdir,"/",f, ".", yr,".perc.short.by.config.png"),width = 160, height = 150, units='mm', res = 300)
perc.short()
dev.off()
#perc.short()

png(filename=paste0(fishdir,"/",f, ".", yr,".size.class.histogram.png"),width = 150, height = 150, units='mm', res = 300)
size.class.histogram()
dev.off()
#size.class.by.config()

}


#----------------------------------------------------------
# By LFA
#----------------------------------------------------------


#
l=29 #test case hard coded
l= unique(vent$lfa)

#reset vent configurations in case some are still in memory
config1=NA
config2=NA


#Can hard code vent configurations here
config1="44x1"
config2="47x1"

lfa=unique(vent$lfa)

#weighted=F  #This takes all trap hauls from the LFA and combines them for the 
#weighted=T  #

# LFA (or MLS) reports
for (l in lfa){
  a=vent2[vent2$lfa %in% l & vent2$yr==yr,] 
  
  trap.tot=length(unique(a$trap.id))*2 #Number of sampled traps included in the study for this year
  
  #determine which configs that fisher tested in that year
  b=a[,colSums(is.na(a))<nrow(a)]
  
  if (is.na(config1)){
  config.subset=names(b)[grepl("shorts", names(b))]
  config.subset=gsub("shorts.","",config.subset)
  config1=config.subset[1]
  config2=config.subset[2]
  }
  

 

  #The following does not weight for individual fishers
  #Just sums all trap hauls
  
  
  #Compare two vent configs
  short1=paste("shorts.",config1, sep="")
  short2=paste("shorts.",config2, sep="")
  
  legal1=paste("legals.",config1, sep="")
  legal2=paste("legals.",config2, sep="")
  
  less.shorts=length(a$trap.id[a[,short1]<a[,short2]]) / length(a$trap.id)
  same.shorts=length(a$trap.id[a[,short1]==a[,short2]]) / length(a$trap.id)
  more.shorts= length(a$trap.id[a[,short1]>a[,short2]]) / length(a$trap.id)
  
  less.legals=length(a$trap.id[a[,legal1]<a[,legal2]]) / length(a$trap.id)
  same.legals=length(a$trap.id[a[,legal1]==a[,legal2]]) / length(a$trap.id)
  more.legals= length(a$trap.id[a[,legal1]>a[,legal2]]) / length(a$trap.id)
 
  #establish titles and better worded config references
  titl=paste("LFA",l, yr, sep=" ")
  if (length(l)!=1){titl=paste0("LFAs ",l[1], " & ", l[2]," ", yr)}
 
  ttl=paste("LFA",l,".", yr, sep="")
  
  #Save Figures
  png(filename=paste0(lfadir,"/",ttl, ".config.comparison.png"),width = 160, height = 150, units='mm', res = 300)
  config.comparison()
  dev.off()
  #config.comparison()  
  
  ventf=vent[vent$lfa %in% l & is.finite(vent$prop.short),]
  ventf=ventf[ventf$config %in% c(config1, config2),]
  mu <- ddply(ventf, "config.name", summarise, grp.mean=mean(prop.short)) #Determines means by config
  
  png(filename=paste0(lfadir,"/",ttl,".perc.short.by.config.png"),width = 160, height = 150, units='mm', res = 300)
  perc.short()
  dev.off()
  #perc.short()
 
  
  png(filename=paste0(lfadir,"/",ttl,".size.class.histogram.png"),width = 150, height = 150, units='mm', res = 300)
  size.class.histogram()
  dev.off()
  #size.class.by.config()
  
}



#----------------------------------------------------------
# By mls
#----------------------------------------------------------

by.mls=T
#choose one
mls=82.5
#mls=84

if (by.mls) {
test=vent[vent$lfa !="29",]
if (mls=="84"){l=29} else {l=unique(as.character(test$lfa))}
rm(test)

#reset vent configurations in case some are still in memory
config1=NA
config2=NA


#Can hard code vent configurations here
config1="44x1"
config2="47x1"

lfa=unique(vent$lfa)

#weighted=F  #This takes all trap hauls from the LFA and combines them for the 
#weighted=T  #

# LFA (or MLS) reports

  a=vent2[vent2$lfa %in% l & vent2$yr==yr,] 
  
  trap.tot=length(unique(a$trap.id))*2 #Number of sampled traps included in the study for this year
  
  #determine which configs tested in that year
  b=a[,colSums(is.na(a))<nrow(a)]
  
  if (is.na(config1)){
    config.subset=names(b)[grepl("shorts", names(b))]
    config.subset=gsub("shorts.","",config.subset)
    config1=config.subset[1]
    config2=config.subset[2]
  }
  
  
  #The following does not weight for individual fishers
  #Just sums all trap hauls
  
  
  #Compare two vent configs
  short1=paste("shorts.",config1, sep="")
  short2=paste("shorts.",config2, sep="")
  
  legal1=paste("legals.",config1, sep="")
  legal2=paste("legals.",config2, sep="")
  
  less.shorts=length(a$trap.id[a[,short1]<a[,short2]]) / length(a$trap.id)
  same.shorts=length(a$trap.id[a[,short1]==a[,short2]]) / length(a$trap.id)
  more.shorts= length(a$trap.id[a[,short1]>a[,short2]]) / length(a$trap.id)
  
  less.legals=length(a$trap.id[a[,legal1]<a[,legal2]]) / length(a$trap.id)
  same.legals=length(a$trap.id[a[,legal1]==a[,legal2]]) / length(a$trap.id)
  more.legals= length(a$trap.id[a[,legal1]>a[,legal2]]) / length(a$trap.id)
  
  #establish titles and better worded config references
  titl=paste("LFA",l, yr, sep=" ")
  if (length(l)!=1){titl=paste0("LFAs ",l[1], " & ", l[2]," ", yr)}
  
  ttl=paste("LFA",l,".", yr, sep="")
  if (length(l)!=1){ttl=paste("LFA",l[1],".", l[2],".", yr, sep="")}
  
  
  #Save Figures
  png(filename=paste0(lfadir,"/",ttl, ".config.comparison.png"),width = 160, height = 150, units='mm', res = 300)
  config.comparison()
  dev.off()
  #config.comparison()  
  
  ventf=vent[vent$lfa %in% l & is.finite(vent$prop.short),]
  ventf=ventf[ventf$config %in% c(config1, config2),]
  mu <- ddply(ventf, "config.name", summarise, grp.mean=mean(prop.short)) #Determines means by config
  
  png(filename=paste0(lfadir,"/",ttl,".perc.short.by.config.png"),width = 160, height = 150, units='mm', res = 300)
  perc.short()
  dev.off()
  #perc.short()
  
  
  png(filename=paste0(lfadir,"/",ttl,".size.class.histogram.png"),width = 150, height = 150, units='mm', res = 300)
  size.class.histogram()
  dev.off()
  #size.class.by.config()
  
}


#Well hey there, want to create some pdf reports?? Let's roll...

#----------------------------------------------------------
# Report Generation
#----------------------------------------------------------

setwd(figdir)

cleanRmd <- function(RmdName = 'VentReport', RmdFolder = 'Markdown') {
  unlink(file.path(RmdFolder,paste(RmdName,'_cache',sep="")),recursive=T)
  unlink(file.path(RmdFolder,paste(RmdName,'_files',sep="")),recursive=T)
  unlink(file.path(RmdFolder,paste(RmdName,'.tex',sep="")))
  cat( paste('Clean',RmdName,"\n",sep=" "))
}

cleanRmd()

fishers=as.character(unique(vent2$fisher))

for(i in 1:length(fishers)) {
  f=fishers[i]
  lfai=as.character(vent2$lfa[vent2$fisher==f][1])
 
   if(!dir.exists(file.path(figdir,'reports'))) dir.create(file.path(figdir,'reports'))
  rmarkdown::render('Markdown/VentReport.Rmd',quiet=T)
  file.rename(from = file.path('Markdown','VentReport.pdf'), to = file.path('Reports',paste(f, "LFA", lfai, "pdf", sep=".")))
  cleanRmd()
  #rm(dat)
}


#--------------------------------
# Deprecated Code. Just never know if you might need it later...
#--------------------------------


#ggplot(prop.sums, aes(x = Size, y = Value, fill = Comparison,label = paste(Value*100))) + 
# geom_bar(position="fill", stat="identity", width=0.6) +
# geom_text(size = 4.5, position = position_stack(vjust = 0.5), color="white") +
# labs(y= "% of Trap Hauls", x=paste("Trap Hauls =",trap.tot)) +
# labs(caption= paste("Shorts", m.s, s.s, l.s,"","Legal-Sized",m.l, s.l,l.l, sep="\n")) +
# ggtitle(label=plot.t, subtitle=plot.sub) +
# theme(axis.text.x = element_text(size=12, face="bold")) +  
# theme(plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
# theme(axis.title = element_text(size=12)) +
# theme(plot.subtitle = element_text(hjust = 0.5)) +
# theme(plot.caption = element_text(hjust=0, size =12)) +
# theme( axis.text.y=element_blank(), ) +
# scale_y_continuous(expand = c(0.05,0))



#text description
#l.s=paste (vent.config1, " had less shorts ", round(less.shorts*100, 1), "% of the time compared to ",vent.config2, sep="" )
#s.s=paste (vent.config1, " had same number of shorts ", round(same.shorts*100, 1), "% of the time compared to ",vent.config2, sep=" " )
# m.s=paste (vent.config1, " had more shorts ", round(more.shorts*100, 1), "% of the time compared to ",vent.config2, sep=" " )
#l.l=paste (vent.config1, " had less legals ", round(less.legals*100, 1), "% of the time compared to ",vent.config2, sep=" " )
# s.l=paste (vent.config1, " had same number of legals ", round(same.legals*100, 1), "% of the time compared to ",vent.config2, sep=" " )
#m.l=paste (vent.config1, " had more legals ", round(more.legals*100, 1), "% of the time compared to ",vent.config2, sep=" " )   


#Graphic Visualization of fisher summary
#prop.sums=data.frame(matrix(NA, nrow = 6, ncol = 3))
#names(prop.sums)=c("Comparison","Size", "Value")  
# prop.sums[1,]=c("Less", "Shorts",less.shorts)
#prop.sums[2,]=c("Same", "Shorts",same.shorts)
#prop.sums[3,]=c("More", "Shorts",more.shorts)
# prop.sums[4,]=c("Less", "Legals",less.legals)
# prop.sums[5,]=c("Same", "Legals",same.legals)
# prop.sums[6,]=c("More", "Legals",more.legals)

# prop.sums$Value=as.numeric(prop.sums$Value)
#prop.sums$Value=round(prop.sums$Value,3)
# prop.sums$Comparison=factor(prop.sums$Comparison, levels=c("More", "Same", "Less"))
# prop.sums$Size=factor(prop.sums$Size, levels=c("Shorts", "Legals"))



#Compare size components from one configuration to another  
# ggplot(data = ventf, aes(x=prop.short*100)) +
#  geom_histogram(colour = 'black', fill='white', binwidth=05,) +
# facet_grid(config.name~.)+
#theme(strip.text = element_text(size = 12, color = "dark green")) +
#geom_vline(data=mu, aes(xintercept=grp.mean*100, color="red"),linetype="dashed", show.legend = F) +
#geom_label(data=mu, aes(x=0.5, y=Inf, label=paste("Average ",round(grp.mean*100,0), "%", sep="")), vjust=4, hjust=-.35, color="red") +
#labs( x="Percent of Shorts in Traps", y= "Number of Trap Hauls") +
#ggtitle(paste("LFA",l, yr, sep=" "))+
#theme(plot.title = element_text(size=14, face="bold")) +
#theme(axis.line = element_line(colour = "black"))+
#theme(panel.background = element_blank()) +
#scale_y_continuous(expand = c(0,0))

#plot the occurrence of each size class by config          
# library(reshape2)
# library(EnvStats)
# 
# if (as.character(ventf$lfa[1])=="29"){ventf2=ventf[,c("s14","s15","s16", "s17a", "s18", "s19", "s20", "config.name")]} else{ ventf2=ventf[,c("s14","s15","s16", "s17a", "s17b","s18", "s19", "s20", "config.name")]  }
# 
# if (as.character(ventf$lfa[1])=="29"){names(ventf2)=c("0-70","70-75","75-80", "80-84", "84-90", "90-95", "95+", "Vent.Configuration")} else{ names(ventf2)=c("0-70","70-75","75-80", "80-82.5", "82.5-85", "85-90", "90-95", "95+", "Vent.Configuration")}  
# 
# if (as.character(ventf$lfa[1])=="29"){mls=4.5}else{mls=5.5}
# 
# bar.p=melt(ventf2, id.vars="Vent.Configuration")
# 
# 
# ggplot(data=bar.p, aes(x=variable, weight=value, fill=Vent.Configuration))+
#   geom_bar(stat="count", position="dodge", width=0.7)+
#   #facet_grid(Vent.Configuration~.) +
#   theme(strip.text = element_text(size = 12, color = "dark green")) +
#   #geom_text(aes(x=1, y=Inf, label=paste("Red line is MLS")), vjust=5, hjust=0, color="red") +
#   geom_vline(xintercept = mls, linetype="dotted", color = "red", size=2.0) +
#   ggtitle(paste("LFA",l, yr, sep=" "))+
#   labs( x="Carapace Length (mm)", y= "Number of Lobster", subtitle=paste ("Number of boats= ", length(unique(ventf$fisher)))) +
#   theme(plot.title = element_text(size=13, face="bold")) +
#   theme(axis.line = element_line(colour = "black"))+
#   theme(panel.background = element_blank()) +
#   scale_y_continuous(expand = c(0,0))  
# 
# 
# 
# 
# ventf=vent[vent$fisher==f & is.finite(vent$prop.short),] #removes NA's for proportions for empty traps, can't compare shorts and legals with no catches
# mu <- ddply(ventf, "config.name", summarise, grp.mean=mean(prop.short)) #Determines means by config
# 














