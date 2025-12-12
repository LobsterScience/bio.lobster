p = bio.lobster::load.environment()

require(bio.utilities)
require(ggplot2)
require(plyr)
require(rmarkdown)
require(tint)
require(rmarkdown)
require(rio)
require(knitr)
require(dplyr)
require(resha)


study.yr="2025"## Make sure to index

#Create directory structure / references
figdir = file.path(project.datadirectory("bio.lobster","requests","vent"))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

lfadir= file.path(figdir,"lfas")
dir.create( lfadir, recursive = TRUE, showWarnings = FALSE )

mlsdir= file.path(figdir,"mls")
dir.create( mlsdir, recursive = TRUE, showWarnings = FALSE )

yr.dir= file.path(figdir,study.yr)
dir.create( yr.dir, recursive = TRUE, showWarnings = FALSE )

fishdir= file.path(figdir,yr.dir, "fishers")
dir.create( fishdir, recursive = TRUE, showWarnings = FALSE )
 
#Import data
vent=read.csv("C:/bio.data/bio.lobster/data/vent/Trap.csv")
names(vent)=c("fisher", "license","lfa", "port","grid","depth.unit","trap.type", "trap.style","unique.trap", "date", "depth", "vent.sz", "vent.num", "trap.num","s14","s15","s16","s17a","s17b","s18","s19","s20")

#sort dates
vent$date=strptime(vent$date, "%m/%d/%Y %H:%M:%S")
vent$yr=lubridate::year(vent$date)
vent$yr=as.factor(vent$yr)

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

#data.check=F
data.check=T

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
test49=test[test$vent.sz=="49",]
test49=test49[,-3]
names(test49)=c("date", "fisher", "sz.49")

#Check 44 vs 47
data.check=merge(test44, test47)
data.check$diff=data.check$sz.44-data.check$sz.47

#2023 John Rockett used 46 mm
test46=test[test$vent.sz=="46",]
test46=test46[,-3]
names(test46)=c("date", "fisher", "sz.46")
data.check.rock=merge(test46, test47)
data.check.rock$diff=data.check.rock$sz.46-data.check.rock$sz.47
names(data.check.rock)=names(data.check)

if(length(probs$date)>0) {View(probs)}else {print("Same # of '44mm' & '47mm' vents for each date for each fisher")}

#Check 47 vs 49
data.check2=merge(test47, test49)
data.check2 <- subset(data.check2, !(grepl("^2022", date) & fisher == "Dave Ferguson")) #Ferguson tested 3 configurations in some pairs in 2022
data.check2$diff=data.check2$sz.47-data.check2$sz.49

probs=data.check2[data.check2$diff!="0",]

if(length(probs$date)>0) {View(probs)}else {print("Same # of '47mm' & '49mm' vents for each date for each fisher")}

rm(test44, test47, test46, test49, probs, data.check,data.check.rock, data.check2, nulls, test)
}

#Process Data and Create Figures

vent <- read.csv("C:/bio.data/bio.lobster/data/vent/Trap.csv")
names(vent) <- c("fisher", "license","lfa", "port","grid","depth.unit","trap.type",
                 "trap.style","unique.trap", "date", "depth", "vent.sz", "vent.num",
                 "trap.num","s14","s15","s16","s17a","s17b","s18","s19","s20")

vent$date <- strptime(vent$date, "%m/%d/%Y %H:%M:%S")
vent$yr <- as.factor(lubridate::year(vent$date))

# Convert depths to meters
vent$depth.m <- NA
feet <- vent[vent$depth.unit=="Feet",]
feet$depth.m <- feet$depth / 6
fathom <- vent[vent$depth.unit=="Fathom",]
fathom$depth.m <- fathom$depth / 1.83
vent <- rbind(feet, fathom)
rm(feet, fathom)

vent$trap.id <- paste(vent$fisher, vent$date, vent$trap.num, sep=":")
vent$config <- paste(vent$vent.sz, "x", vent$vent.num, sep="")


#----------------------------------------------------------
#Functions Block
#----------------------------------------------------------
{
    config.comparison <- function(f="", plot.t=titl) {
        
        prop.sums <- data.frame(matrix(NA, nrow=6, ncol=3))
        names(prop.sums) <- c("Comparison","Size","Value")
        prop.sums[1,] <- c("Less", "Shorts", less.shorts)
        prop.sums[2,] <- c("Same", "Shorts", same.shorts)
        prop.sums[3,] <- c("More", "Shorts", more.shorts)
        prop.sums[4,] <- c("Less", "Legals", less.legals)
        prop.sums[5,] <- c("Same", "Legals", same.legals)
        prop.sums[6,] <- c("More", "Legals", more.legals)
        
        prop.sums$Value <- round(as.numeric(prop.sums$Value),3)
        prop.sums$Comparison <- factor(prop.sums$Comparison, levels=c("More","Same","Less"))
        prop.sums$Size <- factor(prop.sums$Size, levels=c("Shorts","Legals"))
        
        vent.config1 <- paste(substr(config1, 1, 2), "mm with", substr(config1, 4, 4), "vent(s)")
        vent.config1[vent.config1=="44mm with 1 vent(s)"]="one 44mm vent"
        vent.config1[vent.config1=="47mm with 1 vent(s)"]="one 47mm vent"
        vent.config1[vent.config1=="47mm with 2 vent(s)"]="two 47mm vents"
        vent.config1[vent.config1=="46mm with 2 vent(s)"]="two 46mm vents"
        vent.config1[vent.config1=="44mm with 2 vent(s)"]="two 44mm vents"
        
        vent.config2 <- paste(substr(config2, 1, 2), "mm with", substr(config2, 4, 4), "vent(s)")
        
        l.s <- paste("Traps with", vent.config1, "had less shorts", round(less.shorts*100,1), "% of the time compared to", vent.config2)
        s.s <- paste("Traps with", vent.config1, "had same number of shorts", round(same.shorts*100,1), "% of the time compared to", vent.config2)
        m.s <- paste("Traps with", vent.config1, "had more shorts", round(more.shorts*100,1), "% of the time compared to", vent.config2)
        l.l <- paste("Traps with", vent.config1, "had less legals", round(less.legals*100,1), "% of the time compared to", vent.config2)
        s.l <- paste("Traps with", vent.config1, "had same number of legals", round(same.legals*100,1), "% of the time compared to", vent.config2)
        m.l <- paste("Traps with", vent.config1, "had more legals", round(more.legals*100,1), "% of the time compared to", vent.config2)
        
        plot.sub <- paste("Traps with", vent.config1, "as compared to", vent.config2)
        
        p <- ggplot(prop.sums, aes(x = Size, y = Value, fill = Comparison, label = paste(Value*100))) +
            geom_bar(position="fill", stat="identity", width=0.6) +
            geom_text(size=4.5, position=position_stack(vjust=0.5), color="white") +
            labs(y="% of Trap Hauls", x="",
                 caption=paste("Trap hauls included in study:", trap.tot,
                               "\n\nShorts\n", m.s,"\n", s.s,"\n", l.s,
                               "\n\nLegal-Sized\n", m.l,"\n", s.l,"\n", l.l)) +
            theme(plot.caption=element_text(hjust=0, size=8)) +
            ggtitle(label=plot.t, subtitle=plot.sub) +
            theme(axis.text.x=element_text(size=12, face="bold"),
                  plot.title=element_text(size=14, face="bold"),
                  axis.title=element_text(size=12),
                  plot.subtitle=element_text(size=9),
                  axis.text.y=element_blank()) +
            scale_y_continuous(expand=c(0.05,0))
        
        return(p)
    }
    
    # 2. Percent Shorts Histogram
    perc.short <- function(plot.t=titl) {
        
        p <- ggplot(data=ventf, aes(x=prop.short*100)) +
            geom_histogram(colour='black', fill='white', binwidth=5) +
            facet_grid(config.name~.) +
            theme(strip.text=element_text(size=12, color="dark green")) +
            geom_vline(data=mu, aes(xintercept=grp.mean*100), linetype="dashed", color="red", show.legend=F) +
            labs(x="Percent of Shorts in Traps", y="Number of Trap Hauls",
                 caption=paste("\n- Traps with", mu$config.name[1], "had on average", round(mu$grp.mean[1]*100,0), "% short lobsters in the trap",
                               "\n\n- Traps with", mu$config.name[2], "had on average", round(mu$grp.mean[2]*100,0), "% short lobsters in the trap")) +
            theme(plot.caption=element_text(hjust=0, size=9),
                  plot.title=element_text(size=14, face="bold"),
                  axis.line=element_line(colour="black"),
                  panel.background=element_blank()) +
            ggtitle(plot.t) +
            scale_y_continuous(expand=c(0,0))
        
        return(p)
    }
    
    
    # 3. Size Class Histogram
    size.class.histogram <- function(plot.t=titl) {
        
        library(reshape2)
        
        if(as.character(ventf$lfa[1])=="29"){
            ventf2 <- ventf[,c("s14","s15","s16","s17a","s18","s19","s20","config.name")]
            names(ventf2) <- c("0-70","70-75","75-80","80-84","84-90","90-95","95+","Vent.Configuration")
        } else {
            ventf2 <- ventf[,c("s14","s15","s16","s17a","s17b","s18","s19","s20","config.name")]
            names(ventf2) <- c("0-70","70-75","75-80","80-82.5","82.5-85","85-90","90-95","95+","Vent.Configuration")
        }
        
        mls <- 4.5
        bar.p <- melt(ventf2, id.vars="Vent.Configuration")
        
        p <- ggplot(data=bar.p, aes(x=variable, weight=value, fill=Vent.Configuration)) +
            geom_bar(stat="count", position="dodge", width=0.7) +
            geom_vline(xintercept=mls, linetype="dotted", color="red", size=1.0) +
            labs(x="Carapace Length (mm)", y="Number of Lobster",
                 caption="*Dotted line is Minimum Legal Size",
                 tag=paste("Trap Hauls:", trap.tot)) +
            ggtitle(plot.t) +
            theme(plot.title=element_text(size=14, face="bold"),
                  plot.caption=element_text(size=10, color="red", hjust=0),
                  legend.title=element_blank(),
                  axis.line=element_line(colour="black"),
                  panel.background=element_blank(),
                  plot.tag.position=c(.865, .445),
                  plot.tag=element_text(size=9)) +
            scale_y_continuous(expand=c(0,0)) +
            coord_cartesian(clip="off")
        
        return(p)
    }
    
}

#### End functions block
####################


#----------------------------------------------------------
# Data Prep for figure / results creation
#----------------------------------------------------------

#Convert fields to factors as needed
cols=c("fisher", "lfa", "trap.type", "vent.sz", "vent.num", "config")
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
vent$config.name[vent$config=="44x2"] = "Two 44mm Vents"
vent$config.name[vent$config=="44x3"] = "Three 44mm Vents"
vent$config.name[vent$config=="46x2"] = "Two 46mm Vents"
vent$config.name[vent$config=="47x1"] = "One 47mm Vent"
vent$config.name[vent$config=="47x2"] = "Two 47mm Vents"
vent$config.name[vent$config=="47x3"] = "Three 47mm Vents"
vent$config.name[vent$config=="49x1"] = "One 49mm Vent"
vent$config.name[vent$config=="49x2"] = "Two 49mm Vents"
vent$config.name[vent$config=="49x3"] = "Three 49mm Vents"
vent$config.name=factor(vent$config.name)

configs=unique(vent$config)
short.c=paste0("shorts.", configs)
legal.c= paste0("legals.", configs) 
config.cols=c(short.c, legal.c)

vent2=vent #testing placeholder  
vent2=cbind(vent2, setNames( lapply(config.cols, function(x) x=NA), config.cols) ) #adds.columns 


for(i in 1:nrow(vent2)){
    k = vent2[i,]
    na = names(k)
    index = grep(k$config,na)
    if(length(index)==2){
        index1 = grep('shorts', na[index])
        index2 = grep('legals',na[index])
    }
    vent2[i,na[index[index1]]] <- vent2[i,'shorts']
    vent2[i,na[index[index2]]] <- vent2[i,'legals']
    
}

vent3=vent2 #for use later
yrs=as.character(unique(vent2$yr))

#----------------------------------------------------------
# Individual Fisher Reports
##--------------------------------------------------------

#create a dataframe that contains all fishers and their "numbers"
all.fishers=as.data.frame(matrix(nrow=0, ncol=11))
names(all.fishers)=c("fisher", "lfa", "year", "config1", "config2", "less.shorts", "same.shorts", "more.shorts", "less.legals", "same.legals", "more.legals")

### Can run a single year by setting yrs= that year
#yrs="2025"

for (y in yrs){
    
    vent=vent2[as.character(vent2$yr)==y,]
    fishers=as.character(unique(vent$fisher))
    fishdir= paste0(figdir,"/",y,"/","fishers")
    dir.create( fishdir, recursive = TRUE, showWarnings = FALSE )
    
    for (i in 1:length(fishers)){
        f=fishers[i]
        print(paste(f, y, sep=' : '))
        a=vent[vent$fisher==f,]
        #a=a[,colSums(is.na(a))<nrow(a)] #drops columns that are all NA
        cols_to_keep <- c("license", "depth", "depth.m")
        cols_to_remove <- which(colSums(is.na(a)) == nrow(a) & !names(a) %in% cols_to_keep)
        
        # Remove the identified columns
        a <- a[, -cols_to_remove]
        
        trap.tot=length(unique(a$trap.id))*2 #Number of sampled traps included in the study for this year
        
        #determine which configs that fisher tested in that year
        b=a[,colSums(is.na(a))<nrow(a)]
        #b=within(b, rm(shorts, legals)) #removing the shorts and legals columns as they have been moved to the appropriate column now
        config.subset=names(b)[grepl("shorts", names(b))]
        config.subset=gsub("shorts.","",config.subset)
        config.subset=config.subset[! config.subset=="shorts"]
        config1=config.subset[1]
        config2=config.subset[2]
        
        l=a$lfa[1]
        a=within(a, rm("fisher", "lfa","license","port","date", "grid","depth.unit","trap.type","trap.style","unique.trap", "depth",
                       "vent.sz","vent.num","trap.num","s14","s15","s16","s17a","s17b","s18","s19","s20","yr","depth.m",
                       "config","shorts","legals","prop.short","prop.legals","config.name"))
        a=a[,colSums(is.na(a))<nrow(a)]   #remove all config columns that only have NA's
        
        a[is.na(a)]=0 #converts NA's to zero, ok as it is only to sum
        
        test=aggregate(.~trap.id, a, sum)
        test$fisher=f
        test$lfa=l
        a=test
        
        
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
        ind=as.data.frame(matrix(nrow=1, ncol=11))
        names(ind)=c("fisher", "lfa", "year", "config1", "config2", "less.shorts", "same.shorts", "more.shorts", "less.legals", "same.legals", "more.legals")
        ind[1,1]=f
        ind[1,2]=as.character(vent$lfa[vent$fisher==f][1])
        ind[1,3]=y
        ind[1,4]=config1
        ind[1,5]=config2
        ind[1,6]=less.shorts  
        ind[1,7]=same.shorts
        ind[1,8]=more.shorts
        ind[1,9]=less.legals
        ind[1,10]=same.legals
        ind[1,11]=more.legals
        
        all.fishers=rbind(all.fishers, ind)    
        
        titl=paste(f, "LFA",as.character(vent$lfa[vent$fisher==f][1]), y)
        
        
        #Save Figures
        #config.comparison()
        p1 <- config.comparison(plot.t=titl)
        ggsave(filename=paste0(fishdir,"/",f,".",y,".config.comparison.png"), 
               plot=p1, width=160, height=150, units="mm", dpi=300)
        
        ventf=vent[vent$fisher==f & is.finite(vent$prop.short),] #removes NA's for proportions for empty traps, can't compare shorts and legals with no catches
        #mu <- ddply(ventf, "config.name", summarise, grp.mean=mean(prop.short)) #Determines means by config
        mu <- ventf %>%
            group_by(config.name) %>%
            summarise(grp.mean = mean(prop.short, na.rm = TRUE))
        
        #perc.short()
        p2 <- perc.short(plot.t=titl)
        ggsave(filename=paste0(fishdir,"/",f,".",y,".perc.short.by.config.png"), 
               plot=p2, width=160, height=150, units="mm", dpi=300)
       
        #size.class.by.config()
        p3 <- size.class.histogram(plot.t=titl)
        ggsave(filename=paste0(fishdir,"/",f,".",y,".size.class.histogram.png"), 
               plot=p3, width=170, height=170, units="mm", dpi=300)
        
        save(all.fishers, file=paste0(figdir,"/all.fishers.config.rdata"))
    }
    
}


#----------------------------------------------------------
# By LFA
#----------------------------------------------------------

vent=vent3 #bring in all years of data

#import fisher config info if not already in memory
load(file=paste0(figdir,"/all.fishers.config.rdata"))
all.fishers$config.pair=with(all.fishers, paste(config1, config2, sep=":"))
all.fishers$fish.year=with(all.fishers, paste(fisher, year, sep=":"))

lfa=unique(vent$lfa)

for (l in lfa){
  c=all.fishers[all.fishers$lfa==l, ]
  print(l)
  print(unique(c$config.pair))
}


lfa= unique(vent$lfa)

#reset vent configurations in case some are still in memory
config1=NA
config2=NA

#Can hard code vent configurations here
#config1="44x1"
#config2="47x1"

#weighted=F  #This takes all trap hauls from the LFA and combines them for the 
#weighted=T  #


# LFA (or MLS) reports

        for (l in lfa){
          #yr=y
          ab=vent2[as.character(vent2$lfa)==l,] #extract only that lfa
          ab$fish.year=with(ab, paste(fisher, yr, sep=":"))
          to.merge=all.fishers[,c("config.pair", "fish.year")]
          ab=merge(ab, to.merge, by="fish.year")
          ab=ab[!(ab$vent.sz=="49"& ab$lfa=="27"),]  #removes Dave Ferguson's few 49mm samples in 2022. 
          rm(to.merge)          
                    ventc=unique(all.fishers$config.pair[all.fishers$lfa==l])
          
          for(i in 1:length(ventc)){
          
              #determine which configs that lfa tested
              vc=ventc[i]
              config1=substr(vc, 1, 4)
              config2=substr(vc, 6, 9)
              
              a=ab[ab$config.pair==vc,] #subsets the config pair reporting on
              a=a[,colSums(is.na(a))<nrow(a)] #drops columns that are all NA
              trap.tot=length(unique(a$trap.id))*2 #Number of sampled traps included in the study for this year
             
            
              #The following does not weight for individual fishers
              #Just sums all trap hauls
              
              
              #Compare two vent configs
              #short1=paste("shorts.",config1, sep="")
              #short2=paste("shorts.",config2, sep="")
              
              #legal1=paste("legals.",config1, sep="")
              #legal2=paste("legals.",config2, sep="")
              
              #less.shorts=length(a$trap.id[a[,short1]<a[,short2]]) / length(a$trap.id)
              #same.shorts=length(a$trap.id[a[,short1]==a[,short2]]) / length(a$trap.id)
              #more.shorts= length(a$trap.id[a[,short1]>a[,short2]]) / length(a$trap.id)
              
              #less.legals=length(a$trap.id[a[,legal1]<a[,legal2]]) / length(a$trap.id)
              #same.legals=length(a$trap.id[a[,legal1]==a[,legal2]]) / length(a$trap.id)
              #more.legals= length(a$trap.id[a[,legal1]>a[,legal2]]) / length(a$trap.id)
             
              #establish titles and better worded config references
             # titl=paste("LFA",l, yr, sep=" ")
              #if (length(l)!=1){titl=paste0("LFAs ",l[1], " & ", l[2]," ", yr)}
             
              ttl=paste("LFA",l, sep=" ")
              
              #Save Figures
              
              #config.comparison()  
              #png(filename=paste0(lfadir,"/",ttl, ".config.comparison.png"),width = 160, height = 150, units='mm', res = 300)
              #config.comparison()
              #dev.off()
              
              
              #ventf=vent[vent$lfa %in% l & is.finite(vent$prop.short),]
              #ventf=ventf[ventf$config %in% c(config1, config2),]
              #mu <- ddply(ventf, "config.name", summarise, grp.mean=mean(prop.short)) #Determines means by config
              
              #perc.short()
              #png(filename=paste0(lfadir,"/",ttl,".perc.short.by.config.png"),width = 160, height = 150, units='mm', res = 300)
              #perc.short()
              #dev.off()
              
             
              #size.class.by.config()
              
              ventf <- a  # your data for this fisher/year
              p <- size.class.histogram(plot.t=ttl)  # returns the ggplot object
              
              ggsave(
                  filename = paste0(lfadir, "/", ttl, ".", i, ".size.class.histogram.png"),
                  plot = p,
                  width = 170,
                  height = 170,
                  units = "mm",
                  dpi = 300
              )
              #png(filename=paste0(lfadir,"/", ttl,".",i, ".size.class.histogram.png"),width = 170, height = 170, units='mm', res = 300)
              #ventf=a
              #size.class.histogram(plot.t=ttl)
              #dev.off()
              
              }
        }

#----------------------------------------------------------
# By mls
#----------------------------------------------------------

ml=c("82.5","84")

vent=vent3 #bring in all years of data
con.count=data.frame(matrix(NA, nrow = 2, ncol = 2)) #for use later in report generation
names(con.count)=c("mls", "num.configs")

for (i in (1:length(ml))){
    mls=ml[i]
    test=vent[vent$lfa !="29",]
    if (mls=="84"){l=29} else {l=unique(as.character(test$lfa))}
    rm(test)
    
    
    #reset vent configurations in case some are still in memory
    config1=NA
    config2=NA
    
    #lfa=unique(vent$lfa)
    
    #import fisher config info if not already in memory
    load(file=paste0(figdir,"/all.fishers.config.rdata"))
    all.fishers$config.pair=with(all.fishers, paste(config1, config2, sep=":"))
    all.fishers$fish.year=with(all.fishers, paste(fisher, year, sep=":"))
    # 
    
      c=all.fishers[all.fishers$lfa %in% l, ]
      print(l)
      print(unique(c$config.pair))
      con.count[i,1]=mls
      con.count[i,2]=length(unique(c$config.pair))
      
      
    
    # MLS Figures
     
      ab=vent2[as.character(vent2$lfa) %in% l,] #extract only that lfa
      ab$fish.year=with(ab, paste(fisher, yr, sep=":"))
      to.merge=all.fishers[,c("config.pair", "fish.year")]
      ab=merge(ab, to.merge, by="fish.year")
      ab=ab[!(ab$vent.sz=="49"& ab$lfa=="27"),] #removes Dave Ferguson's few 49mm samples in 2022
      rm(to.merge)          
      ventc=unique(all.fishers$config.pair[all.fishers$lfa %in% l])
      
      for(i in 1:length(ventc)){
        
        #determine which configs that mls tested
        vc=ventc[i]
        config1=substr(vc, 1, 4)
        config2=substr(vc, 6, 9)
        
        a=ab[ab$config.pair==vc,] #subsets the config pair reporting on
        a=a[,colSums(is.na(a))<nrow(a)] #drops columns that are all NA
        trap.tot=length(unique(a$trap.id))*2 #Number of sampled traps included in the study for this year
        
         ttl=paste("MLS ", mls,"mm",sep="")
        
        #Save Figures
        
        #size.class.by.config()
        
        ventf=a 
        p <- size.class.histogram(plot.t = ttl)
        
        ggsave(
            filename = paste0(mlsdir, "/", ttl, ".", i, ".size.class.histogram.png"),
            plot = p,
            width = 170,
            height = 170,
            units = "mm",
            dpi = 300
        )
      
        
      }
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

fishers=as.character(unique(vent2$fisher[vent2$yr==study.yr]))
yr=y
for(i in 1:length(fishers)) {
  f=fishers[i]
  lfai=as.character(vent2$lfa[vent2$fisher==f][1])
  if (lfai %in% c("28", "29")){mlsi=84} else {mlsi=82.5}
 
 
   if(!dir.exists(file.path(figdir,y,'reports'))) dir.create(file.path(figdir,y,'reports'))
  rmarkdown::render(file.path(figdir,y,'Markdown/VentReport.Rmd'),quiet=T)
  file.rename(from = file.path(figdir,y,'Markdown','VentReport.pdf'), to = file.path(figdir,y,'reports',paste(f, "LFA", lfai, "pdf", sep=".")))
  cleanRmd()
  print(f)
  #rm(dat
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














