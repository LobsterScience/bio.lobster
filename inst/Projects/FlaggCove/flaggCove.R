require(bio.lobster)
p = bio.lobster::load.environment()
require(bio.polygons)
p$libs = NULL
require(PBSmapping)
require(SpatialHub)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(ggridges)
library(viridis)
library(ggsci)
require(wesanderson)
require(tidyverse)
require(dplyr)
require(stringi)
require(gbm)



flagg<-read.csv(file.path( project.datadirectory('bio.lobster'),"data", "FlaggCove","flaggCove.csv"))
figfp = file.path(project.figuredirectory('bio.lobster'),'data','FlaggCove')

###Look where transects are located 
LFAs<-read.csv(file.path(project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))


LobsterMap(ylim=c(44.73,44.8),xlim=c(-66.8,-66.7),boundaries='LFAs',title="Dive Transects", mapRes="UR")
with(flagg,segments(x0=-longitude_start,y0=latitude_start, x1=-longitude_end, y1=latitude_end))



##Measure transects with position data and add ID
flagg$EID = 1:nrow(flagg)

flagg$date = as.Date(flagg$date, "%d/%m/%Y")

flagg$tmpID = paste(flagg$longitude_start,flagg$latitude_start,flagg$longitude_end,flagg$latitude_end)

x = subset(flagg,!duplicated(tmpID)&!is.na(tmpID)&
             !is.na(longitude_start)&!is.na(latitude_start)&!is.na(longitude_end)&!is.na(latitude_end)
           ,c('longitude_start','latitude_start','tmpID'))
names(x) = c("X","Y","tmpID")
x$PID = 1:nrow(x)
x$POS = 1
y = subset(flagg,!duplicated(tmpID)&!is.na(tmpID)&
             !is.na(longitude_start)&!is.na(latitude_start)&!is.na(longitude_end)&!is.na(latitude_end)
           ,c('longitude_end','latitude_end','tmpID'))
names(y) = c("X","Y","tmpID")
y$PID = 1:nrow(y)
y$POS=2

z = rbind(x,y)
z = z[order(z$PID),]

attr(z,"projection") = "LL"

res = calcLength(z)
res$length = res$length * 1000

k = merge(res,x)

flaggx = merge(flagg,k[,c('tmpID','length')],all.x=T)


hist(flaggx$length,breaks=100)

#reorder
flaggx = flaggx[order(flaggx$EID),]

##Check what lengths transects are, and create unique transect IDs
with(flaggx,tapply(length,seclength_m,median,na.rm=T))


flaggx$TID<- NA
flaggx$TID = paste(flaggx$longitude_start*10000,flaggx$latitude_start*10000,flaggx$date, sep = "-")
flaggx$UID = NA
m = 0

flaggx = subset(flaggx, !is.na(longitude_start) | !is.na(latitude_start))


#stuff brad would never do.

for(i in 1:nrow(flaggx)){
  if(i == 1) {
    m=m+1
    flaggx$UID[i] = m  
  }
  if(i>1){
    if(flaggx$TID[i]== flaggx$TID[i-1]){ # if one row is the same as the previous row - repeat
      flaggx$UID[i] = m 
    }
    if(flaggx$TID[i] != flaggx$TID[i-1]){ #if rows are different than add 1
      m=m+1
      flaggx$UID[i] = m
    }}}


#Check transect lengths based on max section numbers and section length

#Compare if the transectLength of 150 matches the sectno max 6 and 300 matches 12
flaggx$transectLength<-NA
flaggx$transectLength[flaggx$length < 60] = 50
flaggx$transectLength[flaggx$length >140 & flaggx$length < 168] = 150
flaggx$transectLength[flaggx$length >250 & flaggx$length < 360] = 300



lcheck<-aggregate(sectno ~ UID, data = flaggx, FUN= max)
l2check<-aggregate(transectLength ~ UID, data = flaggx, FUN= max)
chex<-merge(lcheck, l2check, by= "UID")
unique(cbind(chex$sectno, chex$transectLength))

lcheck<-aggregate(sectno ~ UID, data = flaggx, FUN= max)
l2check<-aggregate(length ~ UID, data = flaggx, FUN= max)
chex<-merge(lcheck, l2check, by= "UID")
unique(cbind(chex$sectno, chex$length))


#add year
flaggx$year<- year(flaggx$date)
#add month
flaggx$month<-month(flaggx$date)

##Measure density per transect
flaggx$transectLength[is.na(flaggx$transectLength)]<-0
flaggx$area<- NA
flaggx$area<-flaggx$transectLength * flaggx$secwidth_m

##Select only sampling in September
flaggm<-subset(flaggx, month == 9)


#Remove useless data
lobCount<- aggregate(carapace ~ UID + area + date, data=flaggm, length)

#Measure density (# of lobster per area ) within unique transect

colnames(lobCount)[colnames(lobCount)=="carapace"]<- "lobNum"
lobCount$density<- (lobCount$lobNum/lobCount$area)


lobCount$year<- year(lobCount$date)

tnoyrs<-aggregate(UID ~ year, data=lobCount, length)
#plot average density per year

densyrs<-aggregate(density ~ year , lobCount, mean)

##Get rid of the INF density


plot(densyrs[which(densyrs$year >1989),], 
      xlab = "Year", ylab=expression("Density (Number of Lobster/ m"^2* ")"), pch =19, col="skyblue4",cex = 1.25)
#No data for 1996,2009,2010,2013
#savePlot(file.path(figfp,'YearlyDensity.png'),type='png')


##LANDINGS

LobsterMap("38", labels= "grid")

lobster.db( DS = 'annual.landings.redo', p=p)
annual.landings=lobster.db(DS = 'annual.landings', p=p)

loggs<-lobster.db( DS= 'process.logs')
log38<-subset(loggs, GRID_NUM %in% c(50,64))

landings<- with(subset(loggs, LFA ==38),tapply(WEIGHT_KG, SYEAR, sum, na.rm = T))
lands<-landings/1000
an.land<-annual.landings$LFA38[annual.landings$YR >2004 & annual.landings$YR<2018]
perlandings<-lands/an.land

land38<-with(log38, tapply(WEIGHT_KG, SYEAR, sum, na.rm = T))

bumped<-land38/perlandings

  

par(mar=c(3,5,3,3))
barplot(an.land, cex.names= 0.8, space =c(0,0), ylab = "Landings (kg)", ylim = c(0,6000))
box()
#savePlot(file.path(figfp,'LFA38Landings.png'),type='png')


##CATCHRATES
log38<-subset(loggs, GRID_NUM %in% c(50,64))
effort38<-with(log38, tapply(NUM_OF_TRAPS, SYEAR, sum, na.rm = T))
catchrates<-land38/effort38

barplot(catchrates, cex.names= 0.8, space = c(0,0), ylab = "Catch Rates (Number of Traps/Year)", ylim = c(0,2.5))

## Size frequencies each year


ggplot(flaggx, aes(x = carapace))+
  scale_x_continuous(limits =c(30,220), breaks = seq(30,220,10),
    labels=c( "",  " ",  "50",  " "," ", " ", " ", "100", " ", " " ," ", " ", '150', "", " ", " ", " ", "200", " ", " "))+
  scale_y_continuous(expand = c(0,0), limits=c(0,60))+
  geom_histogram(binwidth = 2)+
  facet_wrap(~year, nrow = 4)+
  labs(x="Carapace Length (mm)", y = "Frequency")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour='black'), axis.text.x = element_text(angle = 90))

  
#Ridge Plot

ggplot(flaggm, aes(x= carapace, y=year, group= year))+
  geom_density_ridges(scale=2)+
  labs(x = "Carapace Length(mm)", y = "Annual Length Frequency")+
  theme_ridges(grid = F, center_axis_labels = T)
 
##Ridge plot with Male + Females + Berried
##Subset the data to have Year, carapace length, Sex, and total number per size class  
  
  lengthfreq<- flaggm %>%
    group_by(sex,carapace,year)%>%
    tally()
  
#### MALES ####
  lfm<-lengthfreq %>%
    filter(sex==1)
  
  newlfm<-lfm%>%
    mutate(newlen=ifelse(carapace%in%seq(39,217,2),carapace+1,carapace))
  
  dfm<-aggregate(n~newlen+sex+year,newlfm,sum)
  
#### FEMALES #### 
  lff<-lengthfreq %>%
    filter(sex==2)
  
  newlff<-lff%>%
    mutate(newlen=ifelse(carapace%in%seq(39,217,2),carapace+1,carapace))
  
  dff<-aggregate(n~newlen+sex+year,newlff,sum)

#### BERRIED FEMALES ####   
  lfb<-lengthfreq %>%
    filter(sex==3)
  
  newlfb<-lfb%>%
    mutate(newlen=ifelse(carapace%in%seq(39,217,2),carapace+1,carapace))
  
  dfb<-aggregate(n~newlen+sex+year,newlfb,sum)

#### ALL FEMALES ####   
  lfaf<-lengthfreq %>%
    filter(sex== 2 & 3)
  
  newlfaf<-lfaf%>%
    mutate(newlen=ifelse(carapace%in%seq(39,217,2),carapace+1,carapace))
  
  dfaf<-aggregate(n~newlen+sex+year,newlfaf,sum) 
 
  
#Plotting male, female and berried seaparated on same plot #   
  ggplot() +
    theme_ridges(grid=F,center_axis_labels=T,font_size = 10)+
    theme_classic()+
    xlab('Carapace Length (mm)')+
    ylab('Year')+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.position = c(0.6,0.7),
          plot.title = element_text(hjust = 0.5,size=14),
          plot.subtitle = element_text(hjust = 0.5,size=14),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)),
          plot.margin=unit(c(0.25,1,0.5,0.25),"cm"))+
    #Alpha and scale (below) will control the height and the transparency of the plot
    
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dff,fill="#b2182b",stat = "identity",scale =0.9,alpha=0.5)+
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dfb,fill="#fddbc7",stat = "identity",scale =0.9,alpha=0.5)+
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dfm,fill="#4393c3",stat = "identity",scale =0.9,alpha=0.4)

  
## Maybe 3 separate ridge plots for M, F, B
  
 p1<- ggplot() +
    theme_ridges(grid=F,center_axis_labels=T,font_size = 10)+
    theme_classic()+
    xlab(' ')+
    ylab(' ')+
   scale_y_discrete(
     labels=c( " ",  "1990", " ",  "1992"," ", "1994", " ", "1996", " ", "1998" ," ", "2000", " ", "2002", " ", "2004", " ", "2006", " ", "2008", "2011", "2014", "2015"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(hjust = 0.5,size=14),
          plot.subtitle = element_text(hjust = 0.5,size=14),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          plot.margin=unit(c(0.25,0,0.25,0.25),"cm"))+
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dff,fill="#762a83",stat = "identity",scale =1)
  
  
 p2<- ggplot() +
    theme_ridges(grid=F,center_axis_labels=T,font_size = 10)+
    theme_classic()+
    xlab(' ')+
    ylab('Year')+
   scale_y_discrete(
     labels=c( " ",  "1990", " ",  "1992"," ", "1994", " ", "1996", " ", "1998" ," ", "2000", " ", "2002", " ", "2004", " ", "2006", " ", "2008", "2011", "2014", "2015"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(hjust = 0.5,size=14),
          plot.subtitle = element_text(hjust = 0.5,size=14),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)),
          plot.margin=unit(c(0,0,0,0.25),"cm"))+
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dfb,fill="#e7d4e8",stat = "identity",scale =1)
  
  
p3<-  ggplot() +
    theme_ridges(grid=F,center_axis_labels=T,font_size = 10)+
    theme_classic()+
    xlab('Carapace Length (mm)')+
    ylab(' ')+
  scale_y_discrete(
    labels=c( " ",  "1990", " ",  "1992"," ", "1994", " ", "1996", " ", "1998" ," ", "2000", " ", "2002", " ", "2004", " ", "2006", " ", "2008", "2011", "2014", "2015"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(hjust = 0.5,size=14),
          plot.subtitle = element_text(hjust = 0.5,size=14),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)),
          plot.margin=unit(c(0,0,0.5,0.20),"cm"))+
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dfm,fill="#5aae61",stat = "identity",scale =1)
  

grid.arrange(p1,p2,p3)
#Male and Female Separated Berried included in females  
  
  m1<-ggplot() +
    theme_ridges(grid=F,center_axis_labels=T,font_size = 10)+
    theme_classic()+
    xlab('Carapace Length (mm)')+
    ylab('Year')+
    scale_y_discrete(
          labels=c( " ",  "1990", " ",  "1992"," ", "1994", " ", "1996", " ", "1998" ," ", "2000", " ", "2002", " ", "2004", " ", "2006", " ", "2008", "2011", "2014", "2015"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.position = c(0.6,0.7),
          plot.title = element_text(hjust = 0.5,size=14),
          plot.subtitle = element_text(hjust = 0.5,size=14),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)),
          plot.margin=unit(c(0,1,0.5,0.25),"cm"))+
  
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dfm,fill="#92c5de",stat = "identity",scale =1)
  pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'FlaggCoveRidgesMale.pdf'),width=11,height=12)
  grid.arrange(m1)
  dev.off()
  
  
  f1<-ggplot() +
    theme_ridges(grid=F,center_axis_labels=T,font_size = 10)+
    theme_classic()+
    xlab('Carapace Length (mm)')+
    ylab('Year')+
    scale_y_discrete(
      labels=c( " ",  "1990", " ",  "1992"," ", "1994", " ", "1996", " ", "1998" ," ", "2000", " ", "2002", " ", "2004", " ", "2006", " ", "2008", "2011", "2014", "2015"))+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.position = c(0.6,0.7),
          plot.title = element_text(hjust = 0.5,size=14),
          plot.subtitle = element_text(hjust = 0.5,size=14),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)),
          plot.margin=unit(c(0.25,1,0.5,0.25),"cm"))+
    geom_density_ridges(aes(x = newlen, y = as.factor(year), height = n),dfaf,fill="#b2182b",stat = "identity",scale =1,alpha=0.5)
  
  pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'FlaggCoveRidgesAllFemale.pdf'),width=11,height=12)
  grid.arrange(f1)
  dev.off()
  
  
  
  
  
  grid.arrange(m1,f1)
  
   
#Calculate Proportion berried and number of each sex annually 

portionsex<- flaggm %>%
  drop_na(sex) %>%
  group_by(year, sex) %>%
  summarise(num=n())

psex<-as.data.frame(portionsex)

effort<-aggregate(area ~ TID + year, data = flaggm, FUN = mean)
sumyr<- aggregate(area~ year, data =effort, FUN=sum)

effortcorrect<-merge(psex, sumyr, all.x=T)
effortcorrect$lpa<-(effortcorrect$num/effortcorrect$area)


portionsex$sex_factor <- factor(portionsex$sex, levels = c( 1, 2, 3))

pal<-wes_palette("Zissou1")[c(5,4,1)]

nsex<-ggplot(portionsex, aes(y =num, x = as.factor(year), fill=as.factor(sex))) + geom_bar(stat= "identity") +
  scale_fill_manual(values=pal, labels = c("Male", "Female", "Berried"))+
  scale_y_continuous(expand = c(0,0), limits=c(0,950))+ 
  labs(x= " ", y = "Number of Lobsters", fill="Sex", size = 12)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour='black'),
        panel.border=element_rect(colour ="black", fill=NA, size=1),
        axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 12))

pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'FlaggCoveNumofLob.pdf'),width=11,height=12)
grid.arrange(nsex)
dev.off()

lobperarea<-ggplot(effortcorrect, aes(y =lpa, x = as.factor(year), fill=as.factor(sex))) + geom_bar(stat= "identity") +
  scale_fill_viridis_d(option= "D",begin = 0, end =0.8, "Sex")+
  scale_y_continuous(expand = c(0,0), limits=c(0,0.1))+ 
  labs(x= " ", y = "Number of Lobsters/m squared")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour='black'),panel.border=element_rect(colour ="black", fill=NA, size=1), 
        axis.text.x = element_text(angle = 90))

pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'FlaggCoveLobsterPerArea.pdf'),width=11,height=12)
grid.arrange(lobperarea)
dev.off()



psex<-ggplot(portionsex, aes(y =num, x = as.factor(year), fill=as.factor(sex))) +geom_bar(stat= "identity", position='fill') +
  #scale_fill_viridis_d(option= "D",begin = 0, end =0.8, "Sex")+
  scale_fill_manual(values=pal, labels = c("Male", "Female", "Berried"))+
  labs(x= " ", y = "Number of Lobsters",fill="Sex", size = 12)+
  scale_y_continuous(expand = c(0,0))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour='black'), axis.text.x = element_text(angle = 90, size = 12), 
        axis.text.y = element_text(size = 12))

pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'FlaggCoveSexProportion.pdf'),width=11,height=12)
grid.arrange(psex)
dev.off()


