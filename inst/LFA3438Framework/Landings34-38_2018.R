
p = bio.lobster::load.environment()
la()	

require(plyr)
require(ggplot2)
require(gridExtra)




#seasonalland<- lobster.db('season.dates.redo')
#landingsinfo<-lobster.db('logs.redo')
landingslogs<-lobster.db('process.logs')


logset<-subset(landingslogs,LFA == 36 & SYEAR == 2018)

logdate<-aggregate(WEIGHT_KG ~SYEAR,data=logset, FUN=sum)
extralog<-aggregate(WEIGHT_KG ~SYEAR,data=subset(logset, DATE_FISHED >"2018-06-29"), FUN=sum)

#june 30th - July10th

landlogs<-lobster.db('logs')
LandingsUpdate<-lobster.db('seasonal.landings')



#Seasonal Landings for LFA 34
LU34 = LandingsUpdate[,c('SYEAR','LFA34')]
LU34$YR = as.numeric(substr(LU34$SYEAR, 6,9))
LU34 = subset(LU34,YR<=2018,select=c(LFA34,YR))
names(LU34) <- c('Landings','YR')
LU34$TYPE <- 'Seasonal'

LU35 = LandingsUpdate[,c('SYEAR','LFA35')]
LU35$YR = as.numeric(substr(LU35$SYEAR, 6,9))
LU35 = subset(LU35,YR<=2018,select=c(LFA35,YR))
names(LU35) <- c('Landings','YR')
LU35$TYPE <- 'Seasonal'


LU36 = LandingsUpdate[,c('SYEAR','LFA36')]
LU36$YR = as.numeric(substr(LU36$SYEAR, 6,9))
LU36 = subset(LU36,YR<=2018,select=c(LFA36,YR))
names(LU36) <- c('Landings','YR')
LU36$TYPE <- 'Seasonal'
#286387.2/3705014  = 0.0772972 the portion of 2018 landings after the season closed
#After season: 4022tonnes * 0.0772972 = 310.8893 
#Before Season: 3711.111 t
LU36<- LU36[-c(43),]
adrow<- data.frame(Landings = "3711", YR = "2018", TYPE = "Seasonal")
LU36<-rbind(LU36,adrow)
extlog<- data.frame(Landings = "311", YR = "2018", TYPE = "Extension")
LU36<-rbind(LU36,extlog)
LU36$Landings<-as.numeric(LU36$Landings)
LU36$YR<-as.numeric(LU36$YR)

LU38 = LandingsUpdate[,c('SYEAR','LFA38')]
LU38$YR = as.numeric(substr(LU38$SYEAR, 6,9))
LU38 = subset(LU38,YR<=2018,select=c(LFA38,YR))
names(LU38) <- c('Landings','YR')
LU38$TYPE <- 'Seasonal'



###USR AND LRP FOR EACH LFA
#USR34=8867
# Combined Plot
#USR35-38= 1575 

#Subset the data for each LFA grouping and calculate 3 year running means and add column for years

meanyear <- c(1976:2018)
#34
rmed34 =rmed(meanyear,LU34$Landings)
#35
rmed35 =rmed(meanyear,LU35$Landings)
#36
rmed36 =rmed(LU36$YR,LU36$Landings)

#38
rmed38 =rmed(meanyear,LU38$Landings)
#35-38
#rmed3538=rmed(meanyear, Land3538$LandingSum)


#Make the plots
tt=rmed34
#tt$USR <- 8867
tt<-as.data.frame(tt, col.names =c("Year","Median"))
names(tt)[2]<-"Median"


p34<-ggplot() + 
    geom_bar(data=LU34, aes(x=YR,y=Landings),colour = "black",stat='identity', width=1.0, fill = "gray28")+
    ylab("Landings (t)") + xlab(" ")+ ggtitle("LFA 34")+
    scale_y_continuous(limits=c(0,30000),breaks=seq(0, 30000, 10000), expand =c(0.01,0.01)) +
    scale_x_continuous(limits=c(1975,2019), breaks=seq(1976,2018,5), expand =c(0.01,0.01)) +
    geom_line(data=tt,aes(x=Year,y=Median, colour="red"),linetype = 1,lwd=1)+
  scale_colour_manual(labels = c('Median'), values=c("red"))+
   theme(axis.text.x=element_text(size=10, colour='black'),
          panel.border=element_rect(fill=NA,colour='black'),
          axis.text.y=element_text(size=10, colour='black'),
          axis.title.y= element_text(size=15), 
          panel.background = element_rect(fill = "NA"))+
    theme(legend.position= c(0.1,0.8),legend.title =element_blank())

pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'Landings34.pdf'),width=8,height=10)
grid.arrange(p34)
dev.off()

#png(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'Landings34.png'),width=8,height=10,units='in',res=200)
#grid.arrange(p34)
#dev.off()


#Sum the LFA 35-38 together to use the USR
#Lsum3536<- merge(LU35,LU36, by=c("YR", "TYPE"))
#Land3538<- merge(Lsum3536,LU38, by=c("YR", "TYPE"))
#names(Land3538)[3]<-"LFA35"
#names(Land3538)[4]<-"LFA36"
#names(Land3538)[5]<-"LFA38"
#Land3538$LandingSum<- rowSums(Land3538[,c("LFA35", "LFA36", "LFA38")])

#qq=rmed3538
#qq$USR <-1575
#qq<-as.data.frame(qq, col.names =c("Year","Median", "USR"))
#names(qq)[2]<-"Median"

#p3538<-ggplot() + 
#  geom_bar(data=Land3538, aes(x=YR,y=LandingSum),colour = "black",stat='identity', width=1.0, fill = "gray28")+
 # ylab("Landings (t)") + xlab(" ")+ ggtitle("LFA 35 to 38")+
#  scale_y_continuous(limits=c(0,13000),breaks=seq(0, 13000, 1000), expand =c(0.01,0.01)) + 
 # scale_x_continuous(limits=c(1975,2019), breaks=seq(1975,2019,5), expand =c(0.01,0.01)) +
#  geom_line(data=qq,aes(x=Year,y=Median, colour="red"),linetype = 1,lwd=1)+
#  geom_line(data=qq,aes(x=Year,y=USR, colour="dodgerblue"),linetype = 2,lwd=1)+
#  scale_colour_manual(labels = c("USR","Median"), values=c("dodgerblue3", "red"))+
#  theme(axis.text.x=element_text(size=10, colour='black'),
#        panel.border=element_rect(fill=NA,colour='black'),
#        axis.text.y=element_text(size=10, colour='black'),
#        axis.title.y= element_text(size=15), 
 #       panel.background = element_rect(fill = "NA"))+
#  theme(legend.position= c(0.1,0.8),legend.title =element_blank())

#pdf(file.path(project.figuredirectory('bio.lobster'),'CombinedLandings35-38.pdf'),width=8,height=10)
#grid.arrange(p3538)
#dev.off()

#png(file.path(project.figuredirectory('bio.lobster'),'CombinedLandings35-38.png'),width=8,height=10,units='in',res=200)
#grid.arrange(p3538)
#dev.off()

##Separated by LFA
rr=rmed35
rr<-as.data.frame(rr, col.names =c("Year","Median"))

p35<-ggplot() + 
  geom_bar(data=LU35, aes(x=YR,y=Landings),colour = "black",stat='identity', width=1.0, fill = "gray28")+
  ylab("Landings (t)") + xlab(" ")+ ggtitle("LFA 35")+
  scale_y_continuous(limits=c(0,6000),breaks=seq(0, 6000, 500), expand =c(0.01,0.01)) +
  scale_x_continuous(limits=c(1975,2019), breaks=seq(1975,2019,5), expand =c(0.01,0.01)) +
  geom_line(data=rr,aes(x=Year,y=Median, colour="red"),linetype = 1,lwd=1)+
  scale_colour_manual(labels = c("Median"), values=c("red"))+
  theme(axis.text.x=element_text(size=10, colour='black'),
        panel.border=element_rect(fill=NA,colour='black'),
        axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=15), 
        panel.background = element_rect(fill = "NA"))+
  theme(legend.position= c(0.1,0.8),legend.title =element_blank())

pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'Landings35.pdf'),width=8,height=10)
grid.arrange(p35)
dev.off()

ee=rmed36
ee<-as.data.frame(ee, col.names =c("Year","Median"))
ee<-ee[-c(44),]


p36<-ggplot() + 
  geom_bar(data=LU36, aes(x=YR, y=Landings, fill=TYPE),stat='identity', colour = 'black' ,width=1.0)+
  ylab("Landings (t)") + xlab(" ") + ggtitle("LFA 36")+
  scale_y_continuous(limits=c(0,6000),breaks=seq(0, 6000, 500), expand =c(0.01,0.01)) +
  scale_x_continuous(limits=c(1975,2019), breaks=seq(1975,2019,5), expand =c(0.01,0.01)) +
  geom_line(data=ee,aes(x=Year,y=Median, colour="red"),linetype = 1,lwd=1)+
  scale_colour_manual(labels = c("Median"), values=c("red"))+
  scale_fill_manual(values = c("darkturquoise", "grey28"))+
  theme(axis.text.x=element_text(size=10, colour='black'),
        panel.border=element_rect(fill=NA,colour='black'),
        axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=15), 
        panel.background = element_rect(fill = "NA"))+
  theme(legend.position= c(0.1,0.8),legend.title =element_blank())

pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'Landings36.pdf'),width=8,height=10)
grid.arrange(p36)
dev.off()

ww=rmed38
ww<-as.data.frame(ww, col.names =c("Year","Median"))

p38<-ggplot() + 
  geom_bar(data=LU38, aes(x=YR,y=Landings),colour = "black",stat='identity', width=1.0, fill = "gray28")+
  ylab("Landings (t)") + xlab(" ")+ ggtitle("LFA 38")+
  scale_y_continuous(limits=c(0,6000),breaks=seq(0, 6000, 500), expand =c(0.01,0.01)) +
  scale_x_continuous(limits=c(1975,2019), breaks=seq(1975,2019,5), expand =c(0.01,0.01)) +
  geom_line(data=ww,aes(x=Year,y=Median, colour="red"),linetype = 1,lwd=1)+
  scale_colour_manual(labels = c("Median"), values=c("red"))+
  theme(axis.text.x=element_text(size=10, colour='black'),
        panel.border=element_rect(fill=NA,colour='black'),
        axis.text.y=element_text(size=10, colour='black'),
        axis.title.y= element_text(size=15), 
        panel.background = element_rect(fill = "NA"))+
  theme(legend.position= c(0.1,0.8),legend.title =element_blank())



pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'Landings38.pdf'),width=8,height=10)
grid.arrange(p38)
dev.off()



pdf(file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019",'Landings3438.pdf'),width=11,height=12)
grid.arrange(p34,p35,p36,p38)
dev.off()
