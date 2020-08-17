####### Update 27-33 2017:
p = bio.lobster::load.environment()
la()	

require(plyr)
require(ggplot2)
require(gridExtra)

# Landings
annualLandings<-lobster.db('annual.landings')
LR = as.data.frame(reshape(annualLandings[,c("YR","LFA27",  "LFA28",  "LFA29",  "LFA30",  "LFA31",  "LFA32",  "LFA33")],
                           varying=c("LFA27",  "LFA28",  "LFA29",  "LFA30",  "LFA31",  "LFA32",  "LFA33"),v.names='Landings',timevar='LFA',times=c("LFA27",  "LFA28",  "LFA29",  "LFA30", "LFA31",  "LFA32",  "LFA33"), direction='long'))
attr(LR,'reshapeLong') <- NULL
LR$LFA1 <- bio.utilities::recode(LR$LFA,c("'LFA28'='LFA28.32';'LFA29'='LFA28.32';'LFA30'='LFA28.32'; 'LFA31'='LFA28.32';'LFA32'='LFA28.32'"))
L =aggregate(Landings~LFA1+YR,data=LR,FUN=sum)

LandingsUpdate<-lobster.db('seasonal.landings')
LU = LandingsUpdate[,c('SYEAR','LFA33')]
LU$YR = as.numeric(substr(LU$SYEAR, 6,9))
L3 = subset(L,LFA1=='LFA33' & YR<=1975)
LU = subset(LU,YR<=2017,select=c(LFA33,YR))
names(LU) <- c('Landings','YR')
LU$TYPE <- 'Seasonal'
L3 = L3[,c('Landings','YR')]
L3$TYPE = 'Annual'
LU = rbind(L3,LU)

#USR27=1629
#LRP27=814.5

#USR28.32=688
#LRP28.32=344

#USR33=1838
#LRP33=919

  
#Subset the data for each LFA grouping and calculate 3 year running means and add column for years

meanyear <- c(1949:2017)

L27 = subset(L,LFA1=='LFA27')
rmean27 = apply(embed(L27$Landings,3),1,mean)
mean27<-do.call(rbind, Map(data.frame, Year = meanyear, landingsMean = rmean27))

L28.32=subset(L,LFA1=='LFA28.32')
rmean28.32=apply(embed(L28.32$Landings,3),1,mean)
mean28.32<-do.call(rbind, Map(data.frame, Year = meanyear, landingsMean = rmean28.32))

L33=LU
rmean33=apply(embed(L33$Landings,3),1,mean)
mean33<- data.frame(Year = meanyear, landingsMean = rmean33)

#Make the plots

tt=mean27
tt$USR <- 1629
tt$LRP <- 814.5
colnames(tt)[grep("landings",colnames(tt))] <- "3 Year Mean"

meandata27 <- reshape2::melt(tt,id.vars="Year")
meandata27$variable <- factor(meandata27$variable,levels=c("LRP","USR","3 Year Mean"))

p27<-ggplot() + 
    geom_bar(data=L27, aes(x=YR,y=Landings),stat='identity', width=1.0, fill = "black")+
    ylab("Landings (t)") + xlab(" ")+ ggtitle("LFA27")+
    scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000), expand =c(0.01,0.01)) +
    scale_x_continuous(limits=c(1947,2018), breaks=seq(1947,2018,5), expand =c(0.01,0.01)) +
    geom_line(data=meandata,aes(x=Year,y=value,colour=variable,linetype = variable),lwd=1)+
    theme(axis.text.x=element_text(size=10, colour='black'),
          panel.border=element_rect(fill=NA,colour='black'),
          axis.text.y=element_text(size=10, colour='black'),
          axis.title.y= element_text(size=15), 
          panel.background = element_rect(fill = "NA"))+
    scale_colour_manual(values=c("seagreen2","dodgerblue3","red"))+
    theme(legend.position= c(0.1,0.8),legend.title =element_blank())
  

rr=mean28.32
rr$USR <- 688
rr$LRP <- 344
colnames(rr)[grep("landings",colnames(rr))] <- "3 Year Mean"
meandata28.32 <- reshape2::melt(rr,id.vars="Year")
meandata28.32$variable <- factor(meandata28.32$variable,levels=c("LRP","USR","3 Year Mean"))

p28.32<-ggplot(L28.32, aes(x=YR,y=Landings)) + geom_bar(stat='identity', width=1.0,fill = "black")+
        ylab("Landings (t)") + xlab(" ")+ ggtitle("LFAs 28-32")+
        scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000), expand =c(0.01,0.01)) + 
        scale_x_continuous(limits=c(1947,2018), breaks=seq(1947,2018,5), expand =c(0.01,0.01)) +
        geom_line(data=meandata28.32,aes(x=Year,y=value,colour=variable,linetype = variable),lwd=1)+
        theme(axis.text.x=element_text(size=10, colour='black'),
              panel.border=element_rect(fill=NA,colour='black'),
              axis.text.y=element_text(size=10, colour='black'),
              axis.title.y= element_text(size=15),
              panel.background = element_rect(fill = "NA"))+
  scale_colour_manual(values=c("seagreen2","dodgerblue3","red"))+
  theme(legend.position= c(0.1,0.8),legend.title =element_blank())
p28.32


ee=mean33
ee$USR <- 1838
ee$LRP <- 919
colnames(ee)[grep("landings",colnames(ee))] <- "3 Year Mean"
meandata33 <- reshape2::melt(ee,id.vars="Year")
meandata33$variable <- factor(meandata33$variable,levels=c("LRP","USR","3 Year Mean"))

p33<-ggplot(L33, aes(x=YR,y=Landings)) + geom_bar(stat='identity', width=1.0,fill = "black")+
     ylab("Landings (t)") + xlab(" ")+ ggtitle("LFA 33")+
     scale_y_continuous(limits=c(0,11000),breaks=seq(0, 11000, 1000), expand =c(0.01,0.01)) + 
     scale_x_continuous(limits=c(1947,2018), breaks=seq(1947,2018,5), expand =c(0.01,0.01)) +
     geom_line(data=meandata33,aes(x=Year,y=value,colour=variable,linetype = variable),lwd=1)+
     theme(axis.text.x=element_text(size=10, colour='black'),
          panel.border=element_rect(fill=NA,colour='black'),
          axis.text.y=element_text(size=10, colour='black'),
          axis.title.y= element_text(size=15),
          panel.background = element_rect(fill = "NA"))+
     scale_colour_manual(values=c("seagreen2","dodgerblue3","red"))+
     theme(legend.position= c(0.1,0.8),legend.title =element_blank())
p33


#pdf(file.path(project.figuredirectory('bio.lobster'),'Commercial.Landings.LFA27-33.2018.pdf'),width=8,height=10)

#grid.arrange(p27,p28.32,p33,ncol=1)  
#dev.off()

png(file.path(project.figuredirectory('bio.lobster'),'Commercial.Landings.LFA27-33.2018.png'),width=8,height=10,units='in',res=200)
grid.arrange(p27,p28.32,p33,ncol=1)  
dev.off()
