setwd("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38")
p = bio.lobster::load.environment()
la()

assessment.year = 2021 ########### check the year ############### !!!!!!!!!!!

p$syr = 1989
p$yrs = p$syr:assessment.year



# define place for figures to go
figdir = file.path("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38")

p$lfas = c("35","36","38") # specify lfa
p$subareas = c("35","36","38") # specify subareas for data summary

# update data through RODBC

lobster.db('scallop.redo')
lobster.db('scallop')


############# Scallop Survey	


scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5)

#stupid little map
LobsterMap('34-38',title="Scallop Survey Tows by Month")
colors = brewer.pal(5,"Dark2")[c(rep(NA,4),1:5,NA,NA)]
points(lat~lon,scalSurv,pch=16,col=colors[month(TOW_DATE)])
legend('bottomright',legend=5:9,col=colors[5:9],pch=16,)


# R1

scalSurv<-ScallopSurveyProcess(size.range=c(70,82.5),bin.size=2.5)
scalSurv<-scalSurv[scalSurv$YEAR <2022,] # 2022 data not complete yet

R1.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
R1.ab.35_19=with(subset(scalSurv,LFA==35&YEAR>1998&YEAR<2020),tapply(LobDen,YEAR,mean,na.rm=T))


R1.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
R1.ab.36_19=with(subset(scalSurv,LFA==36&YEAR>1998&YEAR<2020),tapply(LobDen,YEAR,mean,na.rm=T))


R1.ab.38=with(subset(scalSurv,LFA%in%c(38)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
R1.ab.38_19=with(subset(scalSurv,LFA==38&YEAR>1998&YEAR<2020),tapply(LobDen,YEAR,mean,na.rm=T))


require(dplyr)
RA35<-as.data.frame(R1.ab.35)
RA35<-data.frame(cbind(rownames(RA35), data.frame(RA35, row.names = NULL)))
RA35<-RA35 %>%
  rename(Year = rownames.RA35., 
          Recruit_Abundance = R1.ab.35)

RA36<-as.data.frame(R1.ab.36)
RA36<-data.frame(cbind(rownames(RA36), data.frame(RA36, row.names = NULL)))
RA36<-RA36 %>%
  rename(Year = rownames.RA36., 
         Recruit_Abundance = R1.ab.36)

RA38<-as.data.frame(R1.ab.38)
RA38<-data.frame(cbind(rownames(RA38), data.frame(RA38, row.names = NULL)))
RA38<-RA38 %>%
  rename(Year = rownames.RA38., 
         Recruit_Abundance = R1.ab.38)
##Fill 2004 - where survey data is absent
newrow<-c(2004, NA)
RA38<-rbind(RA38, newrow)

RA38<-RA38[order(RA38$Year),]



#plot scallop surveys

ff = "Figs"
fpf1 = file.path("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/inst/Updates/LFA35-38",ff)


######## lfa 35

png(file=file.path(fpf1,'S35recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(RA35$Year, RA35$Recruit_Abundance, pch=16,xlab='Year',ylab = 'Recruit Abundance')
lines(1999:2019,runmed(R1.ab.35_19,3),lwd=2, col='salmon')
#points(x = 2021, y = 29.697509, pch = 16, col='blue')
dev.off()


######## lfa 36

png(file=file.path(fpf1,'S36recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(RA36$Year, RA36$Recruit_Abundance, pch=16,xlab='Year',ylab = 'Recruit Abundance')
lines(1999:2019,runmed(R1.ab.36_19,3),lwd=2, col='salmon')
#points(x = 2021, y = 29.697509, pch = 16, col='blue')
dev.off()


###### lfa 38

png(file=file.path(fpf1,'S38recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(RA38$Year, RA38$Recruit_Abundance, pch=16,xlab='Year',ylab = 'Recruit Abundance')
lines(1999:2019,runmed(R1.ab.38,3),lwd=2, col='salmon')
#points(x = 2021, y = 29.697509, pch = 16, col='blue')
dev.off()

