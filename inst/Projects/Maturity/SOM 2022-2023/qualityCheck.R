### QC Maturity data ###
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(SpatialHub)
require(ggspatial)


p = bio.lobster::load.environment()
la()

##Sambro
matSamp33 = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','Sambro2022.csv'))
matSamp33$Y<-convert.dd.dddd(matSamp33$Lat, format = "dec.deg")
matSamp33$X<--(convert.dd.dddd(matSamp33$Long, format = "dec.deg"))
matSamp33$Date = as.Date(matSamp33$Date,"%d-%m-%Y")
matSamp33$mon = month(matSamp33$Date)
matSamp33$year = year(matSamp33$Date)
matSamp33=subset(matSamp33, Sex == 2) #Remove Berried Females
matSamp33=subset(matSamp33, Cement_gland_stage != 'n/a')  #remove individuals where no cement gland was staged

#### set up data for each Maturity indicator --- Check for justification of each Mature (1) or Not (0)
#Pleopod Staging
matSamp33$Pleopod_mat <-ifelse(matSamp33$Cement_gland_stage<2,0,1)
#Ovary Factor
matSamp33$OvaryFac_mat<-ifelse(matSamp33$Ovary_factor<200,0,1)
#Ovary Colour
matSamp33$OvaryCol_mat<-ifelse(matSamp33$Ovary_colour %in% c("dark green","medium green","Medium green"),1,0) 
#Oocyte Size
matSamp33$Oocyte_size_mat<-ifelse(matSamp33$Oocyte_average_mm<0.8,0,1)
matSamp33$OvaryMaturity<-ifelse(matSamp33$OvaryFac_mat == 1 & matSamp33$OvaryCol_mat == 1 & matSamp33$Oocyte_size_mat ==1,1,0)

##########------------------ Data Input ------------------##########
matSamp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','Maturity_2022_2023.csv'))

matSamp$Y = matSamp$Lat
matSamp$X =matSamp$Long

matSamp$Date = as.Date(matSamp$Date,"%d-%m-%Y")
matSamp$mon = month(matSamp$Date)
matSamp$year = year(matSamp$Date)

matSamp=subset(matSamp, Sex == 2) #Remove Berried Females
matSamp=subset(matSamp, Cement_gland_stage != 'n/a')  #remove individuals where no cement gland was staged

##deal with Cement Gland Stage 0


#### set up data for each Maturity indicator --- Check for justification of each Mature (1) or Not (0)
#Pleopod Staging
matSamp$Pleopod_mat <-ifelse(matSamp$Cement_gland_stage<2,0,1)

#Ovary Factor
matSamp$OvaryFac_mat<-ifelse(matSamp$Ovary_factor<200,0,1)
#Ovary Colour
matSamp$OvaryCol_mat<-ifelse(matSamp$Ovary_colour %in% c("dark green","medium green","Medium green"),1,0) 
#Oocyte Size
matSamp$Oocyte_size_mat<-ifelse(matSamp$Oocyte_average_mm<0.8,0,1)


##Ovary Status
matSamp$OvaryMaturity<-ifelse(matSamp$OvaryFac_mat == 1 & matSamp$OvaryCol_mat == 1 & matSamp$Oocyte_size_mat ==1,1,0)


###Do we need to break it out by Ovary Stage or can we just have 0 / 1 for each criteria
##OVARY STAGE 
## Stage 1 : 
#Ovary = white
#OOcytes <0.5mm 
#Ovary Factor <100

##Stage 2: 
#Yellow, Orange, beige or pale green
#Oocytes <0.8mm
#Ovary Factor <100 

##Stage 3:
#Light to medium green
#Oocytes <1.0mm
#Factor <200

##Stage4a:
#medium to dark green
#Oocytes 0.1-1.6mm
#Ovary factor <200

##Stage 4b
#medium to dark green 
#oocytes 0.8-1.6mm
#factor 200-325

#Stage 5
#Dark green
#oocytes 1.0-1.6
#factor: >325

#Stage 6
#Dark Green 
#1.4-1.6
#Factor>400

#Stage 6a

#Stage 7 



####look at sizes
lenfreq1 <- ggplot(data=matSamp,aes(x=Carapace_mm)) +
  geom_histogram(binwidth=5,boundary=0,closed="left",color="black") +
  scale_y_continuous(name="Number of Lobster", limits = c(0,600), expand=c(0,0)) +
  scale_x_continuous(name="Carapace Length (mm)") +
  theme_bw()

 
##Look at locations ##
LobsterMap(ylim=c(42.5,46.5),xlim=c(-67.8,-59))
addPoints(na.omit(matClean[,c('X','Y','EID')]),col='red')


##Combine Data and Export
matClean<-rbind(matSamp, matSamp33)

matClean$EID = 1:nrow(matClean)

write.csv(matClean, file.path(project.datadirectory('bio.lobster'),'data','Maturity','matClean.csv'))


#Summary Stats
 
check<- subset(matSamp,LFA=='36')
check<-subset(check, year==2023)
