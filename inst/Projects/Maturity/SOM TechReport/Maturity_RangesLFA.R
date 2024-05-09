options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(dplyr)
require(statmod)

p = bio.lobster::load.environment()
la()
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles")


##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','LobsterMaturityDatabase.csv'))
#########-----------------------------------------------##########

a$Y = a$Lat
a$X = a$Long*-1
a$EID = 1:nrow(a)
a$Date = as.Date(a$Date,"%d/%m/%Y")
a$mon = month(a$Date)
a$year = year(a$Date)

a=subset(a, Sex == 2) #Remove Berried Females
a=subset(a, Cemgland_stage != 'n/a') 
a=subset(a, mon == 5 | mon == 6)#Filter out July and August

#Pleopod Staging
a$Pleopod_mat <-ifelse(a$Cemgland_stage<2,0,1)

## Add LFAs
a$LFA <- ""
a$LFA <-ifelse(a$Location == "Lobster Bay", 34, a$LFA)
a$LFA <-ifelse(a$Location == "Harrigan Cove" | a$Location == "Tangier"| a$Location == "Mushaboom", 32, a$LFA)
a$LFA <-ifelse(a$Location == "Port Mouton", 33,a$LFA)
a$LFA <-ifelse(a$Location == "Canso", 31, a$LFA)
a = subset(a,Carapace_mm <120)

 
#Pull out Lobster that are mature
pastMat<-a[a$Pleopod_mat ==1,]
pastMat<-pastMat %>% dplyr::select(LFA, year, Carapace_mm)
pastMat$Datasource<-"Silva"

pastMat$som50<-ifelse(pastMat$LFA == 31,77.5,
                      ifelse(pastMat$LFA == 32,92.2, "NA"))
pastMat = subset(pastMat,LFA<33)

##########------------------ Data Input ------------------##########
b = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','matClean.csv'))
##########-----------------------------------------------##########
newMat<-b[b$Pleopod_mat==1,]
newMat = subset(newMat,Carapace_mm <120)
newMat<-newMat %>% dplyr::select(LFA, year, Carapace_mm)
newMat$Datasource<-"LEAT"


newMat$som50<-ifelse(newMat$LFA == 33,90.3,
                      ifelse(newMat$LFA == 36,88.8,
                                    ifelse(newMat$LFA ==38, 88.5, "NA")))
Matresult<-rbind(pastMat, newMat)

Matresult$som50<- as.numeric(Matresult$som50)
Matresult$Carapace_mm<-round(Matresult$Carapace_mm)
#Matresult$LFA[Matresult$LFA ==31]<-"31A"
Matresult$LFA<-as.numeric(Matresult$LFA)
######################Plot it-- modified box plot.... 

matsum<- Matresult%>%
  group_by(LFA)%>%
mutate(centre =som50,lower=min(Carapace_mm), upper=max(Carapace_mm))
matsum<-as.data.frame(matsum)

p<-ggplot(matsum, aes(x=LFA, y = Carapace_mm, fill = as.factor(Datasource)))+
  geom_jitter(width = 0.2, size = 1, alpha = 0.2)+
  
  p<-p+geom_boxplot(aes(x=LFA,lower=lower, upper=upper, middle = som50, ymin=lower, ymax=upper),
                    stat="identity",alpha =0.5)+
  labs(x="LFA", "Carapace Length (mm)")+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(breaks=(seq(min(Matresult$Carapace_mm), max(Matresult$Carapace_mm), by =5)))




