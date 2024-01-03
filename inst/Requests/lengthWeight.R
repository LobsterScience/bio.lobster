##Length Weight Request
require(tidyverse)

p = bio.lobster::load.environment()
la()

###MALE 
L=50:155
B = lobLW(L, sex=1)
MLW<-cbind(L,B)
MLW<-as.data.frame(MLW)

MLW$Weight_lbs<-MLW$B/453.6

names(MLW)[names(MLW) == 'L'] <- 'CarapaceLength_mm'
names(MLW)[names(MLW) == 'B'] <- 'Weight_g'

write.csv(MLW,"E:/Nova Scotia/Lobster Job/Requests/MaleLengthWeight.csv" )


##Female
L=50:155
B = lobLW(L, sex=2)
FLW<-cbind(L,B)
FLW<-as.data.frame(FLW)

FLW$Weight_lbs<-FLW$B/453.6

names(FLW)[names(FLW) == 'L'] <- 'CarapaceLength_mm'
names(FLW)[names(FLW) == 'B'] <- 'Weight_g'
write.csv(FLW,"E:/Nova Scotia/Lobster Job/Requests/FemaleLengthWeight.csv" )
