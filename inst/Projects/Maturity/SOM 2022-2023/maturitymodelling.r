#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)

p = bio.lobster::load.environment()
la()
##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','LobsterMaturityDatabase.csv'))
a2 = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','Maturity_2022_2023.csv'))

##########-----------------------------------------------##########

 a$Y = a$Lat
 a$X = a$Long*-1
 a$EID = 1:nrow(a)
 a$Date = as.Date(a$Date,"%d/%m/%Y")
 a$mon = month(a$Date)
 a$year = year(a$Date)

 LobsterMap('SWN')
 addPoints(na.omit(a[,c('X','Y','EID')]),col='red')

 b = subset(a,Location=='Lobster Bay')
 
b$Mat = ifelse(b$Cemgland_stage<2,0,1)
b = subset(b,Carapace_mm<120 & Carapace_mm>60)
g = glm(Mat~Carapace_mm,data=subset(b,mon %in% 6:7 & Sex==2),family=binomial(link='logit'))

plot(b$Carapace_mm, b$Mat, pch = 16, xlab = "Carapace Length", ylab = "Maturity")
l = seq(min(b$Carapace_mm),max(b$Carapace_mm),by=.1)
mm =predict(g,list(Carapace_mm=l),type='response')
lines(l,mm)
l[which.min(abs(mm-.5))]



#correct CI's
ndata <- list(Carapace_mm=l)
ndata = glmCIs(g,ndata)

plot(b$Carapace_mm, b$Mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Maturity")
lines(ndata$Carapace_mm, ndata$fit_resp)
lines(ndata$Carapace_mm, ndata$upr, lty=2) 
lines(ndata$Carapace_mm, ndata$lwr,lty=2)

with(ndata,{
	print(Carapace_mm[which.min(abs(fit.resp-.5))])
	 print(Carapace_mm[which.min(abs(upr-.5))])
	 print(Carapace_mm[which.min(abs(lwr-.5))])
})	


########################################### New Data #########################################


a2$Y = a2$Lat
a2$X =a2$Long
a2$EID = 1:nrow(a2)
a2$Date = as.Date(a2$Date,"%d-%m-%Y")
a2$mon = month(a2$Date)
a2$year = year(a2$Date)

b=subset(a2, Sex == 2)

b = subset(a2,year=='2023')
b = subset(b,LFA=='38')

LobsterMap('BoF')
addPoints(na.omit(b[,c('X','Y','EID')]),col='red')


b$Mat = ifelse(b$Cement_gland_stage<2,0,1)
#b = subset(b,Carapace_mm<100 & Carapace_mm>60)
g = glm(Mat~Carapace_mm,data=subset(b,mon %in% 5:7 & Sex==2),family=binomial(link='logit')) ##CHECK what Months you're sampling


plot(b$Carapace_mm, b$Mat, pch = 16, xlab = "Carapace Length", ylab = "Maturity")
l = seq(min(b$Carapace_mm),max(b$Carapace_mm),by=.1)
mm =predict(g,list(Carapace_mm=l),type='response')
lines(l,mm)
l[which.min(abs(mm-.5))]



#correct CI's
ndata <- list(Carapace_mm=l)
ndata = glmCIs(g,ndata)

plot(b$Carapace_mm, b$Mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Maturity")
lines(ndata$Carapace_mm, ndata$fit_resp)
lines(ndata$Carapace_mm, ndata$upr, lty=2) 
lines(ndata$Carapace_mm, ndata$lwr,lty=2)

with(ndata,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	



## add Column if " Overall " Ovarian Maturity - all 3 criteria must be met.


## Boxplot of Ovary status
#X axis is Ovary maturity stage
