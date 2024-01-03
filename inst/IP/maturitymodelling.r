#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)

p = bio.lobster::load.environment()
la()
##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','LobsterMaturityDatabase.csv'))

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

