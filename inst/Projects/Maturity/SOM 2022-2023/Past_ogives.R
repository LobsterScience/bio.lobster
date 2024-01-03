options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(tidyr)
require(statmod)

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


b=a
par(mfrow=c(2,2),oma=c(0,2,0,0),mar=c(4,4,1,1))

###LFA 31A
b31 = subset(b,LFA == 31)
g31 = glm(Pleopod_mat~Carapace_mm,data=b31,family=binomial(link='logit'))

#correct CI's
l = seq(min(b31$Carapace_mm),max(b31$Carapace_mm),by=.1)
ndata31 <- list(Carapace_mm=l)
ndata31 = glmCIs(g31,ndata31)

plot(b31$Carapace_mm, b31$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature",xlim=c(50,120))
lines(ndata31$Carapace_mm, ndata31$fit_resp)
lines(ndata31$Carapace_mm, ndata31$upr, lty=2) 
lines(ndata31$Carapace_mm, ndata31$lwr,lty=2)

with(ndata31,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 31A"))
text(55, 0.8,bquote(paste('L'['50']*' = ','77.5')))

##LFA 32
b32 = subset(b,LFA == 32)
g32 = glm(Pleopod_mat~Carapace_mm,data=b32,family=binomial(link='logit'))

#correct CI's
l = seq(min(b32$Carapace_mm),max(b32$Carapace_mm),by=.1)
ndata32 <- list(Carapace_mm=l)
ndata32 = glmCIs(g32,ndata32)

plot(b32$Carapace_mm, b32$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature",xlim=c(50,120))
lines(ndata32$Carapace_mm, ndata32$fit_resp)
lines(ndata32$Carapace_mm, ndata32$upr, lty=2) 
lines(ndata32$Carapace_mm, ndata32$lwr,lty=2)

with(ndata32,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 32"))
text(55, 0.8,bquote(paste('L'['50']*' = ','92.2')))

##LFA 33
b33 = subset(b,LFA == 33)
g33 = glm(Pleopod_mat~Carapace_mm,data=b33,family=binomial(link='logit'))

#correct CI's
l = seq(min(b33$Carapace_mm),max(b33$Carapace_mm),by=.1)
ndata33 <- list(Carapace_mm=l)
ndata33 = glmCIs(g33,ndata33)

plot(b33$Carapace_mm, b33$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature",xlim=c(50,120))
lines(ndata33$Carapace_mm, ndata33$fit_resp)
lines(ndata33$Carapace_mm, ndata33$upr, lty=2) 
lines(ndata33$Carapace_mm, ndata33$lwr,lty=2)

with(ndata33,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 33"))
text(55, 0.8,bquote(paste('L'['50']*' = ','97.6')))

####LFA 34
b34 = subset(b,LFA == 34)

g34 = glm(Pleopod_mat~Carapace_mm,data=b34,family=binomial(link='logit'))

#correct CI's
l = seq(min(b34$Carapace_mm),max(b34$Carapace_mm),by=.1)
ndata34 <- list(Carapace_mm=l)
ndata34 = glmCIs(g34,ndata34)

plot(b34$Carapace_mm, b34$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature",xlim=c(50,120))
lines(ndata34$Carapace_mm, ndata34$fit_resp)
lines(ndata34$Carapace_mm, ndata34$upr, lty=2) 
lines(ndata34$Carapace_mm, ndata34$lwr,lty=2)

with(ndata34,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 34"))
text(55, 0.8,bquote(paste('L'['50']*' = ','102')))
