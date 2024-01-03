#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(tidyr)
require(statmod)
require(cowplot)

p = bio.lobster::load.environment()
la()
##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','matClean.csv'))
##########-----------------------------------------------##########
b=a
par(mfrow=c(3,1),oma=c(0,2,0,0),mar=c(4,4,1,1))

b= subset(b, Carapace_mm <120)
g = glm(Pleopod_mat~Carapace_mm,data=b,family=binomial(link='logit')) 
#correct CI's
l = seq(min(b$Carapace_mm),max(b$Carapace_mm),by=.1)
ndata <- list(Carapace_mm=l)
ndata = glmCIs(g,ndata)

plot(b$Carapace_mm, b$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature")
lines(ndata$Carapace_mm, ndata$fit_resp, col="black")
lines(ndata$Carapace_mm, ndata$upr, lty=2, col="darkgrey") 
lines(ndata$Carapace_mm, ndata$lwr,lty=2, col="darkgrey")

with(ndata,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(60, 0.85, paste("LFA 36 & 38 "))
text(60, 0.8,bquote(paste('L'['50']*' = ','88.5')))

### LFA 36
b36 = subset(b,LFA=='36')
b36= subset(b36, Carapace_mm <120)
g36 = glm(Pleopod_mat~Carapace_mm,data=b36,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b36$Carapace_mm),max(b36$Carapace_mm),by=.1)
ndata36 <- list(Carapace_mm=l)
ndata36 = glmCIs(g36,ndata36)

plot(b36$Carapace_mm, b36$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature")
lines(ndata36$Carapace_mm, ndata36$fit_resp, col="black")
lines(ndata36$Carapace_mm, ndata36$upr, lty=2, col="darkgrey") 
lines(ndata36$Carapace_mm, ndata36$lwr,lty=2, col="darkgrey")

with(ndata36,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(60, 0.85, paste("LFA 36"))
text(60, 0.8,bquote(paste('L'['50']*' = ','88.8')))

### LFA 38
b38 = subset(b,LFA=='38')
b38= subset(b38, Carapace_mm <120)
g38 = glm(Pleopod_mat~Carapace_mm,data=b38,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b38$Carapace_mm),max(b38$Carapace_mm),by=.1)
ndata38 <- list(Carapace_mm=l)
ndata38 = glmCIs(g38,ndata38)

plot(b38$Carapace_mm, b38$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature")
lines(ndata38$Carapace_mm, ndata38$fit_resp, col="black")
lines(ndata38$Carapace_mm, ndata38$upr, lty=2, col="darkgrey") 
lines(ndata38$Carapace_mm, ndata38$lwr,lty=2, col="darkgrey")

with(ndata38,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(60, 0.85, paste("LFA 38"))
text(60, 0.8,bquote(paste('L'['50']*' = ','88.5')))

dev.off()


### LFA 33
b33 = subset(b,LFA=='33')
b33= subset(b33, Carapace_mm <120)
g33 = glm(Pleopod_mat~Carapace_mm,data=b33,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b33$Carapace_mm),max(b33$Carapace_mm),by=.1)
ndata33 <- list(Carapace_mm=l)
ndata33 = glmCIs(g33,ndata33)

plot(b33$Carapace_mm, b33$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature")
lines(ndata33$Carapace_mm, ndata33$fit_resp, col="black")
lines(ndata33$Carapace_mm, ndata33$upr, lty=2, col="darkgrey") 
lines(ndata33$Carapace_mm, ndata33$lwr,lty=2, col="darkgrey")

with(ndata33,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(75, 0.85, paste("LFA 33"))
text(75, 0.8,bquote(paste('L'['50']*' = ','90.3')))

dev.off()
