### GCIFA OGIVES ###
#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(dplyr)
require(statmod)
require(cowplot)
require(tidyr)
require(measurements)

p = bio.lobster::load.environment()
la()
##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','Maturity_GCIFA.csv'))
##########-----------------------------------------------##########
b=a

b= subset(b, Carapace_mm <121)
par(mfrow=c(2,1),oma=c(2,2,0,0),mar=c(4,4,0.5,1))

#### Clean and convert ####
b$Date = as.Date(b$Date,"%d-%b-%y")
b$mon = month(b$Date)
b$year = year(b$Date)


convert_to_decimal_degrees <- function(dmm) {
  dmm <- trimws(dmm)  # Trim whitespace
  parts <- strsplit(dmm, " ")[[1]]  # Split by space
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  decimal_degrees <- degrees + (minutes / 60)
  return(decimal_degrees)
}


b$Latitude_DD <- sapply(b$Latitude, convert_to_decimal_degrees)
b$Longitude_DD <- sapply(b$Longitude, convert_to_decimal_degrees)

b <- b %>%
  rename(Y = Latitude_DD, X = Longitude_DD)

##Subset and determine SOM for LFA 31A

b31a=subset(b, LFA == "31A")

g31a= glm(Pleopod_mat~Carapace_mm,data=b31a,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b31a$Carapace_mm),max(b31a$Carapace_mm),by=.1)
ndata31a <- list(Carapace_mm=l)
ndata31a= glmCIs(g31a,ndata31a)

plot(b31a$Carapace_mm, b31a$Pleopod_mat, pch = 16, xlab = " ", ylab = "Proportion Mature")
lines(ndata31a$Carapace_mm, ndata31a$fit_resp, col="black")
lines(ndata31a$Carapace_mm, ndata31a$upr, lty=2, col="darkgrey") 
lines(ndata31a$Carapace_mm, ndata31a$lwr,lty=2, col="darkgrey")
abline(v=82.5, col="red", lty= 3)
with(ndata31a,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(55, 0.85, paste("LFA 31A"))
#text(55, 0.79, paste("n=348"))
text(55, 0.73,bquote(paste('L'['50']*' = ','74.4')))



###LFA 31B

##Subset and determine SOM for LFA 31A
b31b=subset(b, LFA == "31B")

g31b= glm(Pleopod_mat~Carapace_mm,data=b31b,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b31b$Carapace_mm),max(b31b$Carapace_mm),by=.1)
ndata31b <- list(Carapace_mm=l)
ndata31b= glmCIs(g31b,ndata31b)

plot(b31b$Carapace_mm, b31b$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature")
lines(ndata31b$Carapace_mm, ndata31b$fit_resp, col="black")
lines(ndata31b$Carapace_mm, ndata31b$upr, lty=2, col="darkgrey") 
lines(ndata31b$Carapace_mm, ndata31b$lwr,lty=2, col="darkgrey")
abline(v=82.5, col="red", lty= 3)
with(ndata31b,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(55, 0.85, paste("LFA 31B"))
#text(55, 0.79, paste("n=221"))
text(55, 0.73,bquote(paste('L'['50']*' = ','77.6')))
