options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(dplyr)
require(statmod)

#p = bio.lobster::load.environment()
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

a = subset(a,Carapace_mm <121)


b=a
par(mfrow=c(2,1),oma=c(2,2,0,0),mar=c(4,4,0.5,1))

############### LFA 31A ##################
b31 = subset(b,LFA == 31)
g31 = glm(Pleopod_mat~Carapace_mm,data=b31,family=binomial(link='logit'))

#correct CI's
l = seq(min(b31$Carapace_mm),max(b31$Carapace_mm),by=.1)
ndata31 <- list(Carapace_mm=l)
ndata31 = glmCIs(g31,ndata31)

plot(b31$Carapace_mm, b31$Pleopod_mat, pch = 16, xlab = " ", ylab = "Proportion Mature",xlim=c(50,120))
lines(ndata31$Carapace_mm, ndata31$fit_resp)
lines(ndata31$Carapace_mm, ndata31$upr, lty=2) 
lines(ndata31$Carapace_mm, ndata31$lwr,lty=2)
abline(v=82.5, col="red", lty= 3)
with(ndata31,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 31A"))
text(55, 0.75,bquote(paste('L'['50']*' = ','77.5')))


# 
# #pull out data 
# b31<- b31%>%
#   dplyr::select(Carapace_mm,Pleopod_mat)
# 
# ## to get a and b from nls binned data
# b31$rcl=round(b31$Carapace_mm/3)*3+1
# b31$co = 1
# b31im = aggregate(co~rcl,data=subset(b31,Pleopod_mat==0),FUN=sum)
# names(b31im)[2] = 'n_imm'
# b31mm = aggregate(co~rcl,data=subset(b31,Pleopod_mat==1),FUN=sum)
# names(b31mm)[2] = 'n_mat'
# 
# b31p=merge(b31mm, b31im, all=T)
# b31p = bio.utilities::na.zero(b31p)
# b31p$n=(b31p$n_mat+b31p$n_imm)
# 
# b31p$p = b31p$n_mat/(b31p$n_mat+b31p$n_imm)
# m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
# b31nlsp<-nls(m1,data=b31p,start=list(a=0.8,b=-.05), weights=b31p$n)
# Propmatp<-predict(b31nlsp,newdata=seq(1,9,1))
# 
# plot(b31p$rcl,b31p$p)
# lines(b31p$rcl,Propmatp)



################### LFA 32 #########################
b32 = subset(b,LFA == 32)
g32 = glm(Pleopod_mat~Carapace_mm,data=b32,family=binomial(link='logit'))

#correct CI's
l = seq(min(b32$Carapace_mm),max(b32$Carapace_mm),by=.1)
ndata32 <- list(Carapace_mm=l)
ndata32 = glmCIs(g32,ndata32)

plot(b32$Carapace_mm, b32$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm) ", ylab = "Proportion Mature ",xlim=c(50,120))
lines(ndata32$Carapace_mm, ndata32$fit_resp)
lines(ndata32$Carapace_mm, ndata32$upr, lty=2) 
lines(ndata32$Carapace_mm, ndata32$lwr,lty=2)
abline(v=82.5, col="red", lty= 3)
with(ndata32,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 32"))
text(55, 0.75,bquote(paste('L'['50']*' = ','92.2')))

# 
# 
# #pull out data 
# b32<- b32%>%
#   dplyr::select(Carapace_mm,Pleopod_mat)
# 
# ## to get a and b from nls binned data
# b32$rcl=round(b32$Carapace_mm/3)*3+1
# b32$co = 1
# b32im = aggregate(co~rcl,data=subset(b32,Pleopod_mat==0),FUN=sum)
# names(b32im)[2] = 'n_imm'
# b32mm = aggregate(co~rcl,data=subset(b32,Pleopod_mat==1),FUN=sum)
# names(b32mm)[2] = 'n_mat'
# 
# b32p=merge(b32mm, b32im, all=T)
# b32p = bio.utilities::na.zero(b32p)
# b32p$n=(b32p$n_mat+b32p$n_imm)
# 
# b32p$p = b32p$n_mat/(b32p$n_mat+b32p$n_imm)
# m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
# b32nlsp<-nls(m1,data=b32p,start=list(a=2,b=-.05), weights=b32p$n)
# Propmatp<-predict(b32nlsp,newdata=seq(1,9,1))
# 
# plot(b32p$rcl,b32p$p)
# lines(b32p$rcl,Propmatp)



############  LFA 33 ####################
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
abline(v=82.5, col="red", lty= 3)
with(ndata33,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 33"))
text(55, 0.75,bquote(paste('L'['50']*' = ','97.6')))

#pull out data 
b33<- b33%>% dplyr::select(Carapace_mm,Pleopod_mat)

## to get a and b from nls binned data
b33$rcl=round(b33$Carapace_mm/3)*3+1
b33$co = 1
b33im = aggregate(co~rcl,data=subset(b33,Pleopod_mat==0),FUN=sum)
names(b33im)[2] = 'n_imm'
b33mm = aggregate(co~rcl,data=subset(b33,Pleopod_mat==1),FUN=sum)
names(b33mm)[2] = 'n_mat'

b33p=merge(b33mm, b33im, all=T)
b33p = bio.utilities::na.zero(b33p)
b33p$n=(b33p$n_mat+b33p$n_imm)

b33p$p = b33p$n_mat/(b33p$n_mat+b33p$n_imm)
m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
b33nlsp<-nls(m1,data=b33p,start=list(a=0.8,b=-.05), weights=b33p$n)
Propmatp<-predict(b33nlsp,newdata=seq(1,9,1))

plot(b33p$rcl,b33p$p)
lines(b33p$rcl,Propmatp)






################# LFA 34 ######################
b34 = subset(b,LFA == 34)

g34 = glm(Pleopod_mat~Carapace_mm,data=b34,family=binomial(link='logit'))

#correct CI's
l = seq(min(b34$Carapace_mm),max(b34$Carapace_mm),by=.1)
ndata34 <- list(Carapace_mm=l)
ndata34 = glmCIs(g34,ndata34)

plot(b34$Carapace_mm, b34$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = " ",xlim=c(50,150))
lines(ndata34$Carapace_mm, ndata34$fit_resp)
lines(ndata34$Carapace_mm, ndata34$upr, lty=2) 
lines(ndata34$Carapace_mm, ndata34$lwr,lty=2)
abline(v=82.5, col="red", lty= 3)
with(ndata34,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.85, paste("LFA 34"))
text(55, 0.75,bquote(paste('L'['50']*' = ','102')))


#pull out data 
b34<- b34%>%
  select(Carapace_mm, Pleopod_mat)

## to get a and b from nls binned data
b34$rcl=round(b34$Carapace_mm/3)*3+1


b34$co = 1
b34im = aggregate(co~rcl,data=subset(b34,Pleopod_mat==0),FUN=sum)
names(b34im)[2] = 'n_imm'
b34mm = aggregate(co~rcl,data=subset(b34,Pleopod_mat==1),FUN=sum)
names(b34mm)[2] = 'n_mat'


b34p=merge(b34mm, b34im, all=T)
b34p = bio.utilities::na.zero(b34p)
b34p$n=(b34p$n_mat+b34p$n_imm)


b34p$p = b34p$n_mat/(b34p$n_mat+b34p$n_imm)
m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
b34nlsp<-nls(m1,data=b34p,start=list(a=0.8,b=-0.05), weights=b34p$n)
Propmatp<-predict(b34nlsp,newdata=seq(1,9,1))

plot(b34p$rcl,b34p$p, ylim = c(0,1))
lines(b34p$rcl,Propmatp)




