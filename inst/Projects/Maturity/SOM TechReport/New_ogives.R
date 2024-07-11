#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(dplyr)
require(statmod)
require(cowplot)
require(tidyr)

p = bio.lobster::load.environment()
la()
##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','matClean.csv'))
##########-----------------------------------------------##########
b=a
b=subset(b,Sex == 2)
b= subset(b, Carapace_mm <121)

par(mfrow=c(2,2),oma=c(2,2,0,0),mar=c(4,4,0.5,1))


######################## LFA 33 #############################
b33 = subset(b,LFA=='33')

g33 = glm(Pleopod_mat~Carapace_mm,data=b33,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b33$Carapace_mm),max(b33$Carapace_mm),by=.1)
ndata33 <- list(Carapace_mm=l)
ndata33 = glmCIs(g33,ndata33)

plot(b33$Carapace_mm, b33$Pleopod_mat, pch = 16, xlab = " ", ylab = "Proportion Mature", xlim=c(50,120))
lines(ndata33$Carapace_mm, ndata33$fit_resp, col="black")
lines(ndata33$Carapace_mm, ndata33$upr, lty=2, col="darkgrey") 
lines(ndata33$Carapace_mm, ndata33$lwr,lty=2, col="darkgrey")
abline(v=82.5, col="red", lty= 3)
with(ndata33,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(60, 0.85, paste("LFA 33"))
text(60, 0.75,bquote(paste('L'['50']*' = ','85.2')))




######### to get a and b from nls binned data
#pull out data 
# b33<- b33%>%
#   dplyr::select(Carapace_mm,Pleopod_mat)


# b33$rcl=round(b33$Carapace_mm/3)*3+1
# b33$co = 1
# b33im = aggregate(co~rcl,data=subset(b33,Pleopod_mat==0),FUN=sum)
# names(b33im)[2] = 'n_imm'
# b33mm = aggregate(co~rcl,data=subset(b33,Pleopod_mat==1),FUN=sum)
# names(b33mm)[2] = 'n_mat'
# 
# b33p=merge(b33mm, b33im, all=T)
# b33p = bio.utilities::na.zero(b33p)
# b33p$n=(b33p$n_mat+b33p$n_imm)
# 
# b33p$p = b33p$n_mat/(b33p$n_mat+b33p$n_imm)
# m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
# b33nlsp<-nls(m1,data=b33p,start=list(a=2,b=-.05), weights=b33p$n)
# Propmatp<-predict(b33nlsp,newdata=seq(1,9,1))
# 
# plot(b33p$rcl,b33p$p, ylim = c(0,1))
# lines(b33p$rcl,Propmatp)


######################## LFA 35 #############################
b35 = subset(b,LFA=='35')
g35 = glm(Pleopod_mat~Carapace_mm,data=b35,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b35$Carapace_mm),max(b35$Carapace_mm),by=.1)
ndata35 <- list(Carapace_mm=l)
ndata35 = glmCIs(g35,ndata35)

plot(b35$Carapace_mm, b35$Pleopod_mat, pch = 16, xlab = " ", ylab = " ", xlim=c(50,120))
lines(ndata35$Carapace_mm, ndata35$fit_resp, col="black")
lines(ndata35$Carapace_mm, ndata35$upr, lty=2, col="darkgrey") 
lines(ndata35$Carapace_mm, ndata35$lwr,lty=2, col="darkgrey")
abline(v=82.5, col="red", lty= 3)
with(ndata35,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(60, 0.85, paste("LFA 35"))
text(60, 0.75,bquote(paste('L'['50']*' = ','83.3')))




########## to get a and b from nls binned data
# #pull out data 
# b35<- b35%>%
#   dplyr::select(Carapace_mm,Pleopod_mat)
# b35$rcl=round(b35$Carapace_mm/3)*3+1
# b35$co = 1
# b35im = aggregate(co~rcl,data=subset(b35,Pleopod_mat==0),FUN=sum)
# names(b35im)[2] = 'n_imm'
# b35mm = aggregate(co~rcl,data=subset(b35,Pleopod_mat==1),FUN=sum)
# names(b35mm)[2] = 'n_mat'
# 
# b35p=merge(b35mm, b35im, all=T)
# b35p = bio.utilities::na.zero(b35p)
# b35p$n=(b35p$n_mat+b35p$n_imm)
# 
# b35p$p = b35p$n_mat/(b35p$n_mat+b35p$n_imm)
# m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
# b35nlsp<-nls(m1,data=b35p,start=list(a=2,b=-.05), weights=b35p$n)
# Propmatp<-predict(b35nlsp,newdata=seq(1,9,1))
# 
# plot(b35p$rcl,b35p$p, ylim = c(0,1))
# lines(b35p$rcl,Propmatp)

#################### LFA 36 ######################
b36 = subset(b,LFA=='36')
g36 = glm(Pleopod_mat~Carapace_mm,data=b36,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b36$Carapace_mm),max(b36$Carapace_mm),by=.1)
ndata36 <- list(Carapace_mm=l)
ndata36 = glmCIs(g36,ndata36)

plot(b36$Carapace_mm, b36$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature", xlim=c(50,120))
lines(ndata36$Carapace_mm, ndata36$fit_resp, col="black")
lines(ndata36$Carapace_mm, ndata36$upr, lty=2, col="darkgrey") 
lines(ndata36$Carapace_mm, ndata36$lwr,lty=2, col="darkgrey")
abline(v=82.5, col="red", lty= 3)
with(ndata36,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(60, 0.85, paste("LFA 36"))
text(60, 0.75,bquote(paste('L'['50']*' = ','88.8')))



########## to get a and b from nls binned data
#pull out data 
# b36<- b36%>%
#   dplyr::select(Carapace_mm,Pleopod_mat)

# b36$rcl=round(b36$Carapace_mm/3)*3+1
# b36$co = 1
# b36im = aggregate(co~rcl,data=subset(b36,Pleopod_mat==0),FUN=sum)
# names(b36im)[2] = 'n_imm'
# b36mm = aggregate(co~rcl,data=subset(b36,Pleopod_mat==1),FUN=sum)
# names(b36mm)[2] = 'n_mat'
# 
# b36p=merge(b36mm, b36im, all=T)
# b36p = bio.utilities::na.zero(b36p)
# b36p$n=(b36p$n_mat+b36p$n_imm)
# 
# b36p$p = b36p$n_mat/(b36p$n_mat+b36p$n_imm)
# m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
# b36nlsp<-nls(m1,data=b36p,start=list(a=0.8,b=-.05), weights=b36p$n)
# Propmatp<-predict(b36nlsp,newdata=seq(1,9,1))
# 
# 
# 
# plot(b36p$rcl,b36p$p)
# lines(b36p$rcl,Propmatp)


########################### LFA 38 ###############################
b38 = subset(b,LFA=='38')
g38 = glm(Pleopod_mat~Carapace_mm,data=b38,family=binomial(link='logit')) ##CHECK what Months you're sampling

#correct CI's
l = seq(min(b38$Carapace_mm),max(b38$Carapace_mm),by=.1)
ndata38 <- list(Carapace_mm=l)
ndata38 = glmCIs(g38,ndata38)

plot(b38$Carapace_mm, b38$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = " ", xlim=c(50,120))
lines(ndata38$Carapace_mm, ndata38$fit_resp, col="black")
lines(ndata38$Carapace_mm, ndata38$upr, lty=2, col="darkgrey") 
lines(ndata38$Carapace_mm, ndata38$lwr,lty=2, col="darkgrey")
abline(v=82.5, col="red", lty= 3)
with(ndata38,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(60, 0.85, paste("LFA 38"))
text(60, 0.75,bquote(paste('L'['50']*' = ','88.5')))





####### to get a and b from nls binned data
# #pull out data 
# b38<- b38%>%
#   dplyr::select(Carapace_mm,Pleopod_mat)

# b38$rcl=round(b38$Carapace_mm/3)*3+1
# b38$co = 1
# b38im = aggregate(co~rcl,data=subset(b38,Pleopod_mat==0),FUN=sum)
# names(b38im)[2] = 'n_imm'
# b38mm = aggregate(co~rcl,data=subset(b38,Pleopod_mat==1),FUN=sum)
# names(b38mm)[2] = 'n_mat'
# 
# b38p=merge(b38mm, b38im, all=T)
# b38p = bio.utilities::na.zero(b38p)
# b38p$n=(b38p$n_mat+b38p$n_imm)
# 
# b38p$p = b38p$n_mat/(b38p$n_mat+b38p$n_imm)
# m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
# b38nlsp<-nls(m1,data=b38p,start=list(a=2,b=-.05), weights=b38p$n)
# Propmatp<-predict(b38nlsp,newdata=seq(1,9,1))
# 
# plot(b38p$rcl,b38p$p)
# lines(b38p$rcl,Propmatp)




###################### BOF ALL ##########################

bof = subset(b,LFA=='35' | LFA=='36' | LFA=="38") ########FIGURE OUT WHAT TO DO HERE ############
g = glm(Pleopod_mat~Carapace_mm,data=bof,family=binomial(link='logit')) 
#correct CI's
l = seq(min(bof$Carapace_mm),max(bof$Carapace_mm),by=.1)
ndata <- list(Carapace_mm=l)
ndata = glmCIs(g,ndata)

plot(bof$Carapace_mm, bof$Pleopod_mat, pch = 16, xlab = "Carapace (mm) ", ylab = "Proportion Mature ")
lines(ndata$Carapace_mm, ndata$fit_resp, col="black")
lines(ndata$Carapace_mm, ndata$upr, lty=2, col="darkgrey") 
lines(ndata$Carapace_mm, ndata$lwr,lty=2, col="darkgrey")
abline(v=82.5, col="red", lty= 3)
with(ndata,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	
text(63, 0.9, paste("Bay of Fundy (LFAs 35,36,38)"))
text(63, 0.85,bquote(paste('L'['50']*' = ','87.3')))



# 
# ## to get a and b from nls binned data
# #pull out data 
# bof<- bof%>%
#   dplyr::select(Carapace_mm,Pleopod_mat)
# 
# 
# 
# bof$rcl=round(bof$Carapace_mm/3)*3+1
# bof$co = 1
# bofim = aggregate(co~rcl,data=subset(bof,Pleopod_mat==0),FUN=sum)
# names(bofim)[2] = 'n_imm'
# bofmm = aggregate(co~rcl,data=subset(bof,Pleopod_mat==1),FUN=sum)
# names(bofmm)[2] = 'n_mat'
# 
# bofp=merge(bofmm, bofim, all=T)
# bofp = bio.utilities::na.zero(bofp)
# bofp$n=(bofp$n_mat+bofp$n_imm)
# 
# 
# bofp$p = bofp$n_mat/(bofp$n_mat+bofp$n_imm)
# m1 = formula(p~1/(1+(exp(a+(b*rcl)))))
# bofnlsp<-nls(m1,data=bofp,start=list(a=2,b=-.05), weights=bofp$n)
# Propmatp<-predict(bofnlsp,newdata=seq(1,9,1))
# 
# 
# plot(bofp$rcl,bofp$p)
# lines(bofp$rcl,Propmatp)
