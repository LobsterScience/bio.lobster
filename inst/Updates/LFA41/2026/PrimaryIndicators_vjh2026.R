require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(devtools)
require(tidyr)
require(ggplot2)
la()
 
p=list()

p$lfas = c("41") # specify lfa
p$current.assessment.year =2026
p$yrs = 1947:p$current.assessment.year

assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!
#fp = file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2026','indicators')
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")
jpeg("CommB_2026.jpeg", width = 9, height = 9, units = "in",res=600, bg = "white")
par(mfrow = c(2,2),las=1,mar = c(2,2,2,2),omi=c(0.5,0.5,0.25,0.25))

###DFO RV survey

#all.out=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/DFO.restratified.All.csv")

#out=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/LFA41-DFOCommercialB.csv")

all.out=read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/DFO.restratified.All.csv")

out=read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/LFA41-DFOCommercialB.csv")

survey.summary <- data.frame(
  Survey = character(),
  Yr = numeric(),
  w.Yst = numeric(),
  rmm = numeric(),
  ub = numeric(),
  llb = numeric(),
  times_usi = numeric(),
  times_llb = numeric(),
  stringsAsFactors = FALSE
)


#a = merge(all.out,out,by='yr')
#median(a$w.Yst.y/a$w.Yst.x)    #0.876 proportion of total weight that comprises commercial animals....assuming constant over
ao = all.out[,c('yr','w.Yst')]
ao$w.Yst = ao$w.Yst * 0.876
ao$w.Yst[ao$yr %in% out$yr[which(!is.na(out$yr))]] <- out$w.Yst[which(!is.na(out$yr))]
#ao$w.Yst[ao$yr %in% out$yr] <- out$w.Yst[which(!is.na(out$yr))]
ao$w.Yst = ao$w.Yst/1000

#ao is full time series of biomasses
#ao<-ao[-c(52),] Data is now available
plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(ao$yr), y = max(ao$w.Yst), labels = "DFO RV41", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")


ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])
abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)

# Most recent year
i <- which.max(ao$yr)

# Terminal-year running median
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

survey.summary <- rbind(
  survey.summary,
  data.frame(
    Survey = "DFO RV41",
    Yr = ao$yr[i],
    w.Yst = ao$w.Yst[i],
    rmm = rmm.val,
    ub = ub,
    llb = llb,
    times_usi = rmm.val / ub, 
    times_llb = rmm.val / llb
  )
)

###Georges###
rm(out)
#aout=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/DFO.Georges.All.csv")
aout=read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/DFO.Georges.All.csv")

aout = aout[,c('yr','w.Yst')]
#out=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/LFA41-DFOCommercialB_georges.csv")
out=read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/LFA41-DFOCommercialB_georges.csv")

out = subset(out,yr>=2007)
out = out[,c('yr','w.Yst')]

#sum(aa[,2])/sum(aa[,3]) #0.872 commercial
aout$w.Yst = aout$w.Yst * 0.872
aout$w.Yst[aout$yr %in% out$yr] <- out$w.Yst


ao = aout
ao$w.Yst = ao$w.Yst/1000

plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(ao$yr), y = max(ao$w.Yst), labels = "DFO Georges Bank", pos = 4, offset = 1, cex = 1.2, col = "dodgerblue4")

#based on bcp
ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])

abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)



i <- which.max(ao$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]
survey.summary <- rbind(
  survey.summary,
  data.frame(
    Survey = "DFO Georges",
    Yr = ao$yr[i],
    w.Yst = ao$w.Yst[i],
    rmm = rmm.val,
    ub = ub,
    llb = llb,
    times_usi = rmm.val / ub, 
    times_llb = rmm.val / llb
  )
)

###Spring ###
rm(out)
#out=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/LFA41-NEFSCSpringCommercialB.csv")
out=read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/NEFSC.spring.restratified.commercial.csv")
ao = out[,c('yr','w.Yst')]
ao$w.Yst = (ao$w.Yst/1000)/0.4537205 ### Fixing scaling 2026?
ao<-na.omit(ao)


plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(ao$yr), y = max(ao$w.Yst), labels = "NEFSC Spring", pos = 4, offset = 1, cex = 1.2, col = "dodgerblue4")

ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])
abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)

i <- which.max(ao$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]
survey.summary <- rbind(
  survey.summary,
  data.frame(
    Survey = "NEFSC Spring",
    Yr = ao$yr[i],
    w.Yst = ao$w.Yst[i],
    rmm = rmm.val,
    ub = ub,
    llb = llb,
    times_usi = rmm.val / ub, 
    times_llb = rmm.val / llb
  )
)


###Fall###
rm(out)
#out=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/LFA41-NEFSCFallCommercialB.csv")
out=read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/NEFSC.fall.restratified.commercial.csv")
out <- subset(out, yr != max(yr, na.rm = TRUE))  ### REMOVING LAST YEAR BECAUSE OF INSUFFICIENT DATA 

ao = out[,c('yr','w.Yst')]
ao$w.Yst = (ao$w.Yst/1000)/0.4537205 ### Fixing scaling 2026?
ao<-na.omit(ao)


plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(ao$yr), y = max(ao$w.Yst), labels = "NEFSC Fall", pos = 4, offset = 1, cex = 1.2, col = "dodgerblue4")


ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])
abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)



mtext(" ",1,1,outer=T, cex= 2)
mtext("Stratified Total Weight (kt)",2,1,outer=T,las=0, cex = 1.5)

i <- which.max(ao$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

survey.summary <- rbind(
  survey.summary,
  data.frame(
    Survey = "NEFSC Fall",
    Yr = ao$yr[i],
    w.Yst = ao$w.Yst[i],
    rmm = rmm.val,
    ub = ub,
    llb = llb,
    times_usi = rmm.val / ub, 
    times_llb = rmm.val / llb
  )
)

survey.summary
survey.summary <- transform(
  survey.summary,
  w.Yst = round(w.Yst, 3),
  ub = round(ub, 3),
  llb = round(llb, 3)
)

print(survey.summary)
write.csv(survey.summary, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/ComB_surveysummary.csv")

dev.off()
################### REPRODUCTIVE POTENTIAL ##########################
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")

require(bio.lobster)
la()
jpeg("Fec_2026.jpeg", width = 9, height = 9, units = "in",res=600, bg = "white")

ff = c(
  file.path('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata'),
  file.path('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata'),
  file.path('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata'),
  file.path('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/maturefemaleLengthFrequenciesLFA41dfogeorges.rdata'))

fec.summary <- data.frame(
  Survey = character(),
  Yr = numeric(),
  Fecm = numeric(),
  rmm = numeric(),
  ub = numeric(),
  llb = numeric(),
  times_usi = numeric(),
  times_llb = numeric(),
  stringsAsFactors = FALSE
)

par(mfrow = c(2,2),las=1,mar = c(2,2,2,2),omi=c(0.5,0.5,0.25,0.25))


#DFO summer
i=1
load(ff[i])
yll = max(aa$n.yst)
h = split(aa,f=aa$yr)
out= c()
for(j in 1:length(h)) {
  g = h[[j]]
  y = unique(g$yr)
  #g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
  g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
  g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
  g$Fecm = g$Fec * g$n.Yst / 1000000
  n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
  out = rbind(out,n)
}


plot(out$yr,out$Fecm,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(out$yr,out$Fecm,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(out$yr,out$Fecm)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(out$yr), y = max(out$Fecm), labels = "DFO RV41", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")

ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
abline(h=ub,col='lightcyan4',lwd=2,lty = 4)



i <- which.max(out$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

fec.summary <- rbind(
  fec.summary,
  data.frame(
    Survey = "DFO RV41",
    Yr = out$yr[i],
    Fecm = out$Fecm[i],
    rmm = rmm.val,
    ub = ub,
    llb =NA,
    times_usi = rmm.val / ub,
    times_llb =NA
  )
)


#  DFO Georges
i=4
load(ff[i])
yll = max(aa$n.yst)
h = split(aa,f=aa$yr)
out= c()
for(j in 1:length(h)) {
  g = h[[j]]
  y = unique(g$yr)
  #g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
  g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
  g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
  g$Fecm = g$Fec * g$n.Yst / 1000000
  n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
  out = rbind(out,n)
}

plot(out$yr,out$Fecm,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(out$yr,out$Fecm,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(out$yr,out$Fecm)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(out$yr), y = max(out$Fecm), labels = "DFO Georges Bank", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")


nub = median(subset(out,yr %in% 1999:2015,select=Fecm)[,1])

i <- which.max(out$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

fec.summary <- rbind(
  fec.summary,
  data.frame(
    Survey = "DFO Georges Bank",
    Yr = out$yr[i],
    Fecm = out$Fecm[i],
    rmm = rmm.val,
    ub = NA,
    llb = NA,
    times_usi = NA,
    times_llb =NA
  )
)


#NEFSC Spring survey
i=2
load(ff[i])
yll = max(aa$n.yst)
h = split(aa,f=aa$yr)
out= c()
for(j in 1:length(h)) {
  g = h[[j]]
  y = unique(g$yr)
  #g$Mat = exp(-17.8583 +(0.1894 *g$FLEN)) / (1+exp(-17.8583 +(0.1894 *g$FLEN))) #pezzack and duggan 1989
  g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
  g$Mat = ifelse(g$FLEN<120, g$Mat/2,g$Mat*(2/3))
  g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
  g$Fecm = g$Fec * g$n.Yst / 1000000
  if(sum(g$n.Yst,na.rm=T)>0){
    n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
    out = rbind(out,n)
  }}


plot(out$yr,out$Fecm,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(out$yr,out$Fecm,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(out$yr,out$Fecm)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(out$yr), y = max(out$Fecm), labels = "NEFSC Spring", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")

ub = median(subset(out,yr %in% 2001:2015,select=Fecm)[,1]) * 0.4
llb = out$Fecm[which(out$Fecm>0)]
llb = median(sort(llb)[1:5])

abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty = 4 )


i <- which.max(out$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

fec.summary <- rbind(
  fec.summary,
  data.frame(
    Survey = "NEFSC Spring",
    Yr = out$yr[i],
    Fecm = out$Fecm[i],
    rmm = rmm.val,
    ub = ub,
    llb = llb,
    times_usi = rmm.val / ub, 
    times_llb = rmm.val / llb
  )
)
#NEFSC Autumn survey
i=3
load(ff[i])
yll = max(aa$n.yst)
h = split(aa,f=aa$yr)
out= c()
for(j in 1:length(h)) {
  g = h[[j]]
  y = unique(g$yr)
  g$Mat = 1 / (1+exp(-(-22.5522 +0.2455 *g$FLEN))) #gaudette 2016 from offshore lobsters collected in 2016
  g$Fec = (g$Mat * 0.0031829 * g$FLEN ^ 3.353501)  # campbell and Robinson 1983
  g$Fecm = g$Fec * g$n.Yst / 1000000
  if(sum(g$n.Yst,na.rm=T)>0){
    n = aggregate(cbind(n.Yst,Fecm)~yr,data = g, FUN=sum, na.rm=T)
    out = rbind(out,n)
  }}


out <- subset(out, yr != max(yr, na.rm = TRUE))  ### REMOVING LAST YEAR BECAUSE OF INSUFFICIENT DATA 


plot(out$yr,out$Fecm,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(out$yr,out$Fecm,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(out$yr,out$Fecm)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')
text(x = min(out$yr), y = max(out$Fecm), labels = "NEFSC Fall", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")


ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
llb = out$Fecm[which(out$Fecm>0)]
llb = median(sort(llb)[1:5])

abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2,lty = 4)



i <- which.max(out$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

fec.summary <- rbind(
  fec.summary,
  data.frame(
    Survey = "NEFSC Fall",
    Yr = out$yr[i],
    Fecm = out$Fecm[i],
    rmm = rmm.val,
    ub = ub,
    llb = llb,
    times_usi = rmm.val / ub, 
    times_llb = rmm.val / llb
  )
)

mtext(" ",1,1,outer=T, cex= 2)
mtext("Reproductive Potential (Millions of eggs) ",2,1,outer=T,las=0, cex = 1.5)

dev.off()


fec.summary$Fecm <- round(fec.summary$Fecm, 3)
fec.summary$rmm <- round(fec.summary$rmm, 3)
fec.summary$ub <- round(fec.summary$ub, 3)
fec.summary$llb <- round(fec.summary$llb, 3)
fec.summary$times_usi <- round(fec.summary$times_usi, 3)

fec.summary


write.csv(fec.summary, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/FecM_surveysummary.csv")



