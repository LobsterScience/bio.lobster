require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(devtools)

la()

p=list()

p$lfas = c("41") # specify lfa
p$current.assessment.year =2023
p$yrs = 1947:p$current.assessment.year

assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!
fp = file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2023','indicators')

#jpeg("BM41_2023TEST.jpeg", width = 9, height = 7, units = "in",res=600, bg = "white")
par(mfrow = c(2,2),las=1,mar = c(2,2,2,2),omi=c(0.5,0.5,0.25,0.25))

###DFO RV survey
all.out=read.csv(file.path(fp,'DFO.restratified.All.csv'))

out=read.csv(file.path(fp,'DFO.restratified.commercial.csv'))
a = merge(all.out,out,by='yr')
#median(a$w.Yst.y/a$w.Yst.x)    #0.876 proportion of total weight that comprises commercial animals....assuming constant over
ao = all.out[,c('yr','w.Yst')]
ao$w.Yst = ao$w.Yst * 0.876
ao$w.Yst[ao$yr %in% out$yr[which(!is.na(out$yr))]] <- out$w.Yst[which(!is.na(out$yr))]
#ao$w.Yst[ao$yr %in% out$yr] <- out$w.Yst[which(!is.na(out$yr))]
ao$w.Yst = ao$w.Yst/1000

#ao is full time series of biomasses
ao<-ao[-c(52),]
plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')


ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])
abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)




###Georges###
rm(out)
aout=read.csv(file.path(fp,'DFO.Georges.All.csv'))
aout = aout[,c('yr','w.Yst')]
out=read.csv(file.path(fp,'DFO.Georges.commercial.csv'))
out = subset(out,yr>=2007)
out = out[,c('yr','w.Yst')]

aa=merge(out,aout,all.x=T,by='yr')
#sum(aa[,2])/sum(aa[,3]) #0.872 commercial
aout$w.Yst = aout$w.Yst * 0.872
aout$w.Yst[aout$yr %in% out$yr] <- out$w.Yst


ao = aout
ao$w.Yst = ao$w.Yst/1000

plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')

#based on bcp
ub = median(subset(ao,yr %in% 2000:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])

abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)


###Spring ###
out=read.csv(file.path(fp,'NEFSC.spring.restratified.commercial.csv'))
ao = out[,c('yr','w.Yst')]
ao$w.Yst = ao$w.Yst/1000

plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')

ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])
abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)


###Fall###

out=read.csv(file.path(fp,'NEFSC.fall.restratified.commercial.csv'))
ao = out[,c('yr','w.Yst')]
ao$w.Yst = ao$w.Yst/1000



plot(ao$yr,ao$w.Yst,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(ao$yr,ao$w.Yst,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(ao$yr,ao$w.Yst)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')

ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])
abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)



mtext(" ",1,1,outer=T, cex= 2)
mtext("Stratified Total Weight (kt)",2,1,outer=T,las=0, cex = 1.5)





################### REPRODUCTIVE POTENTIAL ##########################


require(bio.lobster)
la()

ff = c(
  file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2023','maturefemaleLengthFrequenciesLFA41polygonSummerRV.rdata'),
  file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2023','maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.rdata'),
  file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2023','maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.rdata'),
 file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2023','maturefemaleLengthFrequenciesLFA41dfogeorges.rdata'))


#jpeg("RP41_2022_FR.jpeg", width = 11, height = 8, units = "in",res=600, bg = "white")
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

ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
abline(h=ub,col='lightcyan4',lwd=2,lty = 4)


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


nub = median(subset(out,yr %in% 1999:2015,select=Fecm)[,1])


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

ub = median(subset(out,yr %in% 2001:2015,select=Fecm)[,1]) * 0.4
llb = out$Fecm[which(out$Fecm>0)]
llb = median(sort(llb)[1:5])

abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty = 4 )


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

plot(out$yr,out$Fecm,type='n',xlab=' ',ylab = paste(" ",sep=" "))
points(out$yr,out$Fecm,type='p',lty=1,pch=16,lwd=2)
rmm = rmed(out$yr,out$Fecm)
lines(rmm[[1]],rmm[[2]],lty=1,lwd=3,col='red')

ub = median(subset(out,yr %in% 2000:2015,select=Fecm)[,1]) * 0.4
llb = out$Fecm[which(out$Fecm>0)]
llb = median(sort(llb)[1:5])

abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2,lty = 4)

mtext(" ",1,1,outer=T, cex= 2)
mtext("Reproductive Potential ",2,1,outer=T,las=0, cex = 1.5)









