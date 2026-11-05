require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(devtools)
require(tidyr)
la()

p=list()

p$lfas = c("41") # specify lfa
p$current.assessment.year =2026
p$yrs = 1947:p$current.assessment.year

assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!
#fp = file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2026','indicators')
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")
jpeg("CommB_2026_FRENCH.jpeg", width = 9, height = 9, units = "in",res=600, bg = "white")
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
text(x = min(ao$yr), y = max(ao$w.Yst), labels = "DFO Banc de Georges", pos = 4, offset = 1, cex = 1.2, col = "dodgerblue4")

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
    Survey = "DFO Banc de Georges",
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
text(x = min(ao$yr), y = max(ao$w.Yst), labels = "NEFSC printemps", pos = 4, offset = 1, cex = 1.2, col = "dodgerblue4")

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
    Survey = "NEFSC printemps",
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
text(x = min(ao$yr), y = max(ao$w.Yst), labels = "NEFSC l’automne", pos = 4, offset = 1, cex = 1.2, col = "dodgerblue4")


ub = median(subset(ao,yr %in% 2001:2015,select=w.Yst)[,1]) * 0.4
llb = ao$w.Yst[which(ao$w.Yst>0)]
llb = median(sort(llb)[1:5])
abline(h=llb,col='dodgerblue4',lwd=2)
abline(h=ub,col='lightcyan4',lwd=2, lty=4)



mtext(" ",1,1,outer=T, cex= 2)
mtext("Poids total stratifié (kt)",2,1,outer=T,las=0, cex = 1.5)

i <- which.max(ao$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

survey.summary <- rbind(
  survey.summary,
  data.frame(
    Survey = "NEFSC l’automne",
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
#write.csv(survey.summary, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/ComB_surveysummary.csv")

################### REPRODUCTIVE POTENTIAL ##########################


require(bio.lobster)
la()

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
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")
jpeg("Repro_2026_FRENCH.jpeg", width = 9, height = 9, units = "in",res=600, bg = "white")
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
text(x = min(out$yr), y = max(out$Fecm), labels = "DFO Banc de Georges", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")


nub = median(subset(out,yr %in% 1999:2015,select=Fecm)[,1])

i <- which.max(out$yr)
rmm.val <- rmm[[2]][which.max(rmm[[1]])]

fec.summary <- rbind(
  fec.summary,
  data.frame(
    Survey = "DFO Banc de Georges",
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
text(x = min(out$yr), y = max(out$Fecm), labels = "NEFSC printemps", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")

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
    Survey = "NEFSC printemps",
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
text(x = min(out$yr), y = max(out$Fecm), labels = "NEFSC l’automne", pos=4,  offset = 1, cex = 1.2, col = "dodgerblue4")


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
    Survey = "NEFSC l’automne",
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
mtext("Potentiel de reproduction (millions d’œufs)",2,1,outer=T,las=0, cex = 1.5)




fec.summary$Fecm <- round(fec.summary$Fecm, 3)
fec.summary$rmm <- round(fec.summary$rmm, 3)
fec.summary$ub <- round(fec.summary$ub, 3)
fec.summary$llb <- round(fec.summary$llb, 3)
fec.summary$times_usi <- round(fec.summary$times_usi, 3)

fec.summary


#write.csv(fec.summary, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/FecM_surveysummary.csv")

############### NEW LANDINGS 


require(RODBC)
require(devtools)
require(roxygen2)
require(geosphere)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(rlang)
require(bio.survey)
require(bio.lobster)
require(dplyr)
require(ggplot2)


p=list()
p$assessment.year = 2026 ##Check Year
p$syr = 1989
p$lfa=41
p$yrs = 1947:p$assessment.year
figdir = file.path("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")
p$lfas = c("41") # specify lfa

#lobster.db('process.logs41.redo')
logs41=lobster.db("process.logs41")
#lobster.db('annual.landings.redo')
land = lobster.db('annual.landings')


theme_csas <- function(base_size = 14, base_family = "", text_col = "grey20",
                       panel_border_col = "grey70") {
  half_line <- base_size / 2
  theme_light(base_size = base_size, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = text_col),
      strip.text.y = element_text(colour = text_col),
      axis.text = element_text(colour = text_col),
      axis.title = element_text(colour = text_col),
      legend.title = element_text(colour = text_col, size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = panel_border_col, linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.9), colour = text_col),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = text_col, size = rel(1)),
      plot.subtitle = element_text(colour = text_col, size = rel(.85))
    )
}

#########LANDINGS
land41 <- land[, c("YR", "LFA41")]
land41<- land41[land41$YR > 2001 & land41$YR <= 2026,]
land41 <- land41[order(land41$YR), ]
land41$TAC<- 720
land41$Vessels <-1
land41$Season<-land41$YR

land41 <- land41 %>%
  rename(Landings = LFA41)

land41 <- land41 %>%
  rename(Year = YR)

land41$Season<-as.character(land41$Season)
land41$TAC<-as.character(land41$TAC)


histland<-read.csv('C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/datfiles2026/historiclandings.csv')


# Keep only desired seasons from histland
histland_sub <- histland %>%
  mutate(
    start_year = as.numeric(substr(Season, 1, 4))
  ) %>%
  filter(
    (nchar(Season) == 4 & start_year >= 1981 & start_year <= 1985) |
      (grepl("-", Season) & start_year >= 1985 & start_year <= 2000)
  ) %>%
  select(-start_year)

histland_sub$Landings<-as.numeric(histland_sub$Landings)

histland_sub$Year<-1981:2001


# Merge, keeping all rows from both datasets
land_all <- bind_rows(histland_sub,land41)

land_all <- land_all %>%
  rename(TACDes = TAC)

land_all <- land_all %>%
  mutate(
    TAC = as.numeric(sub(" .*", "", TACDes))
  )

missing_years <- data.frame(
  Season = as.character(1971:1980),
  Year = 1971:1980
)

land_all <- bind_rows(land_all, missing_years) %>%
  arrange(Year)

land_all <- land_all %>%
  mutate(
    TACDes = ifelse(Year %in% 1976:1980, "408 (4X)", TACDes),
    TAC = ifelse(Year %in% 1976:1980, 408, TAC) )

land_all$Vessels[land_all$Year %in% 1971:1975] <- 6
land_all$Vessels[land_all$Year %in% 1976:1980] <- 8

land_all$notes <- NA_character_
land_all$notes[land_all$Year == 1971] <- "La pêche a ouverte en Juillet 1971"
land_all$notes[land_all$Year == 1979] <- "Fermeture de la ZPH 40 pour la protection du stock géniteur"
land_all$notes[land_all$Year == 2013] <- "Cycle de gestion du quota sur 3 ans"

season_years <- c(1985, 1986, 2004, 2006)
interim_tac <- c(2025, 2026)

ann_interim <- data.frame(
  Year = c(2025, 2026),
  y = c(720, 720)
)

ann <- land_all %>%
  filter(!is.na(notes)) %>%
  mutate(
    label_x = Year,
    label_y = 950,
    arrow_y = ifelse(is.na(Landings), 0, Landings + 20)
  )

ann_labels <- ann %>%
  filter(!Year %in% season_years)

ann_stars <- land_all %>%
  filter(Year %in% season_years) %>%
  mutate(
    star_y = ifelse(is.na(Landings), 25, Landings + 25)
  )


jpeg("newlandings_2026_FRENCH.jpeg", width =11, height = 13, units = "in",res=600, bg = "white")
########### PLOT 
ggplot(
  land_all,
  aes(x = Year, y = Landings)
) +
  
  # Landings bars
  geom_col(
    aes(fill = ifelse(
      Year == max(Year, na.rm = TRUE),
      "Recent", "Other"
    )),
    colour = "black",
    width = 0.8
  ) +
  
  # TAC line
  geom_line(
    data = land_all %>%
      filter(!is.na(TAC), Year <= 2024) %>%
      arrange(Year),
    aes(x = Year, y = TAC),
    colour = "red",
    linewidth = 1.2,
    linetype = "dashed") +
  
  geom_segment(
    data = ann_labels,
    aes(
      x = Year,
      y = label_y - 25,
      xend = Year,
      yend = arrow_y
    ),
    linewidth = 0.5,
    colour = "blue",
    arrow = arrow(
      length = unit(0.15, "cm"),
      type = "closed"
    ),
    inherit.aes = FALSE)+
  geom_text(
    data = ann_labels,
    aes(
      x = Year,
      y = label_y,
      label = notes
    ),
    angle = 25,
    colour = "blue",
    hjust = 0,
    vjust = 0,
    size = 3.75,
    inherit.aes = FALSE
  )+
  scale_fill_manual(
    values = c(
      "Other" = "black",
      "Recent" = "grey70"
    )
  ) +
  # Blue asterisks = season changes
  geom_text(
    data = ann_stars,
    aes(
      x = Year,
      y = star_y,
      label = "*"
    ),
    colour = "blue",
    size = 8,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  
  # Red diamond = interim TAC
  geom_point(
    data = ann_interim,
    aes(x = Year, y = y),
    shape = 18,      # diamond
    size = 4,
    colour = "red",
    inherit.aes = FALSE
  ) +
  labs(
    x = "Année",
    y = "Débarquements (t)"
  ) +
  
  scale_x_continuous(
    breaks = seq(1970, 2026, by = 2), 
    expand=c(0,1)
  ) +
  
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1150)
  ) +
  
  theme_csas() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
dev.off()
