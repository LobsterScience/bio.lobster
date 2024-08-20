##Length Weight Request
require(tidyverse)

p = bio.lobster::load.environment()
#la()
#Plot LW Regressions for Lobster by Sex
 
savdir="C:/RSaves/" #Change as Appropriate

#Plot Seperately

sexes=c(1:3)
max.cw=140 #change as appropriate
for (s in sexes){
  
  L=50:max.cw
  B = lobLW(L, sex=s)
  MLW<-cbind(L,B)
  MLW<-as.data.frame(MLW)
  MLW$Weight_lbs<-MLW$B/453.6
  
  names(MLW)[names(MLW) == 'L'] <- 'CarapaceLength_mm'
  names(MLW)[names(MLW) == 'B'] <- 'Weight_g'

  titl=NA
  if (s=="1") {titl="Male Lobster Weight"}
  if (s=="2") {titl="Female Lobster Weight"}
  if (s=="3") {titl="Female Lobster (Berried) Weight"}


png(filename=paste0(savdir,titl,".png"),width=8, height=6.5, units = "in", res = 800)
plot(MLW$CarapaceLength_mm, MLW$Weight_lbs,xlab="Carapace Length (mm)", ylab= "Weight (lbs)", main= titl, type="l")
dev.off()
}


#Plot on one


  L=50:155
  B = lobLW(L, sex=1)
  m.MLW<-cbind(L,B)
  m.MLW<-as.data.frame(m.MLW)
  m.MLW$Weight_lbs<-m.MLW$B/453.6
 
 
  F = lobLW(L, sex=2)
  f.MLW<-cbind(L,F)
  f.MLW<-as.data.frame(f.MLW)
  f.MLW$Weight_lbs<-f.MLW$F/453.6
  
 
  b = lobLW(L, sex=3)
  b.MLW<-cbind(L,b)
  b.MLW<-as.data.frame(b.MLW)
  b.MLW$Weight_lbs<-b.MLW$b/453.6
  
  
  titl="Lobster Weights at Length mls change"
  png(filename=paste0(savdir,titl,".png"),width=8, height=6.5, units = "in", res = 800)
  plot(m.MLW$L, m.MLW$Weight_lbs,xlab="Carapace Length (mm)", ylab= "Weight (lbs)", main= titl, type="n", col="black")
  lines(f.MLW$L, f.MLW$Weight_lbs, col="red")
  lines(b.MLW$L, b.MLW$Weight_lbs, col="darkgreen")
  lines(m.MLW$L, m.MLW$Weight_lbs, col="blue")
  text(x=130, y=1.85, col="red", "Female (No Eggs)", pos=4)
  text(x=130, y=1.5, col="darkgreen", "Female (Berried)", pos=4)
  text(x=130, y=1.15, col="blue", "Male", pos=4)
  #abline(v = 82.5, col = 'black', lwd = 2, lty = 'dashed')
  #abline(v = 86, col = 'black', lwd = 2, lty = 'dashed')
  rect(xleft = 82.5, xright = 86, ybottom = 0, ytop = 1.35, col="darkred", density=30, angle=-30, lwd=1)
  dev.off()
