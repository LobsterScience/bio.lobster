p = bio.lobster::load.environment()

require(ggplot2)

figdir = file.path(project.datadirectory("bio.lobster","requests","season.shift.lfa27",p$current.assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

png(filename=file.path(figdir, "MapLFA27.png"),width=5, height=5, units = "in", res = 800)
LobsterMap('27')
dev.off()

#load("C:/bio.data/bio.lobster/data/ODBCDump/atSea.rdata") #Import AtSea data
#lobster.db('atSea.redo')
#lobster.db('atSea.clean.redo')

atSea.clean=lobster.db('atSea.clean')

lfas= c("27")
p$lfas=lfas
a=atSea.clean[atSea.clean$LFA %in% lfas,]
a$yr=year(a$STARTDATE)
b=a[a$yr>2009,]
b=b[is.finite(b$CARLENGTH) & is.finite(b$SEX),]

high=124 #define slot
low=115
l="27"

c=b[b$CARLENGTH<=160,]

big=160 #upper bound for hist. Needs to be a multiple of 5
#print(l)
cu=c[!duplicated(c[,c('TRIPNO')]),]
cu=cu[cu$WOS %in% c(1:9),]


c=c[c$SEX %in% c(1:3),]
c$SEX[c$SEX==1]="Male"
c$SEX[c$SEX==2]="No Eggs"
c$SEX[c$SEX==3]="Eggs"
c$SEX=factor(c$SEX, levels=c("Eggs", "No Eggs", "Male"))
c$week=factor(paste0("Week ", c$WOS))




require(sp) #sp package allows determination of in/out of poly
div= read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","areas.27.csv"))

#create North/South index
n.ind=which(point.in.polygon(c$X, c$Y,div$X[div$area=="27.North"],div$Y[div$area=="27.North"])==1) #Little River and North
c.ind=which(point.in.polygon(c$X, c$Y,div$X[div$area=="27.Central"],div$Y[div$area=="27.Central"])==1)  #South Bar to Little River
s.ind=which(point.in.polygon(c$X, c$Y,div$X[div$area=="27.South"],div$Y[div$area=="27.South"])==1)  #Gabarus to South Bar


c$area=NA
c$area[n.ind]="north"
c$area[c.ind]="central"
c$area[s.ind]="south"

#Map samples

png(filename=file.path(figdir, "LFA27.Zones.png"),width=6, height=6, units = "in", res = 800)
LobsterMap('27.Crop',addGrids=F, labels="")
addPolys(div[div$area=="27.North",],col="red", density=12, angle =45)
addPolys(div[div$area=="27.Central",],col="blue", density=12, angle =90)
addPolys(div[div$area=="27.South",],col="green", density=12, angle =0)
addPoints(c[is.finite(c$Y),], pch=16, cex=0.3)
addPoints(c[c$area %in% "north",], pch=16, cex=0.3, col="red")
addPoints(c[c$area %in% "central",], pch=16, cex=0.3, col="blue")
addPoints(c[c$area %in% "south",], pch=16, cex=0.3, col="green")
dev.off()


#Samples   
     png(filename=paste0(figdir, "/Yearly Samples LFA",l,".png"))
        yrs=hist(cu$yr, breaks=10, main=paste0("Annual Sampling ",l), xlab="Year", ylab="Number of samples") #shows distribution of samples across time
    dev.off()
    
 
    png(filename=paste0(figdir, "/Weekly Samples LFA",l,".png"))
        weeks=hist(cu$WOS, breaks=seq(1, 9, length.out=10), main=paste0("Weekly Samples ",l), xlab="Week of Season", ylab="Number of Samples") #shows distribution of samples across time
    dev.off()
    
    png(filename=paste0(figdir,"/Yearly Lobster Sampled LFA",l,".png"))    
      all.animals=hist(c$yr, breaks=10, main=paste0(l),xlab="Year", ylab="Number of Lobster") #shows distribution of lobster sizes
    dev.off()

#Lobster Sizes      
    png(filename=paste0(figdir,"/Size Distribution LFA",l,".png"))
      #all=hist(c$CARLENGTH, xlim=c(0,big), breaks=seq(0, big, 5), main=paste0("All Lobster LFA",l), col='grey88', xlab="Carapace Length", ylab="Number of Lobster")
    ggplot(data = c, aes(x = CARLENGTH, fill = SEX)) +
      xlim(40, 130) +
      geom_histogram(colour = 'black', binwidth=2, center=1) +
     
     geom_vline(xintercept = 82.5, linetype="dashed", color = "red", size=1) +
      scale_fill_manual(values = c("orange1", "deepskyblue1", "palegreen3"), labels= c("Female (Eggs)", "Female (No Eggs)", "Male"), name= "") +  
      labs( x="Carapace Length (mm)", y= "Number of Lobster") +
      ggtitle("LFA 27")+
      theme(legend.position = c(0.85, 0.5), plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
      theme(axis.line = element_line(colour = "black"))+
      theme(panel.background = element_blank()) +
      scale_y_continuous(expand = c(0,0.5))
    
    dev.off()
  
#Ovigerous vs non-Ovigerous  

e=c[c$SEX %in% c("No Eggs", "Eggs") & c$CARLENGTH<141,] #Separate female lobster

# ALL-----------------------------------
#-------------------------------------------------
png(filename=paste0(figdir,figdir,"/LFA27.all.berried.week.png"))    
        weeks=c("2","4","8")
        gg=e[e$WOS %in% weeks,]  
            
            ggplot(data = gg, aes(x = CARLENGTH, fill = SEX)) +
              xlim(40, 120) +
              geom_histogram(colour = 'white', binwidth=2, center=1) +
              facet_grid(week~.)+
              geom_vline(xintercept = 82.5, linetype="dashed", color = "red", size=1) +
              geom_hline(yintercept=0) +
              scale_fill_manual(values = c("orange1", "deepskyblue1"), name= "Egg Status") +  
              labs( x="Carapace Length (mm)", y= "Number of Lobster") +
              ggtitle("LFA 27 Berried by Week")+
              theme(legend.position = c(0.85, 0.5), plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
                theme(axis.line = element_line(colour = "black"))+
                theme(panel.background = element_blank()) +
                scale_y_continuous(expand = c(0,0.5))
dev.off()

areas.27=c("north", "central", "south")
weeks=c("2","4","8")

for (i in areas.27) {
  
    j=e[e$area==i,]
    gg=j[j$WOS %in% weeks,]  
    
    png(filename=paste0(figdir,"/LFA27.",i,".berried.week.png")) 
      print(
        ggplot(data = gg, aes(x = CARLENGTH, fill = SEX)) +
          xlim(40, 120) +
          geom_histogram(colour = 'white', binwidth=2, center=1) +
          facet_grid(week~.)+
          geom_vline(xintercept = 82.5, linetype="dashed", color = "red", size=1) +
          geom_hline(yintercept=0) +
          scale_fill_manual(values = c("orange1", "deepskyblue1"), name= "Egg Status") +  
          labs( x="Carapace Length (mm)", y= "Number of Lobster") +
          ggtitle(paste0("LFA 27 ",i," Berried by Week"))+
          theme(legend.position = c(0.85, 0.5), plot.title = element_text(hjust = 0.5, size=14, face="bold")) +
          theme(axis.line = element_line(colour = "black"))+
          theme(panel.background = element_blank()) +
          scale_y_continuous(expand = c(0,0.5))
      )
        dev.off()
}


# Percent Ovigerous
#-------------------------------

berried=data.frame() #all
week=c(1:8)
for (w in week){
  gg=e[e$WOS %in% w,]  
  num.ber=length(gg$EID[gg$SEX=="Eggs"])
  num.not=length(gg$EID[gg$SEX=="No Eggs"])
  per.ber=num.ber/(num.ber + num.not)*100
  output=c(w,per.ber)
  berried=rbind(berried, output)
}
names(berried)=c("Week", "All.Percent Berried")

 n.berried=data.frame() #north

 for (w in week){
   g=e[e$area=="north",]
   g=g[g$WOS %in% w,] 
   n.num.ber=length(g$EID[g$SEX=="Eggs"])
   n.num.not=length(g$EID[g$SEX=="No Eggs"])
   n.per.ber=n.num.ber/(n.num.ber + n.num.not)*100
   n.output=c(w,n.per.ber)
   n.berried=rbind(n.berried, n.output)
 }
 names(n.berried)=c("Week", "N.Percent Berried")
 
 
c.berried=data.frame() #central
for (w in week){
    g=e[e$area=="central",] 
   g=g[g$WOS %in% w,] 
   c.num.ber=length(g$EID[g$SEX=="Eggs"])
   c.num.not=length(g$EID[g$SEX=="No Eggs"])
   c.per.ber=c.num.ber/(c.num.ber + c.num.not)*100
   c.output=c(w,c.per.ber)
   c.berried=rbind(c.berried, c.output)
 }
 names(c.berried)=c("Week", "C.Percent Berried")
 
 s.berried=data.frame() #south

 for (w in week){
   g=e[e$area=="south",]
   g=g[g$WOS %in% w,] 
   s.num.ber=length(g$EID[g$SEX=="Eggs"])
   s.num.not=length(g$EID[g$SEX=="No Eggs"])
   s.per.ber=s.num.ber/(s.num.ber + s.num.not)*100
   s.output=c(w,s.per.ber)
   s.berried=rbind(s.berried, s.output)
 }
 names(s.berried)=c("Week", "S.Percent Berried")

ber=cbind(berried, n.berried[2], c.berried [2], s.berried[2], by=berried$Week)

png(filename=paste0(figdir,"/Percent Berried LFA 27.png"))    
    plot(berried$Week, berried$`Percent Berried`, main="LFA 27", ylab="% Berried", xlab="Week of Season", col="green", type="n", ylim=c(10, 30))
    #points(berried$Week, berried$`Percent Berried`, col="green", pch=16)
    #lines(berried$Week, berried$`Percent Berried`, col="green")
    points(ber$Week, ber$`N.Percent Berried`, col="red", pch=16)
    lines(ber$Week, ber$`N.Percent Berried`, col="red")
    points(ber$Week, ber$`C.Percent Berried`, col="blue", pch=16)
    lines(ber$Week, ber$`C.Percent Berried`, col="blue")
    points(ber$Week, ber$`S.Percent Berried`, col="green", pch=16)
    lines(ber$Week, ber$`S.Percent Berried`, col="green")
    legend(x = "bottomright", legend = c("North", "Central", "South"),lty = c(1, 1), col = c("red", "blue", "green"), bty="n", pch=16)
dev.off()


# Percent Ovigerous
#-------------------------------

berried=data.frame() #all
week=c(1:8)
for (w in week){
  gg=e[e$WOS==w & e$CARLENGTH>78 & e$CARLENGTH<82.5,]  
  num.ber=length(gg$EID[gg$SEX=="Eggs"])
  num.not=length(gg$EID[gg$SEX=="No Eggs"])
  per.ber=num.ber/(num.ber + num.not)*100
  output=c(w,per.ber)
  berried=rbind(berried, output)
}
names(berried)=c("Week", "Sublegal.Percent Berried")

png(filename=paste0(figdir,"/Percent Berried Sublegals.png"))    
plot(berried$Week, berried$`Sublegal.Percent Berried`, main="LFA 27 Sublegal(78-82.4mm)", ylab="% Berried", xlab="Week of Season", col="blue", type="l", ylim=c(10, 30))
points(berried$Week, berried$`Sublegal.Percent Berried`, col="green", pch=16)
dev.off()

n.berried=data.frame() #north

for (w in week){
  g=e[e$area=="north" & e$CARLENGTH>78 & e$CARLENGTH<82.5,]
  g=g[g$WOS %in% w,] 
  n.num.ber=length(g$EID[g$SEX=="Eggs"])
  n.num.not=length(g$EID[g$SEX=="No Eggs"])
  n.per.ber=n.num.ber/(n.num.ber + n.num.not)*100
  n.output=c(w,n.per.ber)
  n.berried=rbind(n.berried, n.output)
}
names(n.berried)=c("Week", "N.Percent Berried")


c.berried=data.frame() #central
for (w in week){
  g=e[e$area=="central" & e$CARLENGTH>78 & e$CARLENGTH<82.5,] 
  g=g[g$WOS %in% w,] 
  c.num.ber=length(g$EID[g$SEX=="Eggs"])
  c.num.not=length(g$EID[g$SEX=="No Eggs"])
  c.per.ber=c.num.ber/(c.num.ber + c.num.not)*100
  c.output=c(w,c.per.ber)
  c.berried=rbind(c.berried, c.output)
}
names(c.berried)=c("Week", "C.Percent Berried")

s.berried=data.frame() #south

for (w in week){
  g=e[e$area=="south" & e$CARLENGTH>78 & e$CARLENGTH<82.5,]
  g=g[g$WOS %in% w,] 
  s.num.ber=length(g$EID[g$SEX=="Eggs"])
  s.num.not=length(g$EID[g$SEX=="No Eggs"])
  s.per.ber=s.num.ber/(s.num.ber + s.num.not)*100
  s.output=c(w,s.per.ber)
  s.berried=rbind(s.berried, s.output)
}
names(s.berried)=c("Week", "S.Percent Berried")

ber=cbind(berried, n.berried[2], c.berried [2], s.berried[2], by=berried$Week)

png(filename=paste0(figdir,"/Sublegal Percent Berried LFA 27.png"))    
plot(berried$Week, berried$`Percent Berried`, main="LFA 27", ylab="% Berried", xlab="Week of Season", col="green", type="n", ylim=c(15, 35))
#points(berried$Week, berried$`Percent Berried`, col="green", pch=16)
#lines(berried$Week, berried$`Percent Berried`, col="green")
points(ber$Week, ber$`N.Percent Berried`, col="red", pch=16)
lines(ber$Week, ber$`N.Percent Berried`, col="red")
points(ber$Week, ber$`C.Percent Berried`, col="blue", pch=16)
lines(ber$Week, ber$`C.Percent Berried`, col="blue")
points(ber$Week, ber$`S.Percent Berried`, col="green", pch=16)
lines(ber$Week, ber$`S.Percent Berried`, col="green")
legend(x = "bottomright", legend = c("North", "Central", "South"),lty = c(1, 1), col = c("red", "blue", "green"), bty="n", pch=16)
dev.off()





#Discards
#---------------------------------------
lfas= c("27", "31A", "31B", "32")
a=atSea.clean[atSea.clean$LFA %in% lfas,]
a$yr=year(a$STARTDATE)
b=a[a$yr>2009,]
b=b[is.finite(b$CARLENGTH) & is.finite(b$SEX),]


c=b[b$LFA==l,]

#All Areas

lfas= c("27", "31A", "31B", "32")

catches=data.frame(matrix(, nrow=length(lfas), ncol=5))
names(catches)=c("LFA","Legal", "Undersize", "Over.Berried", "Under.Berried")
catches$LFA=lfas

for (i in 1:length(lfas)){
      l=lfas[i]
      c=b[b$LFA==l,]
      u=c[c$CARLENGTH<83 & c$SEX %in% c(1,2),]
      u.ber= c[c$SEX=="3" & c$CARLENGTH<83,]
      o.ber=c[c$SEX=="3" & c$CARLENGTH>82,]
      
      
      #discard=rbind(u,berried)
      
      tot=length(c$SEX)
      under=(length(u$SEX))
      o.egg=length(o.ber$SEX)
      u.egg=length(u.ber$SEX)
      dc.num=under+u.egg+o.egg
        
      catches$Legal[catches$LFA==l]=tot-under-o.egg-u.egg
      catches$Undersize[catches$LFA==l]=under
      catches$Over.Berried[catches$LFA==l]=o.egg
      catches$Under.Berried[catches$LFA==l]=u.egg
     }

library(reshape2)
library(EnvStats)

names(catches)=c("LFA", "Legal", "Undersize", ">mls w/eggs", "<mls w/eggs")
catches=catches[,  c("LFA", "Legal", ">mls w/eggs", "<mls w/eggs", "Undersize")]
names(catches)=c("LFA", "Legal-Size", "Legal-Size Egged", "Undersize Egged", "Undersize")

bar.p=melt(catches, id.vars="LFA")

mycolors <- c("green3", "firebrick4", "coral2", "red1")

png(filename=paste0(figdir,"/catch.proportions 27-32.png"),width = 150, height = 150, units='mm', res = 300)
ggplot(bar.p, aes(x = LFA, y = value, fill = variable)) +
geom_col(position="fill") +
scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=mycolors, name="Lobster") +
  labs( x="Lobster Fishing Area", y= "% of Catch") 
dev.off()


# Discards 27 North vs Central vs South

b=a[a$yr>2009 & a$LFA=="27",]
b=b[is.finite(b$CARLENGTH) & is.finite(b$SEX),]
c=b

#create North/South index
n.ind=which(point.in.polygon(c$X, c$Y,div$X[div$area=="27.North"],div$Y[div$area=="27.North"])==1) #Little River and North
c.ind=which(point.in.polygon(c$X, c$Y,div$X[div$area=="27.Central"],div$Y[div$area=="27.Central"])==1)  #South Bar to Little River
s.ind=which(point.in.polygon(c$X, c$Y,div$X[div$area=="27.South"],div$Y[div$area=="27.South"])==1)  #Gabarus to South Bar


c$area=NA
c$area[n.ind]="North"
c$area[c.ind]="Central"
c$area[s.ind]="South"

b=c[!is.na(c$area),]

areas=c("North","Central", "South")

catches=data.frame(matrix(, nrow=length(areas), ncol=5))
names(catches)=c("Area","Legal", "Undersize", "Over.Berried", "Under.Berried")
catches$Area=areas

for (i in 1:length(areas)){
  l=areas[i]
  c=b[b$area==l,]
  u=c[c$CARLENGTH<83 & c$SEX %in% c(1,2),]
  u.ber= c[c$SEX=="3" & c$CARLENGTH<83,]
  o.ber=c[c$SEX=="3" & c$CARLENGTH>82,]
  
  tot=length(c$SEX)
  under=(length(u$SEX))
  o.egg=length(o.ber$SEX)
  u.egg=length(u.ber$SEX)
  dc.num=under+u.egg+o.egg
  
  catches$Legal[catches$Area==l]=tot-under-o.egg-u.egg
  catches$Undersize[catches$Area==l]=under
  catches$Over.Berried[catches$Area==l]=o.egg
  catches$Under.Berried[catches$Area==l]=u.egg
}

library(reshape2)
library(EnvStats)

names(catches)=c("Area", "Legal", "Undersize", ">mls w/eggs", "<mls w/eggs")
catches=catches[,  c("Area", "Legal", ">mls w/eggs", "<mls w/eggs", "Undersize")]
names(catches)=c("Area", "Legal-Size", "Legal-Size Egged", "Undersize Egged", "Undersize")


bar.p=melt(catches, id.vars="Area")
bar.p$Area=factor(bar.p$Area, levels=c("North", "Central", "South"))

mycolors <- c("green3", "firebrick4", "coral2", "red1")

png(filename=paste0(figdir,"/catch.proportions LFA 27 by Area.png"))
ggplot(bar.p, aes(x = Area, y = value, fill = variable)) +
  geom_col(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=mycolors, name="") +
  labs( x="LFA27 Sub-Area", y= "% of Catch") 
dev.off()

# By week
# ----------------------------------

week=c(1:8)
areas=c("North", "South", "Central")

for (aa in areas){

  z=b[b$area %in% aa,]
  
  catches=data.frame(matrix(nrow=length(week), ncol=5))
  names(catches)=c("Week","Legal", "Undersize", "Over.Berried", "Under.Berried")
catches$Week=week


for (i in 1:length(week)){
  l=week[i]
  c=z[z$WOS == l,]
  u=c[c$CARLENGTH<83 & c$SEX %in% c(1,2),]
  u.ber= c[c$SEX=="3" & c$CARLENGTH<83,]
  o.ber=c[c$SEX=="3" & c$CARLENGTH>82,]
  
  tot=length(c$SEX)
  under=(length(u$SEX))
  o.egg=length(o.ber$SEX)
  u.egg=length(u.ber$SEX)
  dc.num=under+u.egg+o.egg
  
  catches$Legal[catches$Week==i]=tot-under-o.egg-u.egg
  catches$Undersize[catches$Week==i]=under
  catches$Over.Berried[catches$Week==i]=o.egg
  catches$Under.Berried[catches$Week==i]=u.egg
}

library(reshape2)
library(EnvStats)

names(catches)=c("Week", "Legal", "Undersize", ">mls w/eggs", "<mls w/eggs")
catches=catches[,  c("Week", "Legal", ">mls w/eggs", "<mls w/eggs", "Undersize")] #reorder
names(catches)=c("Week", "Legal", "Legal-Size Eggs", "Undersize Eggs", "Undersize")


bar.p=melt(catches, id.vars="Week")

mycolors <- c("green3", "firebrick4", "coral2", "red1")

png(filename=paste0(figdir,"/weekly.catch.composition.",aa, ".png"))
print(ggplot(bar.p, aes(x = Week, y = value, fill = variable)) +
  geom_col(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=mycolors, name="Catch Composition") +
  labs( title=paste0(aa), x="Week", y= "% of Catch") 
)
dev.off()


}



#Vents
#Comparing catch of 50mm (1 15/16") vs 44mm (1 3/4") 
#**Number and placement of vents may also differ

l27=b[b$LFA=="27",]
l27=l27[l27$yr>2016,]
big.vent=l27[l27$CAPTAIN %in% c("David Ferguson", "DAVID FERGUSON"),]
big.vent$vent="Big"
small.vent=l27[l27$CAPTAIN %in% c("Raymond Sherwood", "RAYMOND SHERWOOD"),]
small.vent$vent="Small"

escape=rbind(big.vent, small.vent)
biggest=5*(ceiling(max(escape$CARLENGTH)/5))
vents=unique(escape$vent)
escape$size[escape$vent=="Small"]="44mm"
escape$size[escape$vent=="Big"]="50mm"


for (v in vents){

png(filename=paste0(figdir,"/", v, "Vent.png"))
    all=hist(escape$CARLENGTH[escape$vent==v], xlim=c(0,biggest), breaks=seq(0, biggest, 2.5), main=paste0(escape$size[escape$vent==v][1], " Vent"), 
    col='grey88', xlab="Carapace Length", ylab="Number of Lobster")
abline(v = 82.5, col="red", lwd=2, lty=2)
dev.off()
}

#Hist lines
if (plot.hist.line){
  png(filename=paste0(figdir,"/","Vent Comparison.png")) 
    a=hist(escape$CARLENGTH[escape$vent=="Big"],plot=F,  breaks=seq(0, biggest, 2.5)) 
    plot(a$mids,a$density,type='l', xlim=c(0,biggest),col='blue', xlab="Carapace Length", ylab="Proportion of Catch",main="Vent Comparison")
    
    b=hist(escape$CARLENGTH[escape$vent=="Small"],plot=F,  breaks=seq(0, biggest, 2.5)) 
    lines(b$mids,b$density,type='l',col='red')
    
    abline(v = 82.5, col="grey50", lwd=0.5, lty=2)
    legend(x = "topright", legend = c("44mm", "50mm"), inset=0.05,lty = c(1, 1), col = c("red", "blue"), lwd = 2, title="Vent Height") 
  dev.off()
}

vent.catches=data.frame(matrix(, nrow=length(unique(vents)), ncol=5))
names(vent.catches)=c("Vent","Legal", "Undersize", "Over.Berried", "Under.Berried")
vent.catches$Vent=vents

for (i in 1:length(vents)){
  l=vents[i]
  c=escape[escape$vent==l,]
  u=c[c$CARLENGTH<83 & c$SEX %in% c(1,2),]
  u.ber= c[c$SEX=="3" & c$CARLENGTH<83,]
  o.ber=c[c$SEX=="3" & c$CARLENGTH>82,]
  
  #discard=rbind(u,berried)
  
  tot=length(c$SEX)
  under=(length(u$SEX))
  o.egg=length(o.ber$SEX)
  u.egg=length(u.ber$SEX)
  dc.num=under+u.egg+o.egg
  
  vent.catches$Legal[vent.catches$Vent==l]=tot-under-o.egg-u.egg
  vent.catches$Undersize[vent.catches$Vent==l]=under
  vent.catches$Over.Berried[vent.catches$Vent==l]=o.egg
  vent.catches$Under.Berried[vent.catches$Vent==l]=u.egg
  }

names(vent.catches)=c("Vent", "Legal", "Undersize", ">mls w/eggs", "<mls w/eggs")
vent.catches=vent.catches[,  c("Vent", "Legal", ">mls w/eggs", "<mls w/eggs", "Undersize")]
vent.catches$Vent[vent.catches$Vent=="Small"]="44mm"
vent.catches$Vent[vent.catches$Vent=="Big"]="50mm"

bar.v=melt(vent.catches, id.vars="Vent")

ggplot(bar.v, aes(x = Vent, y = value, fill = variable)) + geom_col(position="fill")+ scale_y_continuous(labels = scales::percent)

ggplot(bar.v, aes(x = LFA, y = value, fill = variable)) + geom_col(position="fill")+ scale_y_continuous(labels = scales::percent ) + stat_n_text(size = 4)





