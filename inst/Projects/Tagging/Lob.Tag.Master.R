p = bio.lobster::load.environment()
require(SpatialHub)
require(lubridate)

#la()

#Choose one
assessment.year = p$current.assessment.year 
#assessment.year = p$current.assessment.year-1 

figdir = file.path(project.datadirectory("bio.lobster","figures","tagging"))

dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

p$lfas = c("27", "29", "30", "31A", "31B", "32") # specify lfas for data summary (Modify as required)

#Import CSV that has one location for each tagtging trip to plot all tagging trip locations
dets=read.csv(file.path(project.datadirectory("bio.lobster","data","tagging", "locationmaster.csv")))

#add EID for mapping purposes
dets$EID=1:length(dets$Year)

#Add  point style (PCH) by sampler- Can add additional
#dets$pch=1
#dets$lwd=15
#dets$pch[dets$Sampler=="DFO"]=21 #DFO
#dets$pch[dets$Sampler=="CBFH"]=22 #Cape Breton Fish Harvesters
#dets$pch[dets$Sampler=="FSRS"]=24 #Fishermens and Scientists Research Society

#dets=dets[dets$Year=="2021",]
#Add point colour by year- Can add additional
#dets$col="black"
#dets$bg[dets$Year=="2021"]="red" #DFO
#dets$bg[dets$Year=="2022"]="chartreuse" #DFO

#Add colour by sampler- Can add additional
dets$pch=17

dets$col[dets$Sampler=="DFO"]="firebrick1" #DFO
dets$col[dets$Sampler=="CBFH"]="chartreuse2" #Cape Breton Fish Harvesters
dets$col[dets$Sampler=="FSRS"]="deepskyblue" #Fishermens and Scientists Research Society  

png(filename=file.path(figdir, "All_Tagging_Map.png") ,width=6.5, height=6.5, units = "in", res = 800)
LobsterMap('27-32.Crop', labels=NULL, points.lst=list(dets, dets), pt.cex=12)
text(y=44.7,x=-59.4, "DFO", col="firebrick1", cex=1.5, pos=4)
text(y=44.5, x=-59.4, "CBFH", col="chartreuse2", cex=1.5, pos=4)
text(y=44.3, x=-59.4, "FSRS", col="deepskyblue", cex=1.5, pos=4)
dev.off()

for (l in p$lfas){
  fn=paste(l, "Tagging", "Map.png", sep="_")
  a=l
  if (l=="27"){a="27.Crop"} 
  if (l=="30"){a="30.Crop"} 
  if (l=="31A"){a="31A.Crop"} 
  if (l=="31B"){a="31B.Crop"} 
  if (l=="32"){a="32.Crop"} 
  png(filename=file.path(figdir, fn) ,width=6.5, height=6.5, units = "in", res = 800)
  LobsterMap(area=a, labels=NULL, points.lst=list(dets, dets), pt.cex=12)
  if (l=="27"){text(y=45.7,x=-59.23, "DFO", col="firebrick1", cex=1.2, pos=4)}
  if (l=="27"){text(y=45.6, x=-59.23, "CBFH", col="chartreuse2", cex=1.2, pos=4)}
  if (l=="31B"){text(y=44.76,x=-60.8, "DFO", col="firebrick1", cex=1.3, pos=4)}
  if (l=="31B"){text(y=44.7, x=-60.8, "FSRS", col="deepskyblue", cex=1.3, pos=4)}
  if (l=="32"){text(y=44.56,x=-61.78, "DFO", col="firebrick1", cex=1.3, pos=4)}
  if (l=="32"){text(y=44.5, x=-61.78, "FSRS", col="deepskyblue", cex=1.3, pos=4)}
    dev.off() 
 }