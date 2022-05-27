#making grid group connections

require(bio.lobster)
require(bio.utilities)
require(sp)
library(rgdal)
library(proj4)
library(spdep)

io = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA_33_TO_ 38_GRID_GROUPS_NAD83.csv"))
io$X1<- NULL

io$PID = as.numeric(paste(io$PID,io$SID,sep=""))

g =split(io,f=io$PID)
nm = c()
gp = list()
for(i in 1:length(g)){
  gp[[i]] = Polygons(list(Polygon(g[[i]][,c('X','Y')])),unique(g[[i]]$PID))
}
gpp = SpatialPolygons(gp,proj4string=CRS("+proj=longlat +datum=WGS84"))
gpnb = poly2nb(gpp,row.names=names(gpp))
names(gpnb)=names(gpp)

#usage
#CTF = formula(CODWEIGHT~ (Year) + s(LOCIDS, bs = 'mrf',xt = list(nb = gpnb))+offset(lHook))
#$CTM = gam(CTF,data=CF, family = Tweedie(p=1.25,link=log), method = "REML")
