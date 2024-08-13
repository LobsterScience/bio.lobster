require(ggplot2)
require(sf)
require(bio.lobster)
require(lubridate)
require(tidyr)
require(dplyr)
require(dbscan)
setwd('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/Maturity')
x = read.csv('LobsterMaturityDatabase.csv')
v = read.csv('matClean.csv')
b = read.csv('Maturity_GCIFA.csv')


## Add LFAs
x$LFA <- ""
x$LFA <-ifelse(x$Location == "Lobster Bay", 34, x$LFA)
x$LFA <-ifelse(x$Location == "Harrigan Cove" | x$Location == "Tangier"| x$Location == "Mushaboom", 32, x$LFA)
x$LFA <-ifelse(x$Location == "Port Mouton", 33,x$LFA)
x$LFA <-ifelse(x$Location == "Canso", "31A", x$LFA)

x=subset(x, LFA != "31A")


x = subset(x,select=c(Date,Lat, Long))
x$Proj = 'DFO_AS'
x$Date = as.Date(x$Date,format='%d/%m/%y')
x$Long = x$Long*-1

v=subset(v,select=c(Date,Y,X))
v$Date=as.Date(v$Date)
v$Proj = 'DFO_VH'

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
  rename(Lat = Latitude_DD, Long = Longitude_DD)
b = subset(b,select=c(Date,Lat,Long))
b$Long = b$Long*-1
b$Proj = 'GCIFA'
names(b) = names(x) = names(v)
co = bind_rows(list(x,v,b))
co = subset(co,!is.na(X))

co = st_as_sf(co,coords=c('X','Y'),crs=4326)
coo = st_coordinates(co)

cuco = dbscan(coo,eps=.3,minPts = 5)
co$grp = cuco$cluster


co = subset(co,grp>0)
o = list()
k = unique(co$grp)
for(i in 1:length(k)){
  w = subset(co,grp==k[i])
  ww = st_union(w)
  o[[i]] = st_as_sf(st_convex_hull(ww))
}

cp = (do.call(rbind,o))
cp$ID = 1:nrow(cp)
cp = subset(cp,ID !=2)

require(randomcoloR)
col = setNames(distinctColorPalette(14),c(1,3:15) )
cp = merge(cp,data.frame(ID=names(col),Col = col))

cents = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))

p = ggLobsterMap('inshore',addGrids = F,addLFALabels = T,LFA_label_size = 4,fill.colours = 'white',bathy=F,return.object = T,lwd=10)
p+geom_sf(data=cp,fill=cp$Col)+geom_sf_text(data=cents, aes(label=label),family='sans',size=4)



