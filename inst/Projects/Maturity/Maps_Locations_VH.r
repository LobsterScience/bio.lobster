require(ggplot2)
require(sf)
require(bio.lobster)
require(lubridate)
require(tidyr)
require(dplyr)
require(dbscan)
require(randomcoloR)
require(viridis)
require(viridisLite)

setwd('C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/Maturity')
x = read.csv('LobsterMaturityDatabase.csv')
v = read.csv('matClean.csv')
b = read.csv('Maturity_GCIFA.csv')


######## DFO_Silva Data ########
## Add LFAs
x$LFA <- ""
x$LFA <-ifelse(x$Location == "Lobster Bay", 34, x$LFA)
x$LFA <-ifelse(x$Location == "Harrigan Cove" | x$Location == "Tangier"| x$Location == "Mushaboom", 32, x$LFA)
x$LFA <-ifelse(x$Location == "Port Mouton", 33,x$LFA)
x$LFA <-ifelse(x$Location == "Canso", "31A", x$LFA)


x=subset(x, Sex == 2)
x = subset(x,select=c(Date,Lat,Long,LFA))
x$Proj = 'DFO_AS'
x$Date = as.Date(x$Date,format='%d/%m/%y')
x$Long = x$Long*-1


######## DFO_Howse Data #######
v=subset(v, Sex == 2)
v$LFA<-as.character(v$LFA)
v=subset(v,select=c(Date,Y,X,LFA))
v$Date=as.Date(v$Date)
v$Proj = 'DFO_VH'



######## GCIFA Data ########

#### Clean and convert ####

b=subset(b, Sex == 2)
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
b$Long = b$Long*-1
b$Proj = 'GCIFA'
b = subset(b,select=c(Date,Lat,Long,Proj,LFA))


names(b) = names(x) = names(v)
co = bind_rows(list(x,v,b))
co = subset(co,!is.na(X))  ## These NAs were just some berried females we measured from curiosity

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
cp = subset(cp,ID !=8)


col=setNames(c("#31688e","#f89540","#cc4778","#20a486","#5c01a6","#0d0887","#9c179e",
                "#feb078","#f0f921","#781c6d","#5ec962", "#d84c3e","#c8e020"),c(1,3:7,9:15))


cp = merge(cp,data.frame(ID=names(col),Col = col))

cents = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))


 ledg<- data.frame(ID = cp$ID)
 ledg$DataSource<-c("DFO_Silva",
                  "DFO_Silva",
                  "DFO_Silva",
                  "DFO_Silva",
                  "DFO_Silva",
                  "DFO_Silva",
                  "DFO",
                  "DFO",
                  "DFO",
                  "DFO",
                  "DFO",
                  "DFO",
                  "GCIFA")

 # Perform the left join
 cp_join<- left_join(cp, ledg, by = "ID")

 
p = ggLobsterMap(ylim=c(42.8,46), xlim=c(-67.4,-59),addGrids = F,addLFALabels = T,LFA_label_size = 4,fill.colours = 'white',bathy=F,return.object = T,lwd=10)
p+geom_sf(data= cp_join,fill= cp_join$Col)+geom_sf_text(data=cents, aes(label=label),family='sans',size=4)

p + 
  geom_sf(data = cp_join, aes(fill = DataSource, color = DataSource)) + 
  scale_fill_manual(values = setNames(cp_join$Col, cp_join$DataSource), guide = 'legend') +
  scale_color_manual(values = setNames(cp_join$Col, cp_join$DataSource), guide = 'legend') 


 
 #### ISSUES: 
# -is it possible that the polygons can not cover land? I.e. LFA 38 sampling?
# - The Angelica + GCIFA sampling in LFA 31A are either overlapping or they were "polygonned" together in a single event
# - Is it possible to have two separate polygons there and both be visible

# I did the colouration and legend in an ass backwards way but I couldn't figure out how to keep the DataSource information 
#in the sf object earlier so that we could use it for labelling. I also think maybe just the source colour coding in fine
# instead of the dates + Source like I had originally thought.
 
