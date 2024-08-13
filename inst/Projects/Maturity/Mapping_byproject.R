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


setwd(file.path(project.datadirectory('bio.lobster'),'data','Maturity'))
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
#need to cluster on groups

co$Proj2 = ifelse(co$Proj=='DFO_AS','DFO_Silva',ifelse(co$Proj=='DFO_VH','DFO_VH',ifelse(co$Proj %in% c('31B','31A'),'GCIFA',NA)))

ii = unique(co$Proj2)
oo=list()

for(j in 1:length(ii)){
  cop = subset(co,Proj2==ii[j])
  coo = st_coordinates(cop)
  
  cuco = dbscan(coo,eps=.3,minPts = 5)
  cop$grp = cuco$cluster
  
  
  cop = subset(cop,grp>0)
  o = list()
  k = unique(cop$grp)
  
  for(i in 1:length(k)){
    w = subset(cop,grp==k[i])
    ww = st_union(w)
    o[[i]] = st_as_sf(st_convex_hull(ww))
  }
  
  cp = (do.call(rbind,o))
  cp$ID = 1:nrow(cp)
  cp$Proj=ii[j]
  oo[[j]]=cp
}

cp = do.call(rbind,oo)

# cp = subset(cp,ID !=2)
# cp = subset(cp,ID !=8)
# 
# 
# col=setNames(c("#31688e","#f89540","#cc4778","#20a486","#5c01a6","#0d0887","#9c179e",
#                "#feb078","#f0f921","#781c6d","#5ec962", "#d84c3e","#c8e020"),c(1,3:7,9:15))
# 
# 
# cp = merge(cp,data.frame(ID=names(col),Col = col))

cents = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
ns_coast = readRDS(file.path(project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))



# ledg<- data.frame(ID = cp$ID)
# ledg$DataSource<-c("DFO_Silva",
#                    "DFO_Silva",
#                    "DFO_Silva",
#                    "DFO_Silva",
#                    "DFO_Silva",
#                    "DFO_Silva",
#                    "DFO",
#                    "DFO",
#                    "DFO",
#                    "DFO",
#                    "DFO",
#                    "DFO",
#                    "GCIFA")
# 
# # Perform the left join
# cp_join<- left_join(cp, ledg, by = "ID")
#p+geom_sf(data= cp_join,fill= cp_join$Col)+geom_sf_text(data=cents, aes(label=label),family='sans',size=4)

##Generic LFA MAP
yl=c(42.5,48); xl=c(-67.4,-57.8)
cenn = subset(cents, PID !='27')
p = ggLobsterMap(area='inshore',addGrids = F,addLFALabels = T,LFA_label_size = 4,fill.colours = 'white',bathy=F,return.object = T,lwd=10)
p+geom_sf_text(data=cenn, aes(label=label),family='sans',size=5)
p

##DFO SAMPLING
yl=c(43,45.5); xl=c(-67.4,-62.5)
p = ggLobsterMap(xmin=xl,ylim=yl,addGrids = F,addLFALabels = T,LFA_label_size = 4,fill.colours = 'white',bathy=F,return.object = T,lwd=10)
cenn = subset(cents, PID !='27')
p+geom_sf(data= subset(cp,Proj=='DFO_VH' ),fill= '#00C2D1')+geom_sf(data=ns_coast)+geom_sf_text(data=cenn, aes(label=label),family='sans',size=4)+xlim(xl)+ylim(yl)


##GCIFA SAMPLING
yl=c(44.25,46); xl=c(-62.5,-59)
p = ggLobsterMap(xmin=xl,ylim=yl,addGrids = F,addLFALabels = T,LFA_label_size = 4,fill.colours = 'white',bathy=F,return.object = T,lwd=10)
cenn = subset(cents, PID !='27')
p+geom_sf(data= subset(cp,Proj=='GCIFA' ),fill= '#F9E900')+geom_sf(data=ns_coast)+geom_sf_text(data=cenn, aes(label=label),family='sans',size=4)+xlim(xl)+ylim(yl)


### DFO SILVA SAMPLING
yl=c(42.8,45.5); xl=c(-67,-59.8)
p = ggLobsterMap(xmin=xl,ylim=yl,addGrids = F,addLFALabels = T,LFA_label_size = 4,fill.colours = 'white',bathy=F,return.object = T,lwd=10)
cenn = subset(cents, PID !='27')
p+geom_sf(data= subset(cp,Proj=='DFO_Silva' & ID !=2),fill= '#ED4343')+geom_sf_text(data=cenn, aes(label=label),family='sans',size=4)+xlim(xl)+ylim(yl)

