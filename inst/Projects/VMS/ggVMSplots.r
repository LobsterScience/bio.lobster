#VMS_plots
require(sf)
require(devtools)
require(ggplot2)
require(bio.lobster)
la()

lobster.db('logs41')
vms.data = lobster.db('lfa41.vms')
logs41$YEAR = year(logs41$FV_FISHED_DATETIME)

logs41r = subset(logs41, !is.na(DDLON) & !is.na(DDLAT) & !is.na(FV_FISHED_DATETIME))
logs41r$DDLON = logs41r$DDLON * -1

l = st_as_sf(logs41r,coords = c('DDLON','DDLAT'),crs=4326)
v = st_as_sf(vms.data,coords = c('LON','LAT'),crs=4326)
  
l$DATE  =  as.Date(format(strftime(l$FV_FISHED_DATETIME,format="%Y-%m-%d"), tz="America/Halifax",usetz=TRUE))
v$DATE = as.Date(v$DATE)

l$id = paste(l$VR_NUMBER,l$DATE,sep="-")
v$id = paste(v$VRN,v$DATE,sep="-")

da = unique(l$id)
dav = unique(v$id)
ids = intersect(dav,da)

#something wrong, keeps filtering out good vms data

pdf(file=file.path(project.figuredirectory('bio.lobster'),'lfa41LogsVMS.pdf'), onefile = T)
for(d in 1: length(ids)) {
 ll = subset(l,id == ids[d])
 vv = subset(v,id == ids[d])
gall = ggplot()+
  geom_sf(data=ll,colour='red')+
  geom_sf(data=vv,colour='blue')+
  labs(title=paste(ids[d],'all'))

vvv = subset(vv,SPEED_KNOTS<5)
dist_matrix <- st_distance(vvv,ll)
points_to_remove <- apply(dist_matrix, 1, function(row) all(row < 20000))
vvv <- vvv[points_to_remove, ]

gs = ggplot()+
  geom_sf(data=ll,colour='red')+
  geom_sf(data=vvv,colour='blue')+
  labs(title=paste(ids[d],'speed and distance filtered'))

print(gridExtra::grid.arrange(gall,gs,ncol=2))
}
dev.off()