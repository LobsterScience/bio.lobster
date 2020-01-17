#create grids for lobster
require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
require(gulf)
n = 500
setwd('/home/adam/git/LobsterSpatialModel/SWNSBoF')

#Make outer polygon
LFAs = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
LFAs = subset(LFAs,PID %in% c(34,35,36,38))
LFAs = joinPolys(LFAs,operation='UNION')
keepIslands = FALSE

if(keepIslands){
	g = aggregate(POS~SID,data=LFAs,FUN=length)
	g = subset(g,POS>75)
	LFAs = subset(LFAs,SID %in% g[,1])
	plotPolys(LFAs)
	}
if(!keepIslands){
	g = aggregate(POS~SID,data=LFAs,FUN=length)
	g = g$SID[which(g$POS==max(g$POS))]
	LFAs = subset(LFAs,SID %in% g)
	plotPolys(LFAs)
	
}

	longitude = LFAs$X
	latitude  = LFAs$Y

	attr(LFAs,"projection") <- "LL" 
	lfas = convUL(LFAs)

#Area of polygon and grids
	a = sum(calcArea(lfas))
	d = sqrt(a/n) 

	cat(paste("Polygon area is", round(a, 4), " square kilometers.\n"))
	cat(paste("Square grid dimension is", round(d, 4), "kilometers.\n"))


#bounding box
	bb <- c(min(lfas$X, na.rm = TRUE),
        min(lfas$Y, na.rm = TRUE),
        max(lfas$X, na.rm = TRUE),
        max(lfas$Y, na.rm = TRUE))

# Expand bounding box that top-right coordinate matches grid scale:
		bb[3] <- bb[1] + d*(floor((bb[3]-bb[1]) / d) + 1)
		bb[4] <- bb[2] + d*(floor((bb[4]-bb[2]) / d) + 1)

# Define coordinates of superimposed grid lattice:
			x <- seq(bb[1]-3*d, bb[3], by = d)
			y <- seq(bb[2]-3*d, bb[4], by = d)
			xx <- as.vector(repvec(x, nrow = length(y)))
			yy <- as.vector(repvec(y, ncol = length(x)))

# Convert to 'grids' format:
# Convert corner coordinates to latitude-longitude:
			gx.utm <- cbind(xx, xx, xx + d, xx + d, xx)
			gy.utm <- cbind(yy, yy + d, yy + d, yy, yy)
			a1 <- km2deg(xx, yy)
			a2 <- km2deg(xx, yy+d)
			a3 <- km2deg(xx+d, yy+d)
			a4 <- km2deg(xx+d, yy)


# Define grid corner coordinates:
		gx <- cbind(a1$longitude, a2$longitude, a3$longitude, a4$longitude, a1$longitude)
		gy <- cbind(a1$latitude, a2$latitude, a3$latitude, a4$latitude, a1$latitude)

# Retain only the grids that fall in the 33-38 poly
		index <- rep(FALSE, nrow(gx))
			for(ii in 1:nrow(gx)){
  					if(sum( point.in.polygon( gx[ii, ], gy[ii, ],longitude, latitude) ) >0)index[ii]<-TRUE
				}
			gx=gx[index, ]
			gy=gy[index, ]
			gx.utm <- gx.utm[index, ]
			gy.utm <- gy.utm[index, ]
			#plot grids
			for(ii in 1:nrow(gx)){
			  lines(gx[ii,],gy[ii,])
			}	


	# Calculate proportion of each grid which lies within the inference polygon using a fine mesh:
dx <- range(gx.utm)
dy <- range(gy.utm)
step <- 0.5 # Dimensions in kms of point mesh.
mesh.km <- expand.grid(seq(dx[1], dx[2], by = step), seq(dy[1], dy[2], by = step))
mesh <- km2deg(mesh.km[, 1], mesh.km[, 2])
mesh$grid <- 0
for (ii in 1:nrow(gx)){
  index <- in.polygon(as.polygon(gx[ii,], gy[ii, ]), mesh$longitude, mesh$latitude)
  mesh$grid[index] <- ii
}
mesh<-mesh[mesh$grid >0,] #exclude mesh points that are not in grids

# points(mesh[!is.na(mesh$grid), 1], mesh[!is.na(mesh$grid), 2], cex = 0.1, col = "red", bg = "red", pch = 21)
mesh$in.survey <- point.in.polygon( mesh[, 1], mesh[, 2],longitude, latitude)

res <- aggregate(list(n = mesh$grid), by = mesh["grid"], length)
res <- cbind(res, aggregate(list(k = mesh$in.survey), by = mesh["grid"], function(x) sum(x))["k"])
res$proportion <- res$k / res$n

gridinfo=cbind(res[,c('grid','proportion')],gx,gy)
names(gridinfo)=list('grid','proportion','x1','x2','x3','x4','x5','y1','y2','y3','y4','y5')

#eliminate grids that overlap little with the inference polygon; arbitrarily choose 5% overlap cutoff
gridinfo=gridinfo[gridinfo$proportion>=0.05,]

#check the results
x11()
plotPolys(LFAs)
lines(longitude, latitude,col='red',lwd=2)
for(ii in 1:nrow(gridinfo)){
    with(gridinfo[ii,], polygon(c(x1,x4,x3,x2),c(y1,y4,y3,y2),col =  rgb(1-proportion, 1-proportion, 1-proportion, alpha = 0.8)  )  )
} 
   

#create adjacency matrix for grids; required for the spatial autocorrelation
adj=matrix(rep(99,nrow(gx)^2),nrow=nrow(gx),ncol=nrow(gx) )
for(ii in 1:nrow(gx)){
  for(jj in 1:nrow(gx)){
    if( sum((round(gx[ii,1:4],3) %in%  round(gx[jj,1:4],3)) * (round(gy[ii,1:4],3) %in%  round(gy[jj,1:4],3)))==2 ){  
      adj[ii,jj]=1
    }
    else{
      adj[ii,jj]=0
    }
    if(ii==jj){adj[ii,jj]=-1}
  }
  
}


## write or save the gridinfo and adjacency dataframes
if(save) {
  write.table(   cbind(gridinfo,area=d^2)  ,"lobster_spatial_model\\34_38grids.dat",,sep="\t",row.names=FALSE)
  write.table(adj,"lobster_spatial_model\\adjacency matrix.dat",,sep="\t",row.names=FALSE,col.names=FALSE)
}



## Now need to assign each survey set to a grid; show example with Quebec RV survey
c.set=read.csv( file.path("create inputs","Te_Set.csv"),sep=';',header=T,as.is=T,na.strings=c("NA","NaN"," ",". ") )
c.set=c.set[c.set$source %in% c(6,7,8,16) & c.set$resultat %in% c(1,2) & c.set$typtrait==1, ]
c.set=c.set[c.set$opano != '4T ',]  #exclude sets that cover 4T
c.set$lat=floor(c.set$lat_deb/100) + (c.set$lat_deb- 100*floor(c.set$lat_deb/100))/60
c.set$lon=-(floor(c.set$lon_deb/100) + (c.set$lon_deb- 100*floor(c.set$lon_deb/100))/60)
for(ii in 1:nrow(gridinfo)){
  index_all<-as.logical(point.in.polygon(c.set$lon,c.set$lat,t(gridinfo[ii,c('x1','x2','x3','x4','x5')]),t(gridinfo[ii,c('y1','y2','y3','y4','y5')])))
  c.set$grid[index_all==T]=gridinfo$grid[ii]
}
c.set.out=c.set[!is.na(c.set$grid),] # exclude a few sets that are in 4T

##for AMC
## now merge the sets with your catches, as inputs to the model...
#made same as 4T_SC/data/sGSL_SC_SurveyData_withPLarge........

## need to create a file with depths (on a fine mesh) in each grid cell
#