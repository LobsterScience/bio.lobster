#' @export
# this is a little function to convert PBSmapping format data into shapefiles, can be either spatial polygons or spaital lines

# Arguements

#1 dat:         The data file you want to load, for the moment at least this can only be a csv file. 
#2 proj:        The projection of the data, this is for PBSmapping, at this time it can only be "LL", "UTM", or "NULL".
#3 c_sys:       The coordinate system tied to your data. This can handle the default of "WGS84", "NAD83", "NAD27", "UTM19", and "UTM20"
#                UTM zone 20 is best for BoF and SS, UTM 19 is best for GB, SPA3, and SPA6, and the greater GOM area
#4 type:        The type of shapefile you will make.  Default is "lines" which will make a spatiallines dataframe.  
#               If you know you have closed polygons go with "polygon".
#5 layer.names: The name of the layers in your shapefile.  The default is NULL which will look in your object for a column called "label"
#               and will use that as the layer names.  You can also specify the column name in the data that you want to use as your name.
#6 save.loc:    The location you want to save the shapefiles to.  This is specified as a directory only, each layer will get placed into this
#               directory as a series of files (i.e. GIS shapefiles.)
#7 env.object:  Would you rather save the object to your R environment, instead of saving to a file? (T or F)
#8 spdf:        Do you want a SpatialPolygonsDataFrame object? (T) Or just a SpatialPolygons object (F)
#9 make.sf:     Do you want to make this an SF obejct.  Only works if env.object = T (i.e. you want to return object to R workspace). Default = F for compatibility with older scripts.

pbs.2.gis = function(dat, proj = "LL",c_sys = "WGS84",type = "lines",layer.names = NULL,
                     save.loc, env.object=F, spdf=T,make.sf = F){

options(stringsAsFactors = F)
# Load the libraries you need.
require(splancs) || stop("You need le package splancs, thanks!")
require(PBSmapping) || stop("You need PBSmapping, thanks!")
require(sp) || stop("You need sp, thanks!")
require(rgdal)  || stop("You need rgdal, thanks!")
require(rgeos) || stop("You need rgeos, thanks!")
require(raster) || stop("You need raster, thanks!")
require(maptools) || stop("You need maptools, thanks!")
require(maps) || stop("You need maps, thanks!")
require(mapdata)|| stop("You need mapdata, thanks!")

# Here is our data of interest.
#
if(any(grepl(x=dat, pattern="csv"))==T) dat <- read.csv(dat)

# For PBS mapping to know what these are the lat and lon columns need to be called Y and X respectively, if you haven't done this spit out a warning

if(length(which(names(dat) %in% c("X","Y"))) < 2) stop("You need to rename your lat and lon columns to 'X' and 'Y' for this to work.")

# Get rid of any rows where the X or Y is an NA as this breaks PBSmapping.
dat <- dat[!is.na(dat$X),]
dat <- dat[!is.na(dat$Y),]

# If you don't specify the layer names you need to have a label column in the PBSmapping object that has a name for each of the layers
# in there, without that you can't keep track of the names of each layer.
if(is.null(layer.names)) layer.name <- unique(dat$label)
if(!is.null(layer.names)) layer.name <- unique(dat[,names(dat) == layer.names])
if(is.null(layer.names)) layer.names <- "label"

# Now we set up the main 3 coordinate systems with lat/lon data that I know of, first WGS84, 
if(c_sys == "WGS84") c_sys <- "+init=epsg:4326"
# We believe these would be NAD 83, i..e EPSG:4269
if(c_sys == "NAD83") c_sys <- "+init=epsg:4269"
# If NAD 27 it is EPSG:4267
if(c_sys == "NAD27") c_sys <- "+init=epsg:4267"
# We can also toss in the two main UTM zones in the region
# for utm_19  use +init=epsg:32619, this is best for GB, SPA3 and 6
if(c_sys == "UTM19") c_sys <- "+init=epsg:32619"
# if you want utm_20 you can enter "+init=epsg:32620" which will use utm zone 20 which is best for BoF and SS 
if(c_sys == "UTM20") c_sys <- "+init=epsg:32620"
# if you know your shit you can just directly use the datum that your data is right away, but needs set up in this CRS friendly format ("+init=epsg:####)

# now in case the data has repeated PID's in it we need to do this slightly clumsily...
tmp <- NULL
dat.sdf <- NULL
if(env.object == T) dat.sdf.list <- NULL
for(i in 1:length(layer.name))
{
 
  subs <- which(dat[,names(dat) == layer.names] == layer.name[i])
  tmp <- dat[subs,]
  tmp <- as.PolySet(tmp,projection = proj) # The proj can be "LL", "UTM", or "NULL".
 
  if(type == "polygon") dat.sp <- PolySet2SpatialPolygons(tmp)
  if(type == "lines") dat.sp <- PolySet2SpatialLines(tmp)
  
  # Now we project to the coordinate system we believe the data to be, we DO NOT use spTransform here as we are assuming we know the coordinate system the data is in
  # the PolySet2Spatial... assumes a coordinate system based on the data provided (LL data gets WGS84), here you assume you know better
  proj4string(dat.sp) <- CRS(c_sys) # This will spit out a warning, but that's o.k. based on above comment, I'm assuming you know what you are doing...
  
  # for a spatialpolygons object
  if(spdf==F) dat.sdf <- dat.sp
  
  # For a spatialpolygonsdataframe
  if(spdf==T){
    
    # Now this puppy won't be a spatial dataframe, if we want a spatial dataframe we need to add a dummy variable to this...
    # Create a dataframe and display rownames
    
    # Now replace each of the slots in the spatial data frame with a counter from 1:j
    if(type == "polygon") for(j in 1:length(slot(dat.sp, "polygons"))) slot(slot(dat.sp, "polygons")[[j]], "ID") <- as.character(j)
    if(type == "lines") for(j in 1:length(slot(dat.sp, "lines"))) slot(slot(dat.sp, "lines")[[j]], "ID")  <-  as.character(j)
    
    # Create dataframe with correct rownames
    dat.sp.df <- data.frame(ID=1:length(dat.sp), row.names = as.character(1:length(dat.sp)))
    
    # Try coersion again and check class
    if(type == "lines") dat.sdf <- SpatialLinesDataFrame(dat.sp, dat.sp.df)
    if(type == "polygon") dat.sdf <- SpatialPolygonsDataFrame(dat.sp, dat.sp.df)
    
    if(env.object == T) dat.sdf.list[[i]] <- dat.sdf
  }
  # I think I need to add this 
  #names(dat.sdf) <- layer.name[i]
  # Now you can save this wonderful Spatial data frame into something that any old GIS program can read.
  if(env.object==F) writeOGR(dat.sdf,save.loc,driver = "ESRI Shapefile",layer =layer.name[i])
  if(make.sf == T) dat.sdf <- st_as_sf(dat.sdf)
  if(env.object==T) dat.sdf.list[[layer.name[i]]] <- dat.sdf
  
} # end for(i in 1:length(layer.name))

if(env.object==T)  return(dat.sdf.list)

}


####example
#load(file.path( project.datadirectory("bio.lobster"), "data","maps", 'topex', "bathyPoly1.rdata"))
#bathy.poly<-subset(bathy.poly,Z%in%seq(10,300,10))
#bathy.poly$label = paste(bathy.poly$PID,bathy.poly$SID,sep="-")
#r = pbs.2.gis(bathy.poly,make.sf = T,env.object = T,type='lines',spdf = F)
#r = bio.utilities::list.names.to.columns(r)
#rr = aggregate(Z~label,data=bathy.poly,FUN=max)
#r2 = merge(r,rr,by.x="V2",by.y='label')
#r2$V2 = NULL
#saveRDS(r2,file.path( project.datadirectory("bio.lobster"), "data","maps","bathy100SF.rds"))




