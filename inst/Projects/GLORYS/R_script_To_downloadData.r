# .R file to follow the article: How to download data via the Copernicus Marine Toolbox in R?

# Please run install.packages("lubridate") is you have not already installed it
library(lubridate)

outdir = setwd("~/tmp/GLORYS") # set work directory
print(outdir)

# ================ Variables for your query ================

# Dataset ID
dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m"

# Ocean Variable(s)
# Please keep the space at the beginning
# To add another variable add " --variable your_var" after the last one 
variable <- c(" --variable so --variable bottomT")

# Time range
date_min = ymd(20040101) # start_date
date_max = ymd(20041231) # end_date

# Geographic area and depth level 
lon = list(-70, -58)  # lon_min, lon_max
lat = list(41, 48) # lat_min, lat_max
depth = list(0.4941, .4941) # depth_min, depth_max

# ===================== Copernicus Marine Toolbox command ==============================

# Output filename    
filename = paste("GLORYS",date_min,".nc", sep="")

# Toolbox command

path_copernicusmarine = "C:\\Users\\Cooka\\Documents\\.virtualenvs\\CopernicusMarine\\Scripts\\copernicusmarine.exe"


####single
command <- paste (path_copernicusmarine, " subset -i", dataset_id,                    
                "-x", lon[1], "-X", lon[2],                  
                "-y", lat[1], "-Y", lat[2],
                "-t", date_min, "-T", date_max,
                "-z", depth[1], "-Z", depth[2],                    
                variable, "-o", outdir, "-f", filename, 
                "--force-download", sep = " ")

print(paste("======== Download starting on",date_min,"========"))
print(command)
system(command, intern = TRUE)
print(paste("============= Download completed! File is stored in ",outdir,"=============", sep=" "))

#######################################################################################################################
##multiple 
# Dataset ID
dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m"


dmi = list()
dmx = list()
os = 20040101
oe = 20041231
yrs = 2021-2004+1
for(i in 1:yrs){
  if(i==1) {
    dmi[[i]]=ymd(os)
    dmx[[i]] =ymd(oe)
  } else {
    dmi[[i]] = ymd(os + ((i-1)*10000))
    dmx[[i]] = ymd(oe + ((i-1)*10000))
  }
}
#if date is early https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/download?dataset=cmems_mod_glo_phy_my_0.083deg_P1D-m_202311
dmx[[yrs]] = ymd(20210630)


#now loop through commands
for(i in 1:length(dmx)){
  filename = paste("GLORYS",dmi[[i]],".nc", sep="")
  
  command <- paste (path_copernicusmarine, " subset -i", dataset_id,                    
                  "-x", lon[1], "-X", lon[2],                  
                  "-y", lat[1], "-Y", lat[2],
                  "-t", dmi[[i]], "-T", dmx[[i]],
                  "-z", depth[1], "-Z", depth[2],                    
                  variable, "-o", outdir, "-f", filename, 
                  "--force-download", sep = " ")

print(command)
system(command, intern = TRUE)
print(paste("============= Download completed! ",i, sep=" "))
}

#######################################################################################################################
###forecasts
dataset_id = "cmems_mod_glo_phy_myint_0.083deg_P1D-m"

##multiple 
dmi = list()
dmx = list()
os = 20210101
oe = 20241231
yrs = 4
for(i in 1:yrs){
  if(i==1) {
    dmi[[i]]=ymd(os)
    dmx[[i]] =ymd(oe)
  } else {
    dmi[[i]] = ymd(os + ((i-1)*10000))
    dmx[[i]] = ymd(oe + ((i-1)*10000))
  }
}
dmi[[1]] = ymd(20210701)
dmx[[yrs]] = ymd(20240326)


#now loop through commands
for(i in 1:length(dmx)){
  filename = paste("GLORYS_int",dmi[[i]],".nc", sep="")
  
  command <- paste (path_copernicusmarine, " subset -i", dataset_id,                    
                    "-x", lon[1], "-X", lon[2],                  
                    "-y", lat[1], "-Y", lat[2],
                    "-t", dmi[[i]], "-T", dmx[[i]],
                    "-z", depth[1], "-Z", depth[2],                    
                    variable, "-o", outdir, "-f", filename, 
                    "--force-download", sep = " ")
  
  print(command)
  system(command, intern = TRUE)
  print(paste("============= Download completed! ",i, sep=" "))
}
