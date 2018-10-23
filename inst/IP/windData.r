
 install.package("rWind")  
   
 library(rWind)  
   
 library(fields)  
 library(shape)  
 library(rworldmap)  
 library(lubridate)  
 # Now, we use lubridate package to create a sequence of dates/times (each three  
 # hours)  
   
 dt <- seq(ymd_hms(paste(2018,6,25,00,00,00, sep="-")),  
      ymd_hms(paste(2018,7,4,21,00,00, sep="-")),by="3 hours")  
   
 # Now, we use the new function wind.dl_2 to download the whole time series of  
 # wind data. We use the "dt" object created with lubridate to provide the input   
 # to wind.dl_2. Since it's a large area and many days, it could take a while...  
   x1 = -75
   x2 = -45
   y1 = 35
   y2 = 55
 wind_series <- wind.dl_2(dt,x1,x2,y1,y2)  

 wind_series_layer <- wind2raster(wind_series)  
   

library(animation)

#Set delay between frames when replaying
ani.options(interval=.25)

saveGIF({
id<-0  
 for (i in 1:72) {  
  id <- sprintf("%03d", i)  
  image.plot(wind_series_layer[[i]]$wind.speed, col=bpy.colors(1000),  
        zlim=c(0,18), main =wind_series[[i]]$time[1])  
 map('world',add=T,col='white')
 }
 })  
