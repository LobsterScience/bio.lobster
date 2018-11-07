#(Brent)
# 2013+. A function to convert the eSonar formatted data to the Netmind format.
# Saves the origional eSonar file in the eSonar file branch and rewrites the new file in the
# netmind file branch

esonar2netmind = function(fn = NULL) {
  #Changing operation so that we simply convert esonar archive into netmind archive. -Brent
  nfn = gsub("esonar","netmind", fn, ignore.case = T)
  if (!file.exists(dirname(nfn)))  dir.create(dirname(nfn), recursive = T)

    #file.copy(fn,  nfn, overwrite = T)
    ew = read.csv(fn, nrows = 1, colClasses = c("character"),header=F)
    print(fn)
    if(!grepl('FileName',ew[1])) {
      esonar = read.csv(fn, skip = 8, colClasses = c("character"))
      colnames(esonar) = c("CPUDateTime","GPSDate","GPSTime","Latitude","Longitude","Speed","Heading","Validity","TransducerName","SensorName","SensorValue","ErrorCode","Hydrophone","SignalStrength")

     #if(fn== "/home/bio.data/bio.snowcrab/data/netmind/archive/2015/ep078.txt") browser()
      #Format time
      ir = which(is.na(as.numeric(esonar$SignalStrength)))
      if(length(ir>0)) esonar = esonar[-ir,]
      esonar$GPSTime = as.numeric(esonar$GPSTime)
      esonar$GPSTime = sprintf("%06d", esonar$GPSTime)

      header = read.delim(fn, nrows = 1)
      header = as.character(unlist(header))
      l1 = paste("FileName: ", fn, sep = "")
      l2 = paste("Local Time: ", esonar$CPUDateTime[which(esonar$CPUDateTime != "")][1], sep = "")
      l3 = ""
      l4 = paste("Ship:  Trip:", toupper(unlist(strsplit(header, ","))[2]), " Tow: ", unlist(strsplit(header, ","))[3])
      l5 = "Comments:"

      stop( "Must verify time formats in esonar inputs and converted netmind output")

    ets = as.Date(esonar$GPSDate, tz="UTC" ,format="%d-%b-%Y")  # should check this
    ets = format(ets,"%y%m%d")

      esonar$GPSDate = ets

      esonar$primary = NA  #Headline
      esonar$secondary = NA #Is nothing but may need in file
      esonar$doorspread = NA
      esonar$depth = NA
      esonar$temperature = NA

      esonar$depth[which(esonar$SensorName == "Depth")] = esonar$SensorValue[which(esonar$SensorName == "Depth")]
      esonar$primary[which(esonar$SensorName == "Headline")] = esonar$SensorValue[which(esonar$SensorName == "Headline")]
      esonar$doorspread[which(esonar$SensorName == "DoorMaster")] = esonar$SensorValue[which(esonar$SensorName == "DoorMaster")]
      esonar$temperature[which(esonar$SensorName == "Temperature")] = esonar$SensorValue[which(esonar$SensorName == "Temperature")]

      esonar$CPUDateTime = NULL
      esonar$TransducerName = NULL
      esonar$SensorName = NULL
      esonar$SensorValue = NULL
      esonar$Hydrophone = NULL
      esonar$SignalStrength = NULL
      esonar$Validity = NULL
      esonar$ErrorCode = NULL
      esonar$Heading = NULL
      colnames(esonar) = c("Date","Time","Latitude","Longitude","Speed","Primary","Secondary","DoorSpread","Depth", "Temperature" )

      sink(nfn )

      cat(l1, "\n")
      cat(l2, "\n")
      cat(l3, "\n")
      cat(l4, "\n")
      cat(l5, "\n")
      write.table(esonar, quote = F, row.names = F, sep = "\t")
      sink()
    }
  }

