#' @export

MNR_sets <- function(){
   fd = file.path(project.datadirectory('bio.lobster'),'data','MaineDMRSurvey')
            print('Data obtained from https://mainedmr.shinyapps.io/MaineDMR_Trawl_Survey_Portal/')
            x = dir(fd,full.names = T)
            x = x[grep('csv',x)]
            ou = list()
            for(i in 1:length(x)){
              ou[[i]] = read.csv(x[i])
            }
  
  tow = grep('Tow',x)
  se = ou[[tow]]
  
  len = grep('Length',x)
  de = ou[[len]]
  
  catc = grep('Catch',x)
  ca = ou[[catc]]
  
  se$id = paste(se$Survey,se$Tow_Number,sep="-")
  se$X = (se$Start_Longitude+se$End_Longitude)/2
  se$Y = (se$Start_Latitude+se$End_Latitude)/2
  se$z = ((se$Start_Depth_fathoms+se$End_Depth_fathoms)/2)*1.8288
  se$dist = se$Tow_LengthNM * 1.852
  se = subset(se, id != 'FL15-73',select=c(id,Start_Date,X,Y,z,dist,Region,Depth_Stratum,Tow_Time))
  se$spread = 11 #57 ft rope length; 11m from ASMFC benchmark 2020
  
  se$Dur_m=NA
  
  # Split the time string into minutes and seconds
  for(i in 1:nrow(se)){
    time_parts <- strsplit(se[i,'Tow_Time'], ":")[[1]]
    se[i,'Dur_m'] <- as.numeric(time_parts[2])
  }
  
  
  de$id = paste(de$survey,de$Tow_Number,sep="-")
  de = subset(de,select=c(id,Length,Frequency,Sex))
  
  de = merge(de,se[,c('id','Dur_m','spread','dist')])
  de$Frequency = de$Frequency*(de$Dur_m /20) #back to raw -- frequency data is already corrected for sub sampling
  de$Frequency = de$Frequency/(de$spread/1000 * de$dist)
  de = subset(de,select = c(id,Length,Frequency, Sex))
  
  sc1=seq(3,253,by=5)
  de$SZ = sc1[cut(de$Length,sc1,right=FALSE,labels=F)]
  de$UID = de$ID
  de1 = aggregate(Frequency~id+SZ,data=de,FUN=sum)
  de1$P=de1$Frequency
  bb = reshape(de1[,c('id','SZ','P')],idvar='id',timevar='SZ', direction='wide')
  bb = na.zero(bb)
  
  sp = unique(ca$Common_Name)
  lo = sp[grep('Lobster',sp)]
  ca = subset(ca, Common_Name==lo)
  ca$id = paste(ca$Survey,ca$Tow_Number,sep="-")
  
  ca = subset(ca,ca$id %in% unique(se$id),select=c(id,Year,Date,Expanded_Catch,Expanded_Weight_kg,Number_Caught,Weight_kg,Subsample_Weight_kg))
  ca = na.zero(ca)
  ca = aggregate(cbind(Expanded_Catch,Expanded_Weight_kg,Number_Caught,Weight_kg,Subsample_Weight_kg)~id,data=ca,FUN=sum)
  
  
  de$Recruit=ifelse(de$Length<82 &de$Length>=70,de$Frequency,0)
  de$Reccruit=ifelse(de$Length==82,de$Frequency/2,de$Recruit)
  
  de$Legal=ifelse(de$Length>82,de$Frequency,0)
  de$Legal=ifelse(de$Length==82,de$Frequency/2,de$Legal)
  de$sex = ifelse(de$Sex=='Female',2,ifelse(de$Sex=='Male',1,3))
  de$Legal=ifelse(de$sex==3,0,de$Legal)
  de$Berried=ifelse(de$sex==3,de$Frequency,0)
  
  lobLW1 <- function(row) {
    lobLW(CL=row[1],sex=row[2])
  }
  de$fwt =  apply(de[,c('Length','sex')],1,lobLW1)
  de$Legal_wt = de$Legal*de$fwt/1000
  
  dea = aggregate(cbind(Legal,Recruit,Legal_wt, Berried)~id,data=de,FUN=sum)
  dea = subset(dea,id %in% unique(se$id))
  dea = merge(dea,bb)
  cde = merge(ca,dea)
  scde = merge(se,cde,all.x=T)
  scde = bio.utilities::na.zero(scde)
  aa = scde
  aa$DATE = as.POSIXct(aa$Start_Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  aa$YEAR = lubridate::year(aa$DATE)
  aa$Lobster = aa$Number_Caught
  aa$EMPTY = ifelse(aa$Lobster>0,0,1)
  aa$OFFSET = aa$spread * aa$dist*1000
  aa$SOURCE = 'MNR'
  aa$OFFSET_METRIC = 'TowedDist x wing spread m2'
  aa$Gear = 'Shrimp trawl'
  aa$LATITUDE = aa$Y
  aa$LONGITUDE = aa$X
  aa = aa %>%
        select(id,DATE, YEAR,Lobster, Legal, Legal_wt, EMPTY, Berried, starts_with('P.'),OFFSET, OFFSET_METRIC,Gear, SOURCE,LONGITUDE,LATITUDE)
return(aa)
  }