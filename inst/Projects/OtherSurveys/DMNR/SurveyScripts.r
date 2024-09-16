
##DMR data
require(bio.lobster)
require(devtools)
require(bio.utilities)
require(sf)


fd = file.path(project.datadirectory('bio.lobster'),'data','MaineDMRSurvey')
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

se = subset(se,Season=='Fall')
se$id = paste(se$Survey,se$Tow_Number,sep="-")
se$X = (se$Start_Longitude+se$End_Longitude)/2
se$Y = (se$Start_Latitude+se$End_Latitude)/2
se$z = ((se$Start_Depth_fathoms+se$End_Depth_fathoms)/2)*1.8288
se$dist = se$Tow_LengthNM * 1.852
se = subset(se,Region==5 & id != 'FL15-73',select=c(id,Start_Date,X,Y,z,dist,Region,Depth_Stratum,Tow_Time))
se$spread = 57*0.3048 #57 ft rope length

se$Dur_m=NA

# Split the time string into minutes and seconds
for(i in 1:nrow(se)){
time_parts <- strsplit(se[i,'Tow_Time'], ":")[[1]]
se[i,'Dur_m'] <- as.numeric(time_parts[2])
}


de = subset(de,Season=='Fall')
de$id = paste(de$survey,de$Tow_Number,sep="-")
de = subset(de,select=c(id,Length,Frequency,Sex))

sp = unique(ca$Common_Name)
lo = sp[grep('Lobster',sp)]
ca = subset(ca,Season=='Fall' & Common_Name==lo)
ca$id = paste(ca$Survey,ca$Tow_Number,sep="-")
ca = subset(ca,ca$id %in% unique(se$id),select=c(id,Year,Date,Expanded_Catch,Expanded_Weight_kg,Number_Caught,Weight_kg,Subsample_Weight_kg))
ca = na.zero(ca)
ca = aggregate(cbind(Expanded_Catch,Expanded_Weight_kg,Number_Caught,Weight_kg,Subsample_Weight_kg)~id,data=ca,FUN=sum)


de$Rec=ifelse(de$Length<82 &de$Length>=70,de$Frequency,0)
de$Rec=ifelse(de$Length==82,de$Frequency/2,de$Rec)

de$Comm=ifelse(de$Length>82,de$Frequency,0)
de$Comm=ifelse(de$Length==82,de$Frequency/2,de$Comm)
de$sex = ifelse(de$Sex=='Female',2,ifelse(de$Sex=='Male',1,3))

lobLW1 <- function(row) {
  lobLW(CL=row[1],sex=row[2])
}
de$fwt =  apply(de[,c('Length','sex')],1,lobLW1)

de$Commwt = de$Comm*de$fwt

dea = aggregate(cbind(Comm,Rec,Commwt)~id,data=de,FUN=sum)
dea = subset(dea,id %in% unique(se$id))

cde = merge(ca,dea)
scde = merge(se,cde,all.x=T)

aa = st_as_sf(scde,coords = c('X','Y'),crs=4326)
aa$Comm = aa$Comm*aa$Dur_m/20
aa$Rec = aa$Rec*aa$Dur_m/20
aa$Commwt = aa$Commwt*aa$Dur_m/20

aa$CommwtST = aa$Commwt/(aa$spread/1000 * aa$dist)/1000 #kg per km2

