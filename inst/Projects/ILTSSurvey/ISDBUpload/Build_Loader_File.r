require(readxl)
#the xls file needs to be ONLY the sheets that contain data to be loaded
ns<-length(excel_sheets(file.path("C:/Users/cooka/Downloads/L04092025.xlsx")))

fi = list()
for(i in 1:ns){
  fi[[i]]<-as.data.frame(read_excel(file.path("C:/Users/cooka/Downloads/L04092025.xlsx"),sheet=i))
}

lt1 = read.csv('~/git/bio.lobster.data/loaders/loaderTypes.csv') #this has the line types, names of fields (ie column names) and the number of chars


ISDB_loader <- function(x,lt=lt1){
  #apply this line by line
  ty = x[1,1]
  fo = subset(lt,LineType==ty)
  o = c(sprintf("%-3s",x[1,1])) #makes line type 3 chars and initiates the row
  for(i in 2:nrow(fo)){
    na = which(fo[i,'Data_Field']==names(x)) #match column names from line type structure file to the data entry...need to be exact matches
    sp = fo[i,3]
    v = x[1,na]
    if(!is.na(v) & fo$Data_Field[i]=='LatDDMM') v=signif(v,6) #only XXXX.XX for lats and lons (ie 6 sig figs)
    if(!is.na(v) & fo$Data_Field[i]=='LongDDMM') v=signif(v,6)
    if(is.na(v)) v=""
    o = paste(o,sprintf(paste("%-",sp,"s",sep=""),v),sep="") #combines columns using apprpriate number of characters
  }
  return(o)
  }

out=list()
m=0
for(i in 1:length(fi)){
  k = fi[[i]]
  for(j in 1:nrow(k)){
    m = m+1
     out[[m]] = ISDB_loader(x=k[j,])
    }
}
dat = do.call(rbind,out)
write.table(dat,'data.dat',col.names = F,row.names = F,quote = F)

