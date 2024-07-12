require(readxl)
require(rio)
ns<-length(excel_sheets(file.path("C:/Users/cooka/Downloads/L02072024 2.xlsx")))

fi = list()
for(i in 1:ns){
  fi[[i]]<-import(file.path("C:/Users/cooka/Downloads/L02072024 2.xlsx"),sheet=i)
}

lt1 = read.csv('~/git/bio.lobster/data/loaderTypes.csv')


ISDB_loader <- function(x,lt=lt1){
  #apply this line by line
  ty = x[1,1]
  fo = subset(lt,LineType==ty)
  o = c(sprintf("%-3s",x[1,1]))
  for(i in 2:nrow(fo)){
    na = which(fo[i,'Data_Field']==names(x))
    sp = fo[i,3]
    v = x[1,na]
    if(!is.na(v) & fo$Data_Field[i]=='LatDDMM') v=signif(v,6)
    if(!is.na(v) & fo$Data_Field[i]=='LongDDMM') v=signif(v,6)
    if(is.na(v)) v=""
    o = paste(o,sprintf(paste("%-",sp,"s",sep=""),v),sep="")
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

