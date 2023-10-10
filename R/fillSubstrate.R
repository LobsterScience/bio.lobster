fillSubstrate <- function(x) {
  x = x[order(x$interval),] #Sort transects by interval
  s = data.frame(NewInt = seq(0,unique(x$transect_length),by=5))  #Create new 5m intervals to group  
  i = unique(x$diver) #Index by diver
  o = list()
  for(j in 1:length(i)){
    x2 = subset(x,diver==i[j]) 
    x2i = subset(x2,select = c(location, date, transect_id, transect_no, transect_width, transect_length,
                               start_lat, start_long, end_lat, end_long, diver, max_depth, avg_depth, time_in, time_out, bottom_temp))[1,] #The columns that stay the same throughout transect
    
    x2s = subset(x2,select=c(interval, substrate_1, substrate_2, lobster, carapace, sex, stage, clutch, shell_condition, comment, dates, month, year, tid))
    x2s$NewInt = s$NewInt[cut(x2s$interval,breaks=s$NewInt,labels=F,include.lowest = T)]
    s = subset(s,NewInt<unique(x$transect_length))
    x2sp = merge(x2s,s,all.y=T)
    repeat{
      id = which(is.na(x2sp$substrate_1))
      x2sp$substrate_1[id] = x2sp$substrate_1[id-1]
      id2 = which(is.na(x2sp$substrate_1))
      if(length(id2)==0) break
    }
    repeat{
      id = which(is.na(x2sp$substrate_2))
      x2sp$substrate_2[id] = x2sp$substrate_2[id-1]
      id2 = which(is.na(x2sp$substrate_2))
      if(length(id2)==0) break
    }
    x2sp
    inf = as.data.frame(sapply(x2i,rep.int,nrow(x2sp)))
    o[[j]] = as.data.frame(cbind(inf,x2sp))
  }
  return( data.frame(do.call(rbind,o)))
 
  
  
}