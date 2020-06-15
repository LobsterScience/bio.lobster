#median lengths of bycatch

data = read.csv('~/tmp/bycatchFishLengths/LFA\ 27-33\ fish_lengths.csv')

spp = unique(data$SPECCD_ID)

for(i in 1:length(spp)){
		x11()
		boxplot(FISH_LENGTH~LFA,data=subset(data,SPECCD_ID==spp[i]))
		title(spp[i])
}

agdata = aggregate(FISH_LENGTH~SPECCD_ID+LFA,data=data, FUN=function(x) c(med = median(x),n = length(x)))
agdata = do.call(data.frame,agdata)
agdata = subset(agdata,FISH_LENGTH.n>30)
names(agdata) = c('SPECCD_ID','LFA','MedianLength','SampleSize')
agdata$LFAspecificSizes=T
agdata$id = paste(agdata$SPECCD_ID,agdata$LFA,sep='--')

agdata2 = aggregate(FISH_LENGTH~SPECCD_ID,data=data, FUN=function(x) c(med = median(x),n = length(x)))
agdata2 = do.call(data.frame,agdata2)
ll = unique(data$LFA)
ag = rep(ll,each=nrow(agdata2))
ag = cbind(do.call(rbind,replicate(agdata2,n=length(ll),simplify=FALSE)),ag)
names(ag) = c('SPECCD_ID','MedianLength','SampleSize','LFA')
ag$id = paste(ag$SPECCD_ID,ag$LFA,sep='--')

ag = subset(ag,!id %in% agdata$id )
ag$LFAspecificSizes=F


data = as.data.frame(rbind(ag,agdata))
data$id = NULL
 write.csv(data,'~/tmp/bycatchFishLengths/LFA\ 27-33\ median_fish_lengths.csv',row.names=F)

