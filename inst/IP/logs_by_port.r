#logs by port MacKeracher

require(bio.lobster)
require(bio.utilities)
require(devtools)

#be sure to redo data pulls
repullData = F
if(repullData){
	lobster.db('logs.redo')
	lobster.db('process.logs.redo')
}

d = lobster.db('process.logs.unfiltered')
dRecent = subset(d,SYEAR==2020)
port = lobster.db('community_code')

dd = merge(dRecent,port,by='COMMUNITY_CODE')

dA = aggregate(LICENCE_ID~SYEAR+LFA+COMMUNITY_CODE+COMMUNITY_NAME+WOS,data=dd,FUN=function(x) length(unique(x)))

dAA = aggregate(LICENCE_ID~LFA+COMMUNITY_CODE,data=dA, FUN= function(x) c(min(x), max(x)) )
dAA$ID = ifelse(dAA$LICENCE_ID[,1]>4 & dAA$LICENCE_ID[,2]>4,1,0)

uPort = subset(dAA,ID==1,select=c(COMMUNITY_CODE,LFA))
 uPort$ID=paste(uPort$COMMUNITY_CODE, uPort$LFA,sep="-")

outData = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~COMMUNITY_CODE+COMMUNITY_NAME+WOS+LFA, data=subset(dd, paste(COMMUNITY_CODE,LFA,sep="-") %in% uPort$ID),FUN=sum)

outDataL = aggregate(LICENCE_ID~COMMUNITY_CODE+COMMUNITY_NAME+WOS+LFA, data=subset(dd, paste(COMMUNITY_CODE,LFA,sep="-") %in% uPort$ID),FUN=function(x) length(unique(x)))


dataF = merge(outData,outDataL)

write.csv(dataF,file='~/dellshared/MacKeracher/logbookData2020.csv')