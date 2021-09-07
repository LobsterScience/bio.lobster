p = bio.lobster::load.environment()
la()


logs=lobster.db("process.logs")

lfa29=logs[logs$LFA=="29",]
lfa29=lfa29[lfa29$SYEAR>2006,] #weird data previous to 2007
start=lfa29[lfa29$WOS=="1",]


w1=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_LBS)~SYEAR,data=start,FUN="sum")
w1$W1.CPUE=w1$WEIGHT_LBS / w1$NUM_OF_TRAPS
w1=w1[,c(1, 4 )]

land = lobster.db('annual.landings')
land =land[order(land$YR),]
land=land[, c("YR", "LFA29")]
names(land)=c("SYEAR", "Annual.Landings")

comp=merge(w1, land, sort=F)

diff.table.cpue=diff(comp$W1.CPUE)/comp[-nrow(comp),] * 100
diff.table.cpue=diff.table.cpue[,c(1,2)]
diff.table.cpue$SYEAR=c(2008:2020)

diff.table.landings=diff(comp$Annual.Landings)/comp[-nrow(comp),] *100
diff.table.landings=diff.table.landings[,c(1,3)]
diff.table.landings$SYEAR=c(2008:2020)

diff.table=merge(diff.table.cpue, diff.table.landings)

plot(x=diff.table$W1.CPUE, y=diff.table$Annual.Landings, ylab="Change in Annual Landings", xlab="Change in First Week Catch Rate", pch=19,
     main='LFA 29- % Change from Previous Year')

abline(h=0, lty='dotted')
abline(v=0, lty='dotted')
 
plot(y=diff.table$W1.CPUE, x=diff.table$SYEAR, xlab="Year", ylab="Change in First Week Catch Rate", pch=19,
     main='LFA 29- % Change from Previous Year')

abline(h=0, lty='dotted')


  

