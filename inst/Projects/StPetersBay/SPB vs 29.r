logs=lobster.db('process.logs')
logs=subset(logs, LFA=="29")
y="2012" #set index year
iy=logs$SYEAR==y
i.in=logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour
i.out=! logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour

logs.in=subset(logs, i.in)
logs.out=subset(logs, i.out)
cpue.in.all=aggregate(CPUE~SYEAR, data=logs.in, FUN ="mean")
cpue.out.all=aggregate(CPUE~SYEAR, data=logs.out, FUN ="mean")

plot(cpue.out.all, type="l", ylim=c(0,3), main="In vs Out SPB")
lines(cpue.in.all, col="red")
legend(x=2018, y=0.75, c("Out", "In"),col=c("black", "red"), lty=c(1,1), bty="n")

i.in=logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour
i.out=! logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour

test.in= subset(logs, iy & i.in)
test.out= subset(logs, iy & i.out)

cpue.in=aggregate(CPUE~WOS, data=test.in, FUN ="mean")
cpue.out=aggregate(CPUE~WOS, data=test.out, FUN ="mean")

plot(cpue.out, type="l", ylim=c(0,3), main=y)
lines(cpue.in, col="red")