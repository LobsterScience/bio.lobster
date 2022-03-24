p$current.assessment.year = p$current.assessment.year - 1 

figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA27-32",p$current.assessment.year))
cpue.dir=file.path(figdir, "cpue")

logs=lobster.db('process.logs')
logs=subset(logs, LFA=="29")
y="2021" #set index year
iy=logs$SYEAR==y
i.in=logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour
i.out=! logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour

logs.in=subset(logs, i.in)
logs.out=subset(logs, i.out)
cpue.in.all=aggregate(CPUE~SYEAR, data=logs.in, FUN ="mean")
cpue.out.all=aggregate(CPUE~SYEAR, data=logs.out, FUN ="mean")

png(filename=file.path(cpue.dir, "LFA 29 in and out.png"),width=8, height=5.5, units = "in", res = 800)
plot(cpue.out.all, type="l", ylim=c(0,3), main="", xlab="Year", ylab="Catch Rate (kg/trap)")
lines(cpue.in.all, col="red")
legend(x=2005, y=0.45, c("St Peters, River Bourgeois, Little Harbour", "Other LFA 29 Ports"),col=c("red", "black"), lty=c(1,1), bty="n")
dev.off()


i.in=logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour
i.out=! logs$COMMUNITY_CODE %in% c("10928", "10810", "10925") #chooses vessels from St Peters, River Bourgeois and Little Harbour

test.in= subset(logs, iy & i.in)
test.out= subset(logs, iy & i.out)

cpue.in=aggregate(CPUE~WOS, data=test.in, FUN ="mean")
cpue.out=aggregate(CPUE~WOS, data=test.out, FUN ="mean")

png(filename=file.path(cpue.dir, "LFA 29 Weekly.png"),width=8, height=5.5, units = "in", res = 800)
plot(cpue.out, type="l", ylim=c(0,3), main=paste0(y), xlab="Week of Season", ylab="Catch Rate (kg/trap)")
abline(h=mean(cpue.out$CPUE), lty=3)
lines(cpue.in, col="red")
abline(h=mean(cpue.in$CPUE),col="red", lty=3, lwd=.5)
legend(x=2, y=0.5, c("St Peters, River Bourgeois, Little Harbour", "Other LFA 29 Ports"),col=c("red", "black"), lty=c(1,1), bty="n")
dev.off()

