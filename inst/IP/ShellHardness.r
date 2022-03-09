db.setup()
d = connect.command(con,"SELECT
    a.lfa,
    TO_CHAR(a.startdate, 'mm') month,
    a.shell,
    b.description,
    COUNT(a.shell) n
FROM
    lobster.lobster_atsea_vw   a,
    cris.crshell               b
WHERE
    a.shell = b.shell
    AND a.speciescode = 2550
GROUP BY
    a.lfa,
    TO_CHAR(a.startdate, 'mm'),
    a.shell,
    b.description")


g = subset(d,LFA=='31B')
g = g[order(g$MONTH),]
mm = unique(g$MONTH)
h = split(g,g$MONTH)
bi = data.frame(Cd =1:7, Des=c('Jelly','Breakable','Soft','Hardening','Hard','HardEpi','Moulting'))

par(mfrow=c(length(mm),1), mar = c(0.1,2,0,0.5), omi = c(0.85, 0.75, 0.75, 0.5))

for(i in 1:length(h)){
  #browser()
  hh = h[[i]]
  ss = sum(hh$N)
  hh$P = hh$N/sum(hh$N)*100
  hhh = hh[order(hh$SHELL),c('SHELL', 'P')]
  hhh= merge(bi,hhh,by.x='Cd',by.y='SHELL',all.x=T)  
  hhh$P[which(hhh$Cd %in% c(4,5,6))] <- 0
  hhh$Des <- NULL
  hhh = na.zero(hhh)
  barplot(hhh$P,space=0,yaxt='n',xaxt='n',ylim=c(0,15))
  legend('topleft',bty='n',paste('Month =',month.name[as.numeric(unique(hh$MONTH))],"; Sample Size =",ss))
  
  axis(1,lab=F,tcl=-0.6)
  if(i==1) title(paste('LFA',unique(hh$LFA),sep=" "),line=-2)
  if(i>1)axis(3,lab=F,tcl=0)
    axis(2,seq(0,15,l=5),lab=seq(0,15,l=5),las=1)
  if(i==length(h)){
    text(x=(1:nrow(bi))-.5,y=par('usr')[3]+-3.4,labels=(bi$Des),xpd=NA,srt=35,cex=1.5)
  }
}
mtext("Relative frequency (%)", 2, .5, outer = T, cex = 1.25)    

