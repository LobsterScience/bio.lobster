hcrPlot <- function(B,mF,USR,LRP,RR=NULL,TRP=NULL, yrs,ylims=NULL,xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Fishing mortality', xlab = 'Fishable biomass',yr.ends=F,big.final=F, area.cols=c('lightgreen','lightgoldenrod','darksalmon'),French=F,FrenchCPUE=F,example=F,RRdecTRP=F,noTicks=F,...) {
  
  if (French) {
    labels=c("PRS","PRL","Taux d'exploitation de reference")    
    ylab='Exploitation'
    xlab='Biomasse'
  }
  if (FrenchCPUE) {
    labels=c("PRS","PRL","Taux d'exploitation de reference")  
    ylab='Exploitation'
    xlab='CPUE'
  }	
  
  if(is.null(ylims)) ylims = c(0, (max(na.omit(mF),RR)*1.05))
  if(is.null(xlims)) xlims = c(0, (max(na.omit(B),USR)*1.05))
  if(noTicks==F){  plot( B, mF,  type="b", xlim=xlims, ylim=ylims, col="darkorange", cex=0.8, lwd=2, xlab="", ylab="", pch=20,yaxs='i',xaxs='i',... ); box()}
  if(noTicks==T) {plot( B, mF,  type="b", xlim=xlims, ylim=ylims, col="darkorange", cex=0.8, lwd=2, xlab="", ylab="", pch=20,yaxs='i',axes=F,... ); axis(side=1,labels=NA);axis(side=2,labels=NA);box()}
  
  title( xlab=xlab ) 
  title( ylab=ylab ) 
  if(is.null(TRP) ) {
    polygon(x=c(USR,max(xlims)*1.3,max(xlims)*1.3, USR),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[1],border=NA)
    polygon(x=c(LRP,USR,USR, LRP),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[2],border=NA)
    polygon(x=c(-100,LRP,LRP, -100),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[3],border=NA)
  }
  if(!is.null(TRP) ) {
    polygon(x=c(TRP,max(xlims)*1.3,max(xlims)*1.3, TRP),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[1],border=NA)
    polygon(x=c(USR,TRP,TRP, USR),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[2],border=NA)
    polygon(x=c(LRP,USR,USR, LRP),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[3],border=NA)
    polygon(x=c(-100,LRP,LRP, -100),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[4],border=NA)
  }
  
  
  
  if(!is.null('RR')) {
    arrows(x0 = USR, x1 = USR*1000, length=0,y0 = RR, lty="solid", col="black", lwd=2 )
    text( xlims[2]*0.75, RR+RR*0.04, labels[3], lwd=2 )
    if(RRdec) arrows(x0 = LRP, x1 = USR, length=0,y1 = RR,y0=0, lty="dashed", col="black", lwd=2 )
    abline (v=USR, lty="dotted")
    abline (v=LRP, lty="dotted")
    
    text( USR-0.01*USR, RR+RR*0.1, labels[1] , srt=90, pos=3)
    text( LRP-0.01*USR, RR+RR*0.1, labels[2] , srt=90, pos=3)
    if(!is.null(TRP)){text( TRP-0.01*TRP, RR+RR*0.1, labels[4] , srt=90, pos=3);abline (v=TRP, lty="dotted")
      if(RRdecTRP) arrows(x0 = LRP, x1 = TRP, length=0,y1 = RR,y0=0, lty="dashed", col="black", lwd=2 )
    }
  } 
  if(is.null(RR)){                    
    abline (v=USR, lty="dotted")
    abline (v=LRP, lty="dotted")
    mtext(side=3, at=USR, labels[1] , las=2, line=0.2)
    mtext(side=3, at=LRP,  labels[2] , las=2, line=.2)
    
  }
  
  if(example){
    lines( B, mF, cex=0.8, lwd=2, xlab="", ylab="", pch=20 ,lty=1,type='n')
  }
  if(!example){
    lines( B, mF,  col="darkblue", cex=0.8, lwd=2, xlab="", ylab="", pch=20 ,lty=1,type='b')
    if(!yr.ends) text( B, mF,  labels=yrs, pos=3, cex= 0.8 )
    if(big.final) points( B[length(B)], mF[length(B)],  pch=18, col="blue",  cex= 1.7)
    if(yr.ends) text( B[c(1,length(B))], mF[c(1,length(B))],  labels=yrs[c(1,length(B))], pos=3, cex= 0.8 )
  }
}