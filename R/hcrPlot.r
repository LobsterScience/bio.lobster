#' @export

hcrPlot <- function(B,mF,USR,LRP,RR=NULL,yrs,ylims=NULL,xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Fishing mortality', xlab = 'Fishable biomass',yr.ends=F,area.cols=c('lightgreen','lightgoldenrod','darksalmon'),...) {
	
	
          if(is.null(ylims)) ylims = c(0, (max(na.omit(mF),RR)*1.05))
          if(is.null(xlims)) xlims = c(0, (max(na.omit(B),USR)*1.05))
          plot( B, mF,  type="b", xlim=xlims, ylim=ylims, col="darkorange", cex=0.8, lwd=2, xlab="", ylab="", pch=20,yaxs='i',xaxs='i',... )
          title( xlab=xlab ) 
          title( ylab=ylab ) 
          polygon(x=c(USR,max(xlims)*1.3,max(xlims)*1.3, USR),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[1],border=NA)
          polygon(x=c(LRP,USR,USR, LRP),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[2],border=NA)
          polygon(x=c(-100,LRP,LRP, -100),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col=area.cols[3],border=NA)
	      lines( B, mF,  col="darkblue", cex=0.8, lwd=2, xlab="", ylab="", pch=20 ,lty=1,type='b')
          if(!is.null('RR')) {
                    arrows(x0 = USR, x1 = USR*1000, length=0,y0 = RR, lty="solid", col="black", lwd=2 )
                    text( xlims[2]*0.75, RR+RR*0.05, labels[3], lwd=2 )
           if(RRdec) arrows(x0 = LRP, x1 = USR, length=0,y1 = RR,y0=0, lty="dashed", col="black", lwd=2 );text( xlims[2]*0.75, RR+RR*0.05, labels[3], lwd=2 )
           #if(!RRdec) arrows(x0 = 0, x1 = USR*1000, length=0,y0 = RR, lty="solid", col="black", lwd=2 )
                    abline (v=USR, lty="dotted")
                    abline (v=LRP, lty="dotted")
                    text( USR-0.01*USR, RR+RR*0.05, labels[1] , srt=90, pos=3)
                    text( LRP-0.01*USR, RR+RR*0.05, labels[2] , srt=90, pos=3)
                } 
          if(is.null(RR)){                    
                    abline (v=USR, lty="dotted")
                    abline (v=LRP, lty="dotted")
                    mtext(side=3, at=USR, labels[1] , las=2, line=0.2)
                    mtext(side=3, at=LRP,  labels[2] , las=2, line=.2)
               }

          if(!yr.ends) text( B, mF,  labels=yrs, pos=3, cex= 0.8 )
          if(yr.ends) text( B[c(1,length(B))], mF[c(1,length(B))],  labels=yrs[c(1,length(B))], pos=3, cex= 0.8 )

}
