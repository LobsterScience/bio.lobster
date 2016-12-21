#' @export

hcrPlot <- function(B,mF,USR,LRP,RR,yrs,ylims=NULL,xlims=NULL) {
	
	
          if(is.null(ylims)) ylims = c(0, (max(mF,RR)*1.05))
          if(is.null(xlims)) xlims = c(0, (max(B,USR)*1.05))
          plot( B, mF,  type="b", xlim=xlims, ylim=ylims, col="darkorange", cex=0.8, lwd=2, xlab="", ylab="", pch=20,yaxs='i',xaxs='i' )
          title( xlab="Fishable biomass (t)" ) 
          title( ylab="Fishing mortality" ) 
          polygon(x=c(USR,max(xlims)*1.3,max(xlims)*1.3, USR),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col='lightgreen',border=NA)
          polygon(x=c(LRP,USR,USR, LRP),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col='lightgoldenrod',border=NA)
          polygon(x=c(-100,LRP,LRP, -100),y=c(-0.1,-0.1,max(ylims)*1.3,max(ylims)*1.3),col='darksalmon',border=NA)
	      lines( B, mF,  col="darkblue", cex=0.8, lwd=2, xlab="", ylab="", pch=20 ,lty=1)
          abline (h=RR, lty="solid", col="gray", lwd=2 )
          abline (v=USR, lty="dotted")
          abline (v=LRP, lty="dotted")
          text( USR-0.01*USR, RR+RR*0.1, "USR" , srt=90, pos=3)
          text( LRP-0.01*USR, RR+RR*0.1, "LRP" , srt=90, pos=3)
          text( USR+USR*0.2, RR+RR*0.1, "RR", lwd=2 )
          text( B, mF,  labels=yrs, pos=3, cex= 0.8 )

}