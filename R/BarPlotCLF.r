#' @export
BarPlotCLF<-function(CLF,yrs=2005:2016,CLFyrs=yrs,bins=seq(0,220,5),filen='CLF.pdf',rows=length(yrs),pdf=T,xl,rel=T,mean.line=F,ylp=0.1,ymax,LS=82.5,sample.size=NULL,recline=NULL,wd=10,ht=12,bx=F,xlab="Carapace Length (mm)",ylab="Mean N / standard tow",...){
    
    mids<-bins[-1]-diff(bins)/2
    
    if(!missing(ymax)&&length(ymax)==1)ymax<-rep(ymax,length(CLF))
    if(!is.null(LS)){
        if(is.null(ncol(LS)))LS=cbind(rep(LS,length(yrs)))
        if(ncol(LS)==1)LS=matrix(LS,length(yrs),length(CLF))
    }

    if(pdf) pdf(filen, width = wd, height = ht)
    par(mfcol=c(rows,ceiling(length(yrs)/rows)), mar = c(0,2,0,0.5), omi = c(0.85, 0.75, 0.75, 0.5))
    
    for(i in 1:length(CLF)){
    #browser()
    
        if(missing(xl))xlm<-range(bins)
        else if(!missing(xl))xlm<-xl
        
        for(y in which(yrs==CLFyrs)){
            tmp<-hist(mids,plot=F,breaks=bins)
            tmp$counts<-CLF[[i]][y,] 
            yl2<-ifelse(missing(ymax),max(tmp$counts)*1.2,ymax[i])
            #browser()
            plot(tmp,main="",xlab="",ylab="",xlim=xlm,yaxt='n',xaxt='n',ylim=c(0,yl2),...)
            axis(1,bins,lab=F,tcl=-0.3)
            axis(1,lab=F,tcl=-0.6)
            if(y>1)axis(3,lab=F,tcl=0)
            if(rel==F){
                axis(2, pretty(c(0,yl2/1.3)),las=1)
                axis(4, pretty(c(0,yl2/1.3)),lab=F)
            }
            if(rel==T){
                axis(2,seq(0,max(tmp$counts),l=6),lab=seq(0,100,20),las=1)
                axis(4,seq(0,max(tmp$counts),l=6),lab=F,las=1)
            }
            if(y==max(which(yrs==CLFyrs))){
                axis(1)
                if(mean.line){
                    lines(tmp$mids,colMeans(CLF[[i]][-nrow(CLF[[i]]),]),lwd=1)
                    legend(xlm[2]*0.8,yl2/1.3,paste(yrs[1],'-',yrs[length(yrs)-1]),bty='n',lty=1)
                }
            }
            if(!is.null(LS))abline(v=LS[y,i],lwd=2,col='red')
            if(!is.null(recline))abline(v=recline,lty=2,col='red')
            
            mtext(as.character(CLFyrs[y]), 3, -3, adj=ylp,outer = F,cex=1)
            if(!is.null(sample.size))mtext(paste("N =",sample.size[y]), 3, -3, adj=0.9,outer = F,cex=1)
        }
        mtext(names(CLF)[i], 3, 3, outer = T, cex = 1.5)    
        mtext(xlab, 1, 3, outer = T, cex = 1.5)    
        if(rel==F)mtext(ylab, 2, 2, outer = T, cex = 1.5)    
        if(rel==T)mtext("Relative frequency (%)", 2, 2, outer = T, cex = 1.25)    
        if(bx)box()

    }
if(pdf)dev.off()    
}

