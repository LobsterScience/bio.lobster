#' @export
compareCLF<-function(CLF,labels=c("A","B"),bins=seq(0,220,5),filen='compareCLF.pdf',rows=length(CLF),graphic='pdf',xl,rel=T,ylp=0.9,ymax,LS=82.5,sample.size=NULL,recline=NULL,wd=7,ht=8,bx=T,xlab="Carapace Length (mm)",ylab="Mean N / standard tow",title=NULL,...){
    
    mids<-bins[-1]-diff(bins)/2
    
    if(!missing(ymax)&&length(ymax)==1)ymax<-rep(ymax,length(CLF))

    if(graphic=="R") x11(width = wd, height = ht)
    if(graphic=="pdf") pdf(filen, width = wd, height = ht)
    par(mfcol=c(rows,1), mar = c(0,2,0,0.5), omi = c(0.85, 0.75, 0.75, 0.5))
    
    for(i in 1:length(CLF)){
    #browser()
    
    if(missing(xl))xlm<-c(1,length(bins))
    else if(!missing(xl))xlm<-xl
    
        yl2<-ifelse(missing(ymax),max(CLF[[i]])*1.1,ymax[i])
        
        barplot(CLF[[i]],space=0,xlim=xlm,yaxt='n',xaxt='n',ylim=c(0,yl2),...)

        axis(1,1:length(bins),lab=F,tcl=-0.3)
        axis(1,at=seq(xlm[1],xlm[2],4),lab=F,tcl=-0.6)
        if(i>1)axis(3,lab=F,tcl=0)
        if(rel==F){
            axis(2, pretty(c(0,yl2/1.3)),las=1)
            axis(4, pretty(c(0,yl2/1.3)),lab=F)
        }
        if(rel==T){
            axis(2,seq(0,max(CLF[[i]]),l=6),lab=seq(0,100,20),las=1)
            axis(4,seq(0,max(CLF[[i]]),l=6),lab=F,las=1)
        }
        if(i==length(CLF)){
            axis(1,at=seq(xlm[1],xlm[2],4),lab=seq(min(bins),max(bins),20))
        }
        
        if(!is.null(LS))abline(v=(LS[i]-min(bins))/diff(bins)[1],lwd=2,col='red')
        if(!is.null(recline))abline(v=recline,lty=2,col='red')
        
        mtext(as.character(labels[i]), 3, -3, adj=ylp,outer = F,cex=1.25)
        if(!is.null(sample.size))mtext(paste("N =",sample.size[i]), 3, -3, adj=0.9,outer = F,cex=1)
     if(bx)box()
    }
    mtext(title, 3, 1, outer = T, cex = 1.75)    
    mtext(xlab, 1, 3, outer = T, cex = 1.25)    
    if(rel==F)mtext(ylab, 2, 2, outer = T, cex = 1.25)    
    if(rel==T)mtext("Relative frequency (%)", 2, 2, outer = T, cex = 1.25)    

    
    if(graphic!="R")dev.off()    
}

	