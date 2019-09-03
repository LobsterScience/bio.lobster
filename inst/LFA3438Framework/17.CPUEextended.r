require(bio.lobster)
require(bio.utilities)
  
	p = bio.lobster::load.environment()
	la()
		
	assessment.year = 2018 ########### check the year ############### !!!!!!!!!!!


    p$syr = 1989
    p$yrs = p$syr:(p$current.assessment.year-1)

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA3438Framework2019")

    p$lfas = c("34", "35", "36", "38") # specify lfas for data summary
    p$subareas = c("34", "35", "36", "38") # specify lfas for data summary

    lS<-lobster.db('process.logs')
    lS = subset(lS,SYEAR<2019)
    H = lobster.db('historic.cpue')
    H$CPUE = H$LBSPTRAP/2.2046
	 lobster.db('process.vlog')
	V = vlog
    V$SYEAR = as.numeric(year(V$FDATE))
                  V$SYEAR = year(V$FDATE)
                  V$MONTH = month(V$FDATE)
                  ii = which(V$MONTH>8)
                  V$SYEAR[ii] = V$SYEAR[ii]+1 
              

	

##LFA 34

    H34 = aggregate(CPUE~LFA+SYEAR+SDATE,data=subset(H, LFA==34 & SYEAR<1960),FUN=mean)
    H34 = H34[order(H34$SDATE),]
    names(H34) = c('LFA','SYEAR','SDATE','CPUE')
    aH34 = aggregate(cbind(CPUE,SDATE)~LFA+SYEAR,data=subset(H, LFA==34 & SYEAR<1960),FUN=mean)
    
    V34 = aggregate(cbind(W_KG,N_TRP)~LFA+SYEAR+FDATE, data=subset(V,LFA==34), FUN=sum)
    V34$CPUE = V34$W_KG / V34$N_TRP
    V34$W_KG = V34$N_TRP = NULL
    names(V34) = c('LFA','SYEAR','SDATE','CPUE')

    #kludge

    V34$SYEAR[which(abs(V34$SYEAR-year(V34$SDATE))< -1)] = 1991


    
    aV34 = aggregate(cbind(W_KG,N_TRP)~LFA+SYEAR, data=subset(V,LFA==34), FUN=sum)
    aV34$CPUE = aV34$W_KG / aV34$N_TRP
    aV34$W_KG = aV34$N_TRP = NULL
    names(aV34) = c('LFA','SYEAR','CPUE')
    aaV34 = aggregate(FDATE~LFA+SYEAR, data=subset(V,LFA==34), FUN=mean)
    names(aaV34)[3] = 'SDATE'
    aV34 = merge(aV34,aaV34)

    L34 = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR+DATE_FISHED, data=subset(lS,LFA==34), FUN=sum)
    L34$CPUE = L34$WEIGHT_KG / L34$NUM_OF_TRAPS
    L34$WEIGHT_KG = L34$NUM_OF_TRAPS = NULL
    names(L34) = c('LFA','SYEAR','SDATE','CPUE')
 
    aL34 = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR, data=subset(lS,LFA==34), FUN=sum)
    aL34$CPUE = aL34$WEIGHT_KG / aL34$NUM_OF_TRAPS
    aL34$WEIGHT_KG = aL34$NUM_OF_TRAPS = NULL
    names(aL34) = c('LFA','SYEAR','CPUE')
    aaL34 = aggregate(DATE_FISHED~LFA+SYEAR, data=subset(lS,LFA==34), FUN=mean)
    names(aaL34)[3] = 'SDATE'

    aL34 = merge(aL34,aaL34)

    b34 = as.data.frame(rbind(rbind(H34,V34),L34))
    bb34 = as.data.frame(rbind(rbind(aH34,aV34),aL34))

    yy = unique(a34$SYEAR)

    bb34$SDATE = as.POSIXct(paste(bb34$SYEAR,'01','01',sep="-"))

        with(b34,plot(CPUE~SDATE,type='n',ylim=c(0,max(CPUE,na.rm=T)),col=rgb(0,0,0,0.5)))
        for(i in yy){
            with(subset(b34,SYEAR==i),lines(CPUE~SDATE, col=rgb(0,0,0,0.5)))
            }
            with(subset(bb34,SYEAR<1970),points(CPUE~SDATE, col='red',pch=16,type='b'))
            with(subset(bb34,SYEAR>1970),points(CPUE~SDATE, col='red',pch=16,type='b'))
    
   
#################
##LFA 35 not enough data to make it worth it
    
##LFA 38

    H34 = aggregate(CPUE~LFA+SYEAR+SDATE,data=subset(H, LFA==38 & SYEAR<1960),FUN=mean)
    H34 = H34[order(H34$SDATE),]
    names(H34) = c('LFA','SYEAR','SDATE','CPUE')
    aH34 = aggregate(cbind(CPUE,SDATE)~LFA+SYEAR,data=subset(H, LFA==38 & SYEAR<1960),FUN=mean)
    
    #kludge

    

    L34 = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR+DATE_FISHED, data=subset(lS,LFA==38), FUN=sum)
    L34$CPUE = L34$WEIGHT_KG / L34$NUM_OF_TRAPS
    L34$WEIGHT_KG = L34$NUM_OF_TRAPS = NULL
    names(L34) = c('LFA','SYEAR','SDATE','CPUE')
 
    aL34 = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR, data=subset(lS,LFA==38), FUN=sum)
    aL34$CPUE = aL34$WEIGHT_KG / aL34$NUM_OF_TRAPS
    aL34$WEIGHT_KG = aL34$NUM_OF_TRAPS = NULL
    names(aL34) = c('LFA','SYEAR','CPUE')
    aaL34 = aggregate(DATE_FISHED~LFA+SYEAR, data=subset(lS,LFA==38), FUN=mean)
    names(aaL34)[3] = 'SDATE'

    aL34 = merge(aL34,aaL34)

    a34 = as.data.frame(rbind(H34,L34))
    aa34 = as.data.frame(rbind(aH34,aL34))

    yy = unique(a34$SYEAR)

    aa34$SDATE = as.POSIXct(paste(aa34$SYEAR,'01','01',sep="-"))

        with(a34,plot(CPUE~SDATE,type='n',ylim=c(0,max(CPUE,na.rm=T)),col=rgb(0,0,0,0.5)))
        for(i in yy){
            with(subset(a34,SYEAR==i),lines(CPUE~SDATE, col=rgb(0,0,0,0.5)))
            }
            with(subset(aa34,SYEAR<1970),points(CPUE~SDATE, col='red',pch=16,type='b'))
            with(subset(aa34,SYEAR>1970),points(CPUE~SDATE, col='red',pch=16,type='b'))
   


###plots

  lS<-lobster.db('process.logs')
  lS = subset(lS,SYEAR<2019)
  lS$SDATE = as.POSIXct(lS$DATE_FISHED)
 ade = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR+SDATE, data=subset(lS,LFA %in% c(35,36)), FUN=sum)
 ade$CPUE = ade$WEIGHT_KG/ade$NUM_OF_TRAPS

    adem = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR, data=subset(lS,LFA %in% c(35,36)), FUN=sum)
    
    adem$CPUE = adem$WEIGHT_KG/adem$NUM_OF_TRAPS
    adem$SDATE = as.POSIXct(paste(adem$SYEAR,'01','01',sep="-"))

    xlims=c(min(b34$SDATE),max(b34$SDATE))
    lfa = c(34,35,36,38)
 
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")


 pdf(file.path( fpf1,paste0("CPUERawHistoric.pdf")),8,11)

    par(mfrow=c(length(lfa),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)

    for(j in 1:length(lfa)){
        if(lfa[j] == 34){

        with(b34,plot(CPUE~SDATE,type='n',ylim=c(0,max(CPUE,na.rm=T)),col=rgb(0,0,0,0.5),xlim=xlims))
        yy = unique(bb34$SYEAR)
        for(i in yy){
            with(subset(b34,SYEAR==i),lines(CPUE~SDATE, col=rgb(0,0,0,0.5)))
                }
            with(subset(bb34,SYEAR<1970),points(CPUE~SDATE,pch=21,bg='red',type='b'))
            with(subset(bb34,SYEAR>1970),points(CPUE~SDATE,pch=21,bg='red',type='b'))
            text(min(b34$SDATE,na.rm=T),max(b34$CPUE,na.rm=T)*.8,paste("LFA",lfa[j]),cex=2,pos=4)
            }
        if(lfa[j]== 38){
                with(a34,plot(CPUE~SDATE,type='n',ylim=c(0,max(CPUE,na.rm=T)),col=rgb(0,0,0,0.5),xlim=xlims))
                yy = unique(aa34$SYEAR)
                for(i in yy){
                    with(subset(a34,SYEAR==i),lines(CPUE~SDATE, col=rgb(0,0,0,0.5)))
                    }
                    with(subset(aa34,SYEAR<1970),points(CPUE~SDATE, pch=21,bg='red',type='b'))
                    with(subset(aa34,SYEAR>1970),points(CPUE~SDATE, pch=21,bg='red',type='b'))
        text(min(b34$SDATE,na.rm=T),max(b34$CPUE,na.rm=T)*.8,paste("LFA",lfa[j]),cex=2,pos=4)
            }
        if(lfa[j] %in% c(35, 36)){
        yy = unique(subset(ade,LFA==lfa[j])$SYEAR)
        plot(CPUE~SDATE,subset(ade,LFA==lfa[j]),type='n',ylim=c(0,max(ade$CPUE,na.rm=T)),col=rgb(0,0,0,0.5),xlim=xlims)
        for(i in yy){
        lines(CPUE~SDATE,subset(ade,LFA==lfa[j]& SYEAR==i),type='l',col=rgb(0,0,0,0.5))
                   }
        lines(CPUE~SDATE,subset(adem,LFA==lfa[j]),type='b',pch=21,bg='red')
        text(min(b34$SDATE,na.rm=T),max(b34$CPUE,na.rm=T)*.8,paste("LFA",lfa[j]),cex=2,pos=4)
    
}
}
    mtext("CPUE (kg/TH)", 2, 3, outer = T, cex = 1.5,las=0) 

dev.off()











   ###playing{
      

         a = lobster.db('annual.landings')
         a = subset(aa,YR %in% 1945:1959,select=c(YR,LFA34))

         b = subset(aa34,SYEAR %in% 1947:1959)

         plot(b$CPUE,a$LFA34)
         gg = merge(aa[,c('YR','LFA34')],aa34[,c('SYEAR','CPUE')],by.x='YR',by.y='SYEAR')

with(subset(gg,YR>1970),plot(CPUE,(LFA34-mean(LFA34))/sd(LFA34),pch=16,ylim=c(-2,2.2),xlim=c(0,2),type='b'))
with(subset(gg,YR<1970),points(CPUE,(LFA34-mean(LFA34))/sd(LFA34),pch=16,col='red',type='b'))


gge = subset(gg,YR<1970)
gge$zL = (gge$LFA34-mean(gge$LFA34))/sd(gge$LFA34)


ggl = subset(gg,YR>1970)
ggl$zL = (ggl$LFA34-mean(ggl$LFA34))/sd(ggl$LFA34)

lm(zL~CPUE+0,data=ggl)

with(gge,plot(CPUE,zL,pch=16,xlim=c(0.3,1.5)))
with(ggl,points(CPUE,zL,pch=16,xlim=c(0,2),col='red'))