# constructs a Length Frequecy matrix

CLF<-function(LFdat,bins=seq(0,220,5),yrs,ID="CLF"){

    require("lubridate")
    names(LFdat)[1:2]<-c('YEAR','LENGTH')
    if(missing(yrs))yrs<-sort(unique(LFdat$YEAR))


    # additional columns may be included to produce seperate CLFs
    LFdat<-na.omit(LFdat)
    nc<-ncol(LFdat)
    LFdat$ID<-ID
    if(nc>2){
        LFdat$ID<-paste0(names(LFdat)[3],LFdat[,3])
        if(nc>3){
            for(i in 4:nc) LFdat$ID<-paste0(LFdat$ID,paste0(names(LFdat)[i],LFdat[,i]))    
        }
    }        

    CLF<-list()
    IDs<-sort(unique(LFdat$ID))
    for(i in 1:length(IDs)){
        CLF[[i]]<-t(sapply(yrs,function(y){with(subset(LFdat,YEAR==y&ID==IDs[i]&LENGTH>=min(bins)&LENGTH<max(bins)),hist(LENGTH,breaks=bins,plot=F)$count)}))
    }
    names(CLF)<-IDs

    return(CLF)
}


