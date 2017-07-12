
#' @export
CarapaceLengthFrequencies<-function(DS="atSea", LFAs=c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34"),  bins=seq(0,220,5), Yrs=2005:2016, by=NULL, sex=1:2, fn='',GEAR='280 BALLOON',ss=F,vers=1,... ) {

    ### Carapace Length Frequencies (CLF)

        rootdir=file.path(project.datadirectory('bio.lobster'),'figures')

        #MLS
        mls<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","MinLegalSize.csv"))
        mlslfas<-as.numeric(substr(names(mls[-1]),4,5))
        if(31.1%in%LFAs) mlslfas[which(mlslfas==31)]<-c(31.1,31.2)
        mls=cbind(mls[mls$Year%in%Yrs,which(mlslfas%in%LFAs)+1])
        
  
        if(length(LFAs)==1&&LFAs=='34'){
        ## from surveys in LFA 34 only
        
            if(DS=='LobsterSurvey'){
                # Gets Lobster Survey Data
                surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=Yrs,mths=c("Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1], gear.type = GEAR)

                # Limit data to 32 selected index stations
                LS32stns<-read.csv(file.path(project.datadirectory('bio.lobster'),"data","products","survey32Stations.csv"))
                
                # Construct CLF
                LobsterSurveyCLF<-t(sapply(Yrs,function(y){colMeans(subset(surveyLobsters34,YEAR==y&SID%in%LS32stns$SID,paste0("CL",bins[-length(bins)])),na.rm=T)}))

                # plot
                BarPlotCLF(list(LobsterSurveyCLF),yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFLobSurvLFA34",fn,".pdf")),LS=mls, ...)
                return(LobsterSurveyCLF)
            }


            if(DS=='ScallopSurvey'){

                ### Scallop Survey seperate SPA 3 and SFA 29
                # SFA 29 : Aug-Oct 2000-
                # SPA 3 : Aug-Sep 1991-2003; May-Jul 2004-

                SCALSURV3.dat<-ScallopSurveyProcess(SPA="3",Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
                SCALSURV29.dat<-ScallopSurveyProcess(SPA="29",Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
                
                # Construct CLF
                ScalSurvey<-list()
                ScalSurvey$ScallopSurvey3<-t(sapply(Yrs,function(y){colMeans(subset(SCALSURV3.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
                ScalSurvey$ScallopSurvey29<-t(sapply(Yrs,function(y){colMeans(subset(SCALSURV29.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
                
               # plot

                BarPlotCLF(ScalSurvey,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFScalSurvLFA34",fn,".pdf")),LS=mls, ...)
                #BubblePlotCLF(ScalSurvey,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="ScalSurveyLFA34",prop=T)
                return(ScalSurvey)
            }
        }


        if(DS=='ScallopSurvey'){

            SCALSURV.dat<-ScallopSurveyProcess(Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
            
            # Construct CLF
            ScalSurvey<-list()
            for(i in 1:length(LFAs)){

                ScalSurvey[[i]]<-t(sapply(Yrs,function(y){colMeans(subset(SCALSURV.dat,YEAR==y&LFA==LFAs[i],paste0("CL",bins[-length(bins)])),na.rm=T)}))
              
            }
            names(ScalSurvey) <- paste("LFA",LFAs)
            
            # plot
            BarPlotCLF(ScalSurvey,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFScalSurv",fn,".pdf")),LS=mls, ...)
            #BubblePlotCLF(ScalSurvey,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="ScalSurveyLFA34",prop=T)
            return(ScalSurvey)
        }
    


        if(DS=='atSea'){
        # from At Sea Sampling

            lobster.db('atSea')
            
            # add columns for year, quarter
            atSeaData<-addSYEAR(subset(atSea,LFA%in%LFAs))
            atSeaData$YEAR<-year(atSeaData$SDATE)
            atSeaData$Q<-quarter(atSeaData$SDATE)

            # Construct CLF
            atSeaCLF<-CLF(subset(atSeaData,SYEAR%in%Yrs&SEX%in%sex,c("SYEAR","CARLENGTH",by)),yrs=Yrs,bins=bins,vers=vers)

            
            if(ss){
                ss = unlist(lapply(with(subset(atSeaData,SYEAR%in%Yrs&SEX%in%sex),tapply(paste(SYEAR,TRIPNO),SYEAR,unique)),length))
                ss = data.frame(Year=names(ss),N=ss)
                ss = merge(data.frame(Year=Yrs),ss,all=T)
                ss = ss$N
            }
            else ss = NULL

            #browser()

            # plot
            if(vers==1)BarPlotCLF(atSeaCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFSeaSampling",fn,".pdf")),LS=mls, sample.size=ss,...)
            if(vers==2)BarPlotCLF2(atSeaCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFSeaSampling",fn,".pdf")),LS= as.vector(mls), sample.size=ss,...)
            #BubblePlotCLF(atSeaCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,bins=bins,filen="SeaSamplingLFA34",yrs=Yrs)
            return(atSeaCLF)
        }
        

        if(DS=='port'){
        # from Port Sampling

            PS<-PortSamplesProcess(lfa=LFAs)

            seX<-c("M","F","B")[sex]
            portCLF<-CLF(subset(PS$portlengths,SEX%in%seX,c("SYEAR","LENGTH",by)),yrs=Yrs,bins=bins)
            BarPlotCLF(portCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFPortSampling",fn,".pdf")),LS=mls,...)
            #BubblePlotCLF(portCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),bins=bins,prop=T,filen="PortSamplingLFA34",yrs=Yrs,LS=82.5)
            return(portCLF)
        }
        

        if(DS=='fsrs'){

            lobster.db("fsrs")

            fsrs$SYEAR<-fsrs$HAUL_YEAR
            fsrs$SYEAR[fsrs$LFA%in%c("33","34","35")]<-as.numeric(substr(fsrs$S_LABEL[fsrs$LFA%in%c("33","34","35")],6,9))

            fsrs$HAUL_DATE<-as.Date(fsrs$HAUL_DATE)

            # this section is to deal with the fact that there are uneven binning going on for the different size categories
            # it creates a pseudo CL (the mid point of each size category)
            scd<-read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","FSRS_SIZE_CODES.csv"))
            scd$LENGTH<-rowMeans(scd[c("MIN_S","MAX_S")])
            LFdat<-merge(fsrs,scd[c("SIZE_CD","LENGTH")])

            # Construct CLF
            fsrsCLF<-CLF(subset(LFdat,SYEAR%in%Yrs&SEX%in%sex&LFA%in%LFAs,c("SYEAR","LENGTH",by)),yrs=Yrs,bins=bins)
            # plot
            BarPlotCLF(fsrsCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFfsrs",fn,".pdf")),rel=T,LS=mls,wd=9,...)
            return(fsrsCLF)
        }


        if(DS=='groundfish'){

        }


}

