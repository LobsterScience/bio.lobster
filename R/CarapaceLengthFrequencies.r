
#' @export
CarapaceLengthFrequencies=function(DS="atSea", LFAs=c("27", "28", "29", "30", "31A", "31B", "32", "33", "34"),  bins=seq(0,200,5), Yrs=2005:2016, by=NULL, sex=1:3, fn='',gear.type=NULL,Net=NULL,comparative=F,species=2550,index.stations=F,ss=T,vers=1, min.size=0,rootdir=file.path(project.datadirectory('bio.lobster'),'figures'),est.den=F,... ) {

    ### Carapace Length Frequencies (CLF)


        #MLS
        mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('MLS.Changes.all.LFA',year(Sys.time())-1,'.csv')))       
        mls.lst=list()
        for(i in 1:length(LFAs)){
          mls.lst[[i]]=  mls$MLS_MM[mls$Year%in%Yrs&mls$LFA%in%LFAs]
        }
        mls  = do.call("cbind",mls.lst)
      
        
  
        if(length(LFAs)==1&&LFAs=='34'){
        ## from surveys in LFA 34 only
        
            if(DS=='LobsterSurvey'){
                if(vers==1){

                    # Gets Lobster Survey Data
                    surveyLobsters34=LobsterSurveyProcess(lfa="34",yrs=1996:year(Sys.time()),mths=c("Aug","Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1], sex=sex, gear.type = gear.type, Net = Net,comparative=comparative,species = species)
                    #browser()
                    if(index.stations)  surveyLobsters34 = calcIndexStations(surveyLobsters34,n=16, map=F)
                
                    # Construct CLF
                    LobsterSurveyCLF=t(sapply(Yrs,function(y){colMeans(subset(surveyLobsters34,YEAR==y,paste0("CL",bins[-1])),na.rm=T)}))
                    # plot
                    BarPlotCLF(list(LobsterSurveyCLF),yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFLobSurvLFA34",fn)),LS=mls, ...)
                }
                if(vers==2){
                    # Construct CLF
                    # Gets Lobster Survey Data
                    surveyLobstersM=LobsterSurveyProcess(lfa="34",yrs=1996:year(Sys.time()),mths=c("Aug","Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1], sex=1, gear.type = gear.type, Net = Net,comparative=comparative,species = species)
                    surveyLobstersM$SEX=1
                    surveyLobstersF=LobsterSurveyProcess(lfa="34",yrs=1996:year(Sys.time()),mths=c("Aug","Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1], sex=2, gear.type = gear.type, Net = Net,comparative=comparative,species = species)
                    surveyLobstersF$SEX=2
                    surveyLobstersB=LobsterSurveyProcess(lfa="34",yrs=1996:year(Sys.time()),mths=c("Aug","Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1], sex=3, gear.type = gear.type, Net = Net,comparative=comparative,species = species)
                    surveyLobstersB$SEX=3
                    surveyLobsters34 = rbind(surveyLobstersM,surveyLobstersF,surveyLobstersB)
                    LobsterSurveyCLF=list()
                    for(i in 1:length(Yrs)){
                        LobsterSurveyCLF[[i]]=t(sapply(sex,function(y){colMeans(subset(surveyLobsters34,YEAR==Yrs[i]&SEX==y,paste0("CL",bins[-1])),na.rm=T)}))
                    }
                    #browser()
                    BarPlotCLF2(LobsterSurveyCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFLobSurvLFA34",fn)),LS= as.vector(mls),...)
                }
                return(LobsterSurveyCLF)
            }


            if(DS=='ScallopSurvey'){

                ### Scallop Survey seperate SPA 3 and SFA 29
                # SFA 29 : Aug-Oct 2000-
                # SPA 3 : Aug-Sep 1991-2003; May-Jul 2004-

                SCALSURV3.dat=ScallopSurveyProcess(SPA="3",Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
                SCALSURV29.dat=ScallopSurveyProcess(SPA="29",Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
                
                # Construct CLF
                ScalSurvey=list()
                ScalSurvey$ScallopSurvey3=t(sapply(Yrs,function(y){colMeans(subset(SCALSURV3.dat,YEAR==y,paste0("CL",bins[-1])),na.rm=T)}))
                ScalSurvey$ScallopSurvey29=t(sapply(Yrs,function(y){colMeans(subset(SCALSURV29.dat,YEAR==y,paste0("CL",bins[-1])),na.rm=T)}))
                
               # plot

                BarPlotCLF(ScalSurvey,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFScalSurvLFA34",fn)),LS=mls, ...)
                #BubblePlotCLF(ScalSurvey,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="ScalSurveyLFA34",prop=T)
                return(ScalSurvey)
            }
        }


        if(DS=='ScallopSurvey'){

            SCALSURV.dat=ScallopSurveyProcess(Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
            
            # Construct CLF
            ScalSurvey=list()
            for(i in 1:length(LFAs)){

                ScalSurvey[[i]]=t(sapply(Yrs,function(y){colMeans(subset(SCALSURV.dat,YEAR==y&LFA==LFAs[i],paste0("CL",bins[-1])),na.rm=T)}))
              
            }
            names(ScalSurvey) = paste("LFA",LFAs)
            
            # plot
            BarPlotCLF(ScalSurvey,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFScalSurv",fn)),LS=mls, ...)
            #BubblePlotCLF(ScalSurvey,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="ScalSurveyLFA34",prop=T)
            return(ScalSurvey)
        }
    


        if(DS=='atSea'){
        # from At Sea Sampling
      
            lobster.db('atSea')
         
            # add columns for year, quarter
            atSeaData=addSYEAR(subset(atSea,LFA%in%LFAs))
            atSeaData$YEAR=year(atSeaData$STARTDATE)
            atSeaData$Q=quarter(atSeaData$STARTDATE)

            # Construct CLF
            atSeaCLF=CLF(subset(atSeaData,SYEAR%in%Yrs&SEX%in%sex&CARLENGTH>min.size,c("SYEAR","CARLENGTH",by)),yrs=Yrs,bins=bins,vers=vers,est.den=est.den)

           
            if(!is.null(ss)){
              
                ss = unlist(lapply(with(subset(atSeaData,SYEAR%in%Yrs&SEX%in%sex),tapply(paste(SYEAR,TRIPNO),SYEAR,unique)),length))
                ss = data.frame(Year=names(ss),N=ss)
                ss = merge(data.frame(Year=Yrs),ss,all=T)
                ss = ss$N
            }
        

            # plot
            if(vers==1)BarPlotCLF(atSeaCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFSeaSampling",fn)),LS=mls, sample.size=ss,...)
            if(vers==2)BarPlotCLF2(atSeaCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFSeaSampling",fn)),LS= as.vector(mls), sample.size=ss,...)
            #BubblePlotCLF(atSeaCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,bins=bins,filen="SeaSamplingLFA34",yrs=Yrs)
            return(atSeaCLF)
        }
        

        if(DS=='port'){
        # from Port Sampling

            PS=PortSamplesProcess(lfa=LFAs)

            PS$portlengths$SEX[PS$portlengths$SEX=="M"]=1
            PS$portlengths$SEX[PS$portlengths$SEX=="F"]=2
            PS$portlengths$SEX[PS$portlengths$SEX=="B"]=3

            if(!is.null(ss)){
                 
               ss = unlist(lapply(with(subset(PS$portlengths,SYEAR%in%Yrs&SEX%in%sex),tapply(paste(SYEAR,SAMPLE_ID),SYEAR,unique)),length))
               ss = data.frame(Year=names(ss),N=ss)
               ss = merge(data.frame(Year=Yrs),ss,all=T)
               ss = ss$N
            }

            portCLF=CLF(subset(PS$portlengths,SEX%in%sex,c("SYEAR","LENGTH",by)),yrs=Yrs,bins=bins,vers=vers,est.den=est.den)
            if(vers==1)BarPlotCLF(portCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFSeaSampling",fn)),LS=mls, sample.size=ss,...)
            if(vers==2)BarPlotCLF2(portCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFSeaSampling",fn)),LS= as.vector(mls), sample.size=ss,...)
            #BarPlotCLF(portCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFPortSampling",fn)),LS=mls,...)
            #BubblePlotCLF(portCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),bins=bins,prop=T,filen="PortSamplingLFA34",yrs=Yrs,LS=82.5)
            return(portCLF)
        }
        

        if(DS=='fsrs'){

            lobster.db("fsrs")

            fsrs$HAUL_DATE=as.Date(fsrs$HAUL_DATE)
            fsrs$SYEAR=fsrs$HAUL_YEAR
            fsrs$SYEAR[fsrs$LFA%in%c("33","34")]=as.numeric(substr(fsrs$S_LABEL[fsrs$LFA%in%c("33","34")],6,9))
            fsrs$SYEAR[fsrs$LFA%in%c("35")&month(fsrs$HAUL_DATE)>8]=fsrs$SYEAR[fsrs$LFA%in%c("35")&month(fsrs$HAUL_DATE)>8]+1

            # this section is to deal with the fact that there are uneven binning going on for the different size categories
            # it creates a pseudo CL (the mid point of each size category)
            scd=read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","FSRS_SIZE_CODES.csv"))
            scd$LENGTH=rowMeans(scd[c("MIN_S","MAX_S")])
            LFdat=merge(fsrs,scd[c("SIZE_CD","LENGTH")])

            # Construct CLF
            fsrsCLF=CLF(subset(LFdat,SYEAR%in%Yrs&SEX%in%sex&LFA%in%LFAs,c("SYEAR","LENGTH",by)),yrs=Yrs,bins=bins,vers=vers)
            # plot
            if(vers==1)BarPlotCLF(fsrsCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFfsrs",fn)),rel=T,LS=mls,wd=9,...)
            if(vers==2)BarPlotCLF2(fsrsCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("CLFfsrs",fn)),LS= as.vector(mls), sample.size=ss,...)
           return(fsrsCLF)
        }


        if(DS=='groundfish'){

        }


}

