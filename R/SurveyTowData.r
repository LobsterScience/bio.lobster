
SurveyTowData<-function(Size.range=c(70,82.5),Sex = c(1,2,3), Years=1970:2018,lab=NULL,redo=T,by.sex=F, Lobster.survey.correction=F,lfas=c("34","35","36","37","38")){


  if (redo){
  	require(bio.groundfish)
    p=list()
	
     #American Trawl Survey Data

     	inf = nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)
      ca = nefsc.db( DS = 'uscat.clean.redo',fn.root = NULL,p=p)
      de = nefsc.db( DS = 'usdet.clean.redo',fn.root = NULL,p=p)
         nefsc.db(DS = 'usstrata.area.redo')        

         #American Trawl Surveys Spring and Fall
                p$reweight.strata = F #this subsets 
                p$years.to.estimate = Years
                p$length.based = T

            ###important lines    
                p$size.class= floor(Size.range)
                p$by.sex =  F
                p$sex = Sex
            ####
                p$bootstrapped.ci=F
                p$strata.files.return=T
                p$strata.efficiencies=F
                p$clusters = c( rep( "localhost", 7) )
                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'all'
                        p$return.both = NULL
                      p = make.list(list(yrs=p$years.to.estimate),Y=p)
                      aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
                SpringNefsc = do.call(rbind,lapply(aout,"[[",2))
                
                p$season =c('fall')# p$series =c('spring');p$series =c('fall')
                aout= nefsc.analysis(DS='stratified.estimates.redo',p=p)
                FallNefsc = do.call(rbind,lapply(aout,"[[",2))
        
        nefsc.tow.area = 0.023144074

        FallNefsc$LobDen = FallNefsc$TOTNO / nefsc.tow.area
        FallNefsc$AreaSwept = nefsc.tow.area * FallNefsc$DISTCORRECTION
        FallNefsc$LobCatch = round(FallNefsc$TOTNO * FallNefsc$DISTCORRECTION)
        
        SpringNefsc$LobDen = SpringNefsc$TOTNO / nefsc.tow.area
        SpringNefsc$AreaSwept = nefsc.tow.area * SpringNefsc$DISTCORRECTION
        SpringNefsc$LobCatch = round(SpringNefsc$TOTNO * SpringNefsc$DISTCORRECTION)

          #DFO RV Setup #we only have size data for lobster post 1998 
              p$reweight.strata = F #this subsets 
              p$series =c('summer')# p$series =c('georges');p$series =c('fall')
              p$define.by.polygons = F
              p$lobster.subunits=F
              p$area = 'custom'
              p$strat = 470:495
              p$years.to.estimate = Years
              p$length.based = T
          #Important lines
              p$size.class= floor(Size.range)
              p$by.sex =  by.sex
              p$sex = Sex
          ###########  
              p$bootstrapped.ci=F
              p$strata.files.return=T
              p$vessel.correction.fixed=1.2
              p$clusters = c( rep( "localhost", 7) )
              p$strata.efficiencies = F
              p = make.list(list(yrs=p$years.to.estimate),Y=p)
              aout= bio.lobster::dfo.rv.analysis(DS='stratified.estimates.redo',p=p)
        DFOsummer = do.call(rbind,lapply(aout,"[[",2))

        dfo.tow.area = 0.040502129
        
        DFOsummer$LobDen = DFOsummer$totno / dfo.tow.area
        DFOsummer$AreaSwept = dfo.tow.area * (DFOsummer$dist/1.75)
        DFOsummer$LobCatch = round(DFOsummer$totno * (DFOsummer$dist/1.75))

        DFOsummer$YEAR = year(DFOsummer$sdate)

      if(by.sex==T){
        scalSurv0<-ScallopSurveyProcess(size.range=Size.range,bin.size=2.5,sex=0,convert2nest=Lobster.survey.correction)
        scalSurv<-ScallopSurveyProcess(size.range=Size.range,bin.size=2.5,sex=Sex,convert2nest=Lobster.survey.correction)
        if(Sex%in%1:2)scalSurv$LobDen = scalSurv$LobDen + scalSurv0$LobDen/2
      } else{
        scalSurv<-ScallopSurveyProcess(size.range=Size.range,bin.size=2.5,convert2nest=Lobster.survey.correction)
      }

      LobSurvNest<-LobsterSurveyProcess(lfa=lfas,yrs=Years,bin.size=2.5,gear.type='NEST',size.range=Size.range,sex=Sex)
      LobSurvBalloon<-LobsterSurveyProcess(lfa=lfas,yrs=Years,bin.size=2.5,gear.type='280 BALLOON',size.range=Size.range,sex=Sex)
      LobSurvCombined<-LobsterSurveyProcess(lfa=lfas,yrs=Years,bin.size=2.5,Net='NEST',size.range=Size.range,sex=Sex)



      col.names = c("year","setno","date","X","Y","Z","LobDen","AreaSwept","LobCatch")


      LobSurvBalloon = subset(LobSurvBalloon,!is.na(LobDen),c("YEAR","SET_NO","SET_DATE","SET_LONG","SET_LAT","SET_DEPTH","LobDen","AREA_SWEPT"))
      LobSurvBalloon$LobCatch = round(LobSurvBalloon$LobDen *  LobSurvBalloon$AREA_SWEPT)
      names(LobSurvBalloon) = col.names
      LobSurvBalloon$survey = "LobsterBalloon"

      LobSurvNest = subset(LobSurvNest,!is.na(LobDen),c("YEAR","SET_NO","SET_DATE","SET_LONG","SET_LAT","SET_DEPTH","LobDen","AREA_SWEPT"))
      LobSurvNest$LobCatch = round(LobSurvNest$LobDen *  LobSurvNest$AREA_SWEPT)
      names(LobSurvNest) = col.names
      LobSurvNest$survey = "LobsterNest"
      
      LobSurvCombined = subset(LobSurvCombined,!is.na(LobDen),c("YEAR","SET_NO","SET_DATE","SET_LONG","SET_LAT","SET_DEPTH","LobDen","AREA_SWEPT"))
      LobSurvCombined$LobCatch = round(LobSurvCombined$LobDen *  LobSurvCombined$AREA_SWEPT)
      names(LobSurvCombined) = col.names
      LobSurvCombined$survey = "Lobster"

     
      scalSurv = subset(scalSurv,!is.na(LobDen),c("YEAR","TOW_SEQ","TOW_DATE","lon","lat","DEPTH","LobDen","AREA_SWEPT"))
      scalSurv$LobCatch = round(scalSurv$LobDen *  scalSurv$AREA_SWEPT)
      names(scalSurv) = col.names
      scalSurv$survey = "Scallop"
      
      DFOsummer = subset(DFOsummer,!is.na(LobDen),c("YEAR","setno","sdate","X","Y","z","LobDen","AreaSwept","LobCatch"))
      names(DFOsummer) = col.names
      DFOsummer$survey = "DFOsummer"

      FallNefsc = subset(FallNefsc,!is.na(LobDen),c("GMT_YEAR","SETNO","BEGIN_GMT_TOWDATE","X","Y","z","LobDen","AreaSwept","LobCatch"))
      names(FallNefsc) = col.names
      FallNefsc$survey = "NEFSCfall"

      SpringNefsc = subset(SpringNefsc,!is.na(LobDen),c("GMT_YEAR","SETNO","BEGIN_GMT_TOWDATE","X","Y","z","LobDen","AreaSwept","LobCatch"))
      names(SpringNefsc) = col.names
      SpringNefsc$survey = "NEFSCspring"


      if(Lobster.survey.correction) AllSurveys = rbind(LobSurvCombined,scalSurv,DFOsummer,FallNefsc,SpringNefsc)
      else AllSurveys = rbind(LobSurvBalloon,LobSurvNest,scalSurv,DFOsummer,FallNefsc,SpringNefsc)
      AllSurveys =assignArea(AllSurveys,coords=c("X","Y"))

      AllSurveys = AllSurveys[order(AllSurveys$date),]

    write.csv(AllSurveys,file.path(project.datadirectory("bio.lobster"),"data","products","LFA3438Framework2019",paste0("AllSurvey",lab,".csv")),row.names=F)
  } else {
    AllSurveys = read.csv(file.path(project.datadirectory("bio.lobster"),"data","products","LFA3438Framework2019",paste0("AllSurvey",lab,".csv")))

  }

return(AllSurveys)
}








