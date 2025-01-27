#' @title nefsc.analysis
#' @description Stratified analysis of NEFSC lobster data with bootstrapped resampling and set-up the data for sensitivity analysis (removed strata 1310 and 1320 as they were not sampled since mid 80's (B. Shank pers comm))
#' @param \code{DS} :the selection of analysis, options include \code{stratified.estimates}
#' @param \code{out.dir} : specify the location of data saves, default is null and uses the project.datadirectory function as default
#' @param \code{p} : the parameter list which contains the specifics of the analysis at a minimum includes the season and area for analysis
#' @return saves or loads .rdata objects named \code{usinf}, \code{usdet}, \code{uscat}, \code{usstrat.area}
#' @examples
#' require(devtools)
#' load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
#' nefsc.db(DS = 'odbc.dump.redo')
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export



nefsc.analysis <- function(DS='stratified.estimates', out.dir = 'bio.lobster', p=p, ip=NULL,save=T) {
    
    loc = file.path( project.datadirectory(out.dir), "analysis" )
    dir.create( path=loc, recursive=T, showWarnings=F )
    if(p$season=='spring')  {SEASON = 'Spring'    } 
    if(p$season=='fall') {SEASON = 'Fall'}
    if(p$area=='georges.canada')               {STRATUM = c(1160,1170,1180,1190,1200,1210,1220); props = c(0.5211409, 0.7888889, 0.7383721, 0.0009412, 0.0007818, 0.4952830, 0.2753304)}
    if(p$area=='georges.US')                   {STRATUM = c(1130,1140,1150,1160,1170,1180,1190,1200,1210,1220,1230); props=c(1,1,1,1-0.5211409, 1-0.7888889, 1-0.7383721, 1-0.0009412, 1-0.0007818, 1-0.4952830, 1-0.2753304,1)}
    if(p$area=='LFA41')                        {STRATUM = c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1340, 1360); props = 1}
    if(p$area=='LFA41' & p$define.by.polygons) {STRATUM = c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1340, 1360); props = c(0.5211409, 0.7888889, 0.7383721, 0.0006892, 0.0005209, 0.4952830, 0.2753304, 0.3842528, 0.8799349,  0.0105999, 0.2922712)}
    if(p$area=='adjacentLFA41')                {STRATUM = c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1340, 1360); props = 1-c(0.5211409, 0.7888889, 0.7383721, 0.0006892, 0.0005209, 0.4952830, 0.2753304, 0.3842528, 0.8799349, 0.0105999, 0.2922712)}
    if(p$area=='all')                          {STRATUM = c(1080,1090,1100,1110,1120,1130,1140,1150,1160,1170,1190,1200,1210,1220,1230,1240,1180,1010,1020,1030,1040,1050,1060,1070,1300,1340,1351,1360,1370,1380,1390,1400,1610,1620,1630,1640,1650,1660,1670,1680,1690,1700,1710,1720,1730,1740,1750,1760,1250,1260,1270,1280,1290,1330,1350,1410,1420,1490,1990); props=rep(1,59)}
    if(p$area == 'LFA34' )                     {STRATUM = c(1340,1330,1360,1351,1352); props = c(0.873,1,0.1086,0.313619,0.5297)}
    if(p$area %in% c('LFA35','LFA36') )        {print('No Overlap'); stop()}
    if(p$area == 'LFA38')                      {STRATUM = c(1340,1351,1352,3920,3900); props = c(0.06997,0.6416,0.4703,0.9997,0.56988)}
    if(p$area == 'EGOM')                      {STRATUM = c(1160, 1170, 1180, 1210, 1220, 1290, 1300, 1340, 1360,1330,1351,1352,3900); 
                                                props =  c(0.518325391, 0.788107606, 0.733339882, 0.496439623, 0.277368510, 0.880947001, 0.532039207, 0.953296443, 1.000000000, 0.000293131, 0.957286619, 1.000000000, 0.618941550)}
    
    if(p$area == 'LFA36-38')                   {print('Only 38 not appropriate to group'); stop()}
    
    if(p$lobster.subunits==T & p$area=='Georges.Bank') {STRATUM = c(1160,1170,1180); props = c(0.3462588,0.6487552,0.450009)}   
    if(p$lobster.subunits==T &p$area=='Georges.Basin') {STRATUM = c(1160,1170,1180,1190,1200,1210,1220,1300,1290); props = c(0.170,0.1377,0.2812,0.0006,0.0005,0.4938,0.2771,0.321,0.195)}      
    if(p$lobster.subunits==T &p$area=='Crowell.Basin') {STRATUM = c(1300,1290,1360); props = c(0.1794,0.0707,0.23669)}   
    if(p$lobster.subunits==T &p$area=='SE.Browns' )    {STRATUM = c(1290); props = c(0.029519)}  
    if(p$lobster.subunits==T &p$area=='SW.Browns' )    {STRATUM = c(1300,1290,1340,1360); props=c(0.391,0.0879,0.0103,0.0606)}  
    
    
    if (exists( "libs", p)) {
        p0 = p;
        #     RLibrary( p$libs )
        p=p0
    }
    # if (exists( "libs", p)) RLibrary( p$libs )
    if (is.null(ip)) ip = 1:p$nruns
    
    if(DS %in% c('species.set.data')) {
      a = dir(loc)
      a = a[grep('strata.files',a)]
      a = a[grep(p$area,a)]
      a = a[grep(p$season,a)]
      if(p$length.based) {
        a = a[grep(p$size.class[1],a)]
        a = a[grep(p$size.class[2],a)]
      }
      if(p$by.sex) {
        k = ifelse(p$sex==1,'male',ifelse(p$sex==2,'female','berried'))
        if(length(k)>1) k = paste(k[1],k[2],sep='&')
        a = a[grep(k,a)]
      }
      a = load(file.path(loc,a))
      return(strata.files)
    }
    
    if(DS %in% c('stratified.estimates','stratified.estimates.redo')) {
      if(DS=='stratified.estimates'){
        a = dir(loc)
        a = a[grep('stratified',a)]
        a = a[grep(p$area,a)]
        a = a[grep(p$season,a)]
        if(p$length.based) {
          a = a[grep(p$size.class[1],a)]
          a = a[grep(p$size.class[2],a)]
        }
        if(p$by.sex) {
          k = ifelse(p$sex==1,'male',ifelse(p$sex==2,'female','berried'))
          if(length(k)>1) k = paste(k[1],k[2],sep='&')
          a = a[grep(k,a)]
        }
        
        load(file.path(loc,a))
        return(out)
      }
      
      set =  nefsc.db(DS='usinf.clean')
      cas =  nefsc.db(DS='uscat.clean')
      stra = nefsc.db(DS='usstrata.area')
      de =   nefsc.db(DS='usdet.clean')
    
      set$EID = 1:nrow(set)
      a = st_as_sf(readRDS(file.path(bio.directory,'bio.lobster.data','mapping_data','BTS_Strata.rds')) )
      a = st_make_valid(a)
      
      st_crs(a) <- 4326
 #turned this section off sept 17 2024
           #     a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
          ##      l = attributes(a)$PolyData[,c('PID','STRATA')]
          #      a = merge(a,l,by='PID',all.x=T)
          #      
           #     sett = findPolys(set,a)
            #    sett = merge(sett,l,by='PID')
            #    set = merge(set,sett,by='EID')
             #   set$STRATUM = set$STRATA
            #    set$STRATA = set$PID = set$SID = set$Bdry = set$EID = NULL
                
            set$STRATUM = as.numeric(set$STRATUM)
      # all catches have been converted to bigelow equivalents and therefore do not need any further towed distance calculations, the DISTCORRECTION is a standardized distance against the mean of the towed distance for that gear and is therefore the correction for towed distance to be used
      #US nautical mile is 6080.2ft bigelow is 42.6'
      #tow dist is 1nm for bigelow
      
      stra$NH = stra$area
      strata.files = list()
      big.out = matrix(NA,nrow=p$nruns,ncol=length(seq(0.01,0.99,0.01))+1)
      out = data.frame(yr=NA,w.yst=NA,w.yst.se=NA,w.ci.yst.l=NA,w.ci.yst.u=NA,w.Yst=NA,w.ci.Yst.l=NA,w.ci.Yst.u=NA,n.yst=NA,n.yst.se=NA,n.ci.yst.l=NA,n.ci.yst.u=NA,n.Yst=NA,n.ci.Yst.l=NA,n.ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithLobster=NA,ObsLobs = NA,gini = NA,gini.lo =NA, gini.hi=NA,df.yst=NA)
      mp=0
      np=1
      effic.out = data.frame(yr=NA,strat.effic.wt=NA,alloc.effic.wt=NA,strat.effic.n=NA,alloc.effic.n=NA)
      nopt.out =  list()
      
      
      for(iip in ip) {
        mp = mp+1
        yr = p$runs[iip,"yrs"]
        print ( p$runs[iip,] )
        
        #iv = which(cas$spec %in% vv) Turn on if species are selected right now only lobster is being brought in
        iy = which(set$GMT_YEAR %in% yr)
        ix = which(tolower(set$SEASON) == tolower(SEASON))
        pi = 'base'
        if(p$define.by.polygons) {
          print('dbp')
          pi = 'restratified'
          if(p$area=='georges.US') {return('this is not setup')}
          if(p$area=='LFA41'){
            l41 = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA41Offareas.csv'))
            if(p$lobster.subunits) {
              l = l41[which(l41$OFFAREA == p$area),]
            } else {
              print('All LFA41 subsetted by LFA Area')
              l41 = joinPolys(as.PolySet(l41),operation='UNION')
              attr(l41,'projection') <- 'LL'
              l =l41 = subset(l41, SID==1)
            }
          }
          if(p$area %in% c('LFA34','LFA35','LFA36','LFA38','LFA36-38','EGOM')) {
            LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
            if(p$area %ni% c('LFA35-38','EGOM'))        ppp = as.numeric(strsplit(p$area,"LFA")[[c(1,2)]])
            if(p$area =='LFA35-38') ppp = c(35,36,38)
            if(p$area =='EGOM') ppp = c(33,34,35,36,38,41)
            
            lll = subset(LFAs,PID %in% ppp)
            l = subset(lll,SID==1)
            attr(l,'projection') <- "LL"
          }
          
          set$EID = 1:nrow(set)
          a = findPolys(set,l)
          iz = which(set$EID %in% a$EID)
          if(p$area=='adjacentLFA41') {
            iz = which(set$EID %ni% a$EID)
            ir = which(set$STRATUM %in% c(STRATUM))
            iz = intersect(iz,ir)
          }
        } else {
          iz = which(set$STRATUM %in% c(STRATUM))
        }
        iy = intersect(intersect(ix,iy),iz)
        se = set[iy,]
        if(nrow(se)<1) {out[mp,1] <- yr 
        next()
        }
        se$EID = 1:nrow(se)
        ca = cas #cas[iv,] see above
        se$z = se$AVGDEPTH
        vars.2.keep = c('MISSION','X','Y','ID','SETNO','BEGIN_GMT_TOWDATE','GMT_YEAR','STRATUM','z','BOTTEMP','BOTSALIN','DISTCORRECTION','SEASON')
        se = se[,vars.2.keep]
        se$slong = se$X
        se$slat = se$Y
        if(nrow(se)>1){
          p$lb = p$length.based
          if(p$by.sex & !p$length.based) {p$size.class=c(0,1000); p$length.based=T}
          if(!p$lb) { vars.2.keep =c('MISSION','SETNO','TOTWGT','TOTNO')#,'SIZE_CLASS')
          ca = ca[,vars.2.keep]
          }
          
          
          if(p$length.based) {
            dp = de
            dp = dp[which(dp$ID %in% unique(se$ID)),]
            if(nrow(dp)>=1) {
              
              flf = p$size.class[1]:p$size.class[2]
              dp$clen2 = ifelse(dp$FLEN %in% flf,dp$CLEN,0)
              
              if(p$by.sex) dp$clen2 = ifelse(dp$FSEX %in% p$sex, dp$clen2, 0)
              
              dp$pb = dp$FWT * dp$CLEN
              dp$pb1 = dp$FWT * dp$clen2
              
              dpp = data.frame(mission=NA,setno=NA,size_class=NA,pn=NA,pw=NA)
              
              if(nrow(dp)>0) {
                dpp = aggregate(cbind(CLEN,clen2,pb,pb1)~MISSION+SETNO,data=dp,FUN=sum)
                dpp$pn = dpp$clen2/dpp$CLEN
                dpp$pw = dpp$pb1/dpp$pb
                dpp = dpp[,c('MISSION','SETNO','pn','pw')]
              }
              
              ca1 = merge(ca,dpp,by=c('MISSION','SETNO'))
              ca1$TOTWGT = ca1$TOTWGT * ca1$pw
              ca1$TOTNO = ca1$TOTNO * ca1$pn
              vars.2.keep =c('MISSION','SETNO','TOTWGT','TOTNO')
              ca = ca1[,vars.2.keep]
            }
          }
          
          if(nrow(ca)>=1) {
            #browser()
            ca = aggregate(cbind(TOTWGT,TOTNO)~MISSION+SETNO,data=ca,FUN=sum)
            sc = merge(se,ca,by=c('MISSION','SETNO'),all.x=T)
            sc[,c('TOTWGT','TOTNO')] = na.zero(sc[,c('TOTWGT','TOTNO')])
            #sc$TOTNO = sc$TOTNO / sc$DISTCORRECTION #this happens during the nefsc.db uscat.clean step
            #sc$TOTWGT = sc$TOTWGT / sc$DISTCORRECTION
            io = which(stra$STRATA %in% unique(sc$STRATUM))
            sc$Strata = sc$STRATUM
            st = stra[io,c('STRATA','NH')]
            st = st[order(st$STRATA),]
            spr = data.frame(STRATA = STRATUM, Pr = props)
            st = merge(st,spr)
            names(st)[1] = 'Strata'
            if(p$reweight.strata) st$NH = st$NH * st$Pr #weights the strata based on area in selected region
            
            if(exists('temperature',p)) {sc = sc[!is.na(sc$BOTTEMP),] ; sc$TOTNO = sc$BOTTEMP; sc$TOTWGT = sc$BOTTEMP }
            if(nrow(sc)==0)next()
            if(length(st$Strata)==0) next()
            st = st[order(st$Strata),]
            st = Prepare.strata.file(st)
            sc1= sc
            sc = Prepare.strata.data(sc)
            strata.files[[mp]]  = list(st,sc1)
            sW = Stratify(sc,st,sc$TOTWGT)
            sN = Stratify(sc,st,sc$TOTNO)
            ssW = summary(sW)
            ssN = summary(sN)
            
            if(p$strata.efficiencies) {
              ssW = summary(sW,effic=T,nopt=T)
              ssN = summary(sN,effic=T,nopt=T)
              effic.out[mp,] = c(yr,ssW$effic.str,ssW$effic.alloc,ssN$effic.str,ssN$effic.alloc)
              nopt.out[[mp]] = list(yr,ssW$n.opt,ssN$n.opt)
            }
            
            if(!p$strata.efficiencies) {
              
              bsW = list(NA,NA,NA)
              bsN = list(NA,NA,NA)
              nt = NA
              if(p$bootstrapped.ci) {
                bsW = summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='BC')
                bsN = summary(boot.strata(sN,method='BWR',nresamp=1000),ci.method='BC')
                nt  = sum(sW$Nh)/1000
              }
              if(exists('big.ci',p)) {
                big.out[mp,] = c(yr,summary(boot.strata(sN,method='BWR',nresamp=1000),ci.method='BC',big.ci=T))
              }
              out[mp,] = c(yr,ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]]/1000,bsW[[1]][1]*nt,bsW[[1]][2]*nt,
                           ssN[[1]],ssN[[2]],bsN[[1]][1],bsN[[1]][2],ssN[[3]]/1000,bsN[[1]][1]*nt,bsN[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),round(sum(sc$TOTNO)),ssN$gini,bsN[[2]][1],bsN[[2]][2],ssN$df.yst)
            }   else {
              out[mp,] = c(yr,rep(0,23))
            }
            
          }
        }
      }
      if(exists('big.ci',p)) {
        return(big.out)
      }
      if(p$strata.efficiencies) {
        return(list(effic.out,nopt.out))
      }
      lle = 'all'
      lbs = 'not'
      if(p$length.based) lle = paste(p$size.class[1],p$size.class[2],sep="-")
      if(p$by.sex)      lbs = ifelse(p$sex==1,'male',ifelse(p$sex==2,'female','berried'))
      if(length(lbs)>1) lbs = paste(lbs[1],lbs[2],sep='&')
      
      fn = paste('stratified','nefsc',p$season,p$area,pi,'length',lle,lbs,'sexed','rdata',sep=".")
      fn.st = paste('strata.files','nefsc',p$season,p$area,pi,'length',lle,lbs,'sexed','rdata',sep=".")
      if(save) {  
        save(out,file=file.path(loc,fn))
        save(strata.files,file=file.path(loc,fn.st))
      }
      if(p$strata.files.return) return(strata.files)
      if(exists('return.both',p)) return(list(strat.ests = out,data=strata.files))
      return(out)
      
    }
    
}
