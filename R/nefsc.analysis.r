#' @title nefsc.analysis
#' @description Stratified analysis of NEFSC lobster data with bootstrapped resampling and set-up the data for sensitivity analysis
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



nefsc.analysis <- function(DS='stratified.estimates', out.dir = 'bio.lobster', p=p, ip=NULL) {
    loc = file.path( project.datadirectory(out.dir), "analysis" ,'nefsc')

    dir.create( path=loc, recursive=T, showWarnings=F )
         if(p$season=='spring')  {SEASON = 'Spring'    } 
         if(p$season=='fall') {SEASON = 'Fall'}
         if(p$area=='georges.canada') {STRATUM = c(1160,1170,1180,1190,1200,1210,1220); props = c(0.5211409, 0.7888889, 0.7383721, 0.0009412, 0.0007818, 0.4952830, 0.2753304)}
         if(p$area=='georges.US') {STRATUM = c(1130,1140,1150,1160,1170,1180,1190,1200,1210,1220,1230); props=c(1,1,1,1-0.5211409, 1-0.7888889, 1-0.7383721, 1-0.0009412, 1-0.0007818, 1-0.4952830, 1-0.2753304,1)}
         if(p$area=='LFA41') {STRATUM = c(1160, 1170, 1180, 1190, 1200, 1210, 1220, 1290, 1300, 1310, 1340, 1360); props = c(0.5211409, 0.7888889, 0.7383721, 0.0006892, 0.0005209, 0.4952830, 0.2753304, 0.3842528, 0.8799349, 0.1597051, 0.0105999, 0.2922712)}
          
         if(p$lobster.subunits==T & p$area=='Georges.Bank') {STRATUM = c(1160,1170,1180); props = c(0.3462588,0.6487552,0.450009)}   
         if(p$lobster.subunits==T &p$area=='Georges.Basin') {STRATUM = c(1160,1170,1180,1190,1200,1210,1220,1300,1290); props = c(0.170,0.1377,0.2812,0.0006,0.0005,0.4938,0.2771,0.321,0.195)}      
         if(p$lobster.subunits==T &p$area=='Crowell.Basin') {STRATUM = c(1300,1290,1360); props = c(0.1794,0.0707,0.23669)}   
         if(p$lobster.subunits==T &p$area=='SE.Browns' )    {STRATUM = c(1290,1310); props = c(0.029519,0.08869)}  
         if(p$lobster.subunits==T &p$area=='SW.Browns' )    {STRATUM = c(1300,1290,1310,1340,1360); props=c(0.391,0.0879,0.0709,0.0103,0.0606)}  

         
    
         if(!is.null(p$strat)) strat = p$strat

         if (exists( "libs", p)) {
            p0 = p;
            RLibrary( p$libs )
            p=p0
          }
         if (exists( "libs", p)) RLibrary( p$libs )
         if (is.null(ip)) ip = 1:p$nruns

if(DS %in% c('species.set.data')) {
           outa = NULL
            a = dir(loc)
            a = a[grep('strata.files',a)]
            a = a[grep(paste(p$species,collapse="|"),a)]
            if(exists('strata.files.return',p)){
                  it = grep(paste(p$size.class,collapse="-"),a)
                  load(file.path(loc,a[it]))
                  return(strata.files)
                  }
            for(op in a) {
                load(file.path(loc,op))
                al = lapply(strata.files,"[[",2)
                al = do.call('rbind',al)
                al$Sp= strsplit(op,"\\.")[[1]][3]
                b = strsplit(op,"\\.")
                b = b[[1]][grep('length',b[[1]])+1]
                al = rename.df(al,c('totwgt','totno'),c(paste('totwgt',b,sep="."),paste('totno',b,sep=".")))
                if(is.null(outa)) {outa = rbind(al,outa)
                  } else {
                 outa = merge(outa,al[,c('mission','setno',paste('totwgt',b,sep="."),paste('totno',b,sep="."))],by=c('mission','setno'))
                }
                }
                return(outa)
              }

if(DS %in% c('stratified.estimates','stratified.estimates.redo')) {
          if(DS=='stratified.estimates'){
            outa = NULL
            a = dir(loc)
            a = a[grep('stratified',a)]
            a = a[grep(paste(p$species,collapse="|"),a)]
          for(op in a) {
                load(file.path(loc,op))
                b = strsplit(op,"\\.")
                b = b[[1]][grep('length',b[[1]])+1]
                out$group = b
                outa = rbind(out,outa)
                }
                return(outa)
              }


        set =  nefsc.db(DS='usinf.clean')
        cas =  nefsc.db(DS='uscat.clean')
        stra = nefsc.db(DS='usstrata.area')
        de =   nefsc.db(DS='usdet.clean')


        # all catches have been converted to bigelow equivalents and therefore do not need any further towed distance calculations, the DISTCORRECTION is a standardized distance against the mean of the towed distance for that gear and is therefore the correction for towed distance to be used
        #US nautical mile is 6080.2ft bigelow is 42.6'
        #tow dist is 1nm for bigelow
     stra$NH = stra$AREA/(1* 42.6 / 6080.2)
     strata.files = list()
     out = data.frame(yr=NA,w.yst=NA,w.yst.se=NA,w.ci.yst.l=NA,w.ci.yst.u=NA,w.Yst=NA,w.ci.Yst.l=NA,w.ci.Yst.u=NA,n.yst=NA,n.yst.se=NA,n.ci.yst.l=NA,n.ci.yst.u=NA,n.Yst=NA,n.ci.Yst.l=NA,n.ci.Yst.u=NA,dwao=NA)
     mp=0
     np=1
     
     for(iip in ip) {
            mp = mp+1
            yr = p$runs[iip,"yrs"]
            print ( p$runs[iip,] )
         
            #iv = which(cas$spec %in% vv) Turn on if species are selected right now only lobster is being brought in
            iy = which(set$GMT_YEAR %in% yr)
            ix = which(set$SEASON == SEASON)
            iz = which(set$STRATUM %in% c(STRATUM))
            iy = intersect(intersect(ix,iy),iz)
                se = set[iy,]
                ca = cas #cas[iv,] see above
                se$z = se$AVGDEPTH
                vars.2.keep = c('MISSION','X','Y','ID','SETNO','BEGIN_GMT_TOWDATE','GMT_YEAR','STRATUM','z','BOTTEMP','BOTSALIN','DISTCORRECTION','SEASON')
                se = se[,vars.2.keep]
                se$slong = se$X
                se$slat = se$Y

        p$lb = p$length.based

        if(p$by.sex & !p$length.based) {p$size.class=c(0,1000); p$length.based=T}

        if(!p$lb) { vars.2.keep =c('MISSION','SETNO','TOTWGT','TOTNO','SIZE_CLASS')
                    ca = ca[,vars.2.keep]
                }

        if(p$length.based){
                  dp = de
                  dp = dp[which(dp$ID %in% unique(se$ID)),]
                  flf = p$size.class[1]:p$size.class[2]
                  dp$clen2 = ifelse(dp$LENGTH %in% flf,dp$CLEN,0)

              if(p$by.sex) dp$clen2 = ifelse(dp$FSEX %in% p$sex, dp$clen2, 0)

              if(any(!is.finite(dp$FWT))) {
                  io = which(!is.finite(dp$FWT))
                  fit = nls(fwt~a*flen^b,de[which(is.finite(de$FWT)),],start=list(a=0.001,b=3.3))
                  ab = coef(fit)
                  dp$FWT[io] = ab[1]*dp$FLEN[io]^ab[2]
                  }
                  dp$pb = dp$FWT * dp$CLEN
                  dp$pb1 = dp$FWT * dp$clen2

                  dpp = data.frame(mission=NA,setno=NA,size_class=NA,pn=NA,pw=NA)
                  if(nrow(dp)>0) {
                  dpp = aggregate(cbind(CLEN,clen2,pb,pb1)~MISSION+SETNO+SIZE_CLASS,data=dp,FUN=sum)
                  dpp$pn = dpp$clen2/dpp$CLEN
                  dpp$pw = dpp$pb1/dpp$pb
                  dpp = dpp[,c('MISSION','SETNO','SIZE_CLASS','pn','pw')]
                  }
                  ca1 = merge(ca,dpp,by=c('MISSION','SETNO','SIZE_CLASS'))
                  ca1$TOTWGT = ca1$TOTWGT * ca1$pw
                  ca1$TOTNO = ca1$TOTNO * ca1$pn
                  vars.2.keep =c('MISSION','SETNO','TOTWGT','TOTNO','SIZE_CLASS')
                  ca = ca1[,vars.2.keep]
              }

            if(nrow(ca)>=1) {
		        ca = aggregate(cbind(TOTWGT,TOTNO)~MISSION+SETNO,data=ca,FUN=sum)
                  sc = merge(se,ca,by=c('MISSION','SETNO'),all.x=T)
                  sc[,c('TOTWGT','TOTNO')] = na.zero(sc[,c('TOTWGT','TOTNO')])
                  sc$TOTNO = sc$TOTNO * sc$DISTCORRECTION
                  sc$TOTWGT = sc$TOTWGT * sc$DISTCORRECTION
                  io = which(stra$STRAT %in% unique(sc$STRATUM))
                  sc$Strata = sc$STRATUM
                  st = stra[io,c('STRAT','NH')]
                  st = st[order(st$STRAT),]
                  spr = data.frame(STRAT = STRATUM, Pr = props)
                  st = merge(st,spr)
                  names(st)[1] = 'Strata'
                  st$NH = st$NH * st$Pr
                  st = Prepare.strata.file(st)
                  sc1= sc
                  sc = Prepare.strata.data(sc)
                  strata.files[[mp]]  = list(st,sc1)
                  sW = Stratify(sc,st,sc$TOTWGT)
                  sN = Stratify(sc,st,sc$TOTNO)
                  ssW = summary(sW)
                  ssN = summary(sN)
               if(p$bootstrapped.ci) {
                  bsW = summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='BC')
                  bsN = summary(boot.strata(sN,method='BWR',nresamp=1000),ci.method='BC')
                  nt  = sum(sW$Nh)/1000
                }
                out[mp,] = c(yr,ssW[[1]],ssW[[2]],bsW[1],bsW[2],ssW[[3]]/1000,bsW[1]*nt,bsW[2]*nt,
                ssN[[1]],ssN[[2]],bsN[1],bsN[2],ssN[[3]]/1000,bsN[1]*nt,bsN[2]*nt,ssW$dwao)
                } else {
                out[mp,] = c(yr,rep(0,15))
               
              }
            }
                        lle = 'all'
              if(p$length.based) lle = paste(p$size.class[1],p$size.class[2],sep="-")
              fn = paste('stratified','nefsc',p$season,p$series,'strata',min(strat),max(strat),'length',lle,'rdata',sep=".")
              fn.st = paste('strata.files','nefsc',p$season,p$series,'strata',min(strat),max(strat),'length',lle,'rdata',sep=".")
             
              save(out,file=file.path(loc,fn))
              save(strata.files,file=file.path(loc,fn.st))
             if(p$strata.files.return) return(strata.files)
             return(out)

   }

}
