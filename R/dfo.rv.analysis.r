#' @title dfo.rv.analysis
#' @description Stratified analysis of DFO lobster data with bootstrapped resampling and set-up the data for sensitivity analysis
#' @param \code{DS} :the selection of analysis, options include \code{stratified.estimates}
#' @param \code{out.dir} : specify the location of data saves, default is null and uses the project.datadirectory function as default
#' @param \code{p} : the parameter list which contains the specifics of the analysis at a minimum includes the season and area for analysis
#' @return saves or loads .rdata objects 
#' @examples
#' require(devtools)
#' load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
#' dfo.rv.analysis(DS = 'stratified.estimates')
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export



dfo.rv.analysis <- function(DS='stratified.estimates', out.dir = 'bio.lobster', p=p, ip=NULL,save=T) {
    loc = file.path( project.datadirectory(out.dir), "analysis" )
  print('since spring 2024 all means will now be in x/km2 not x/tow as previous....y axis scales will be different')
    dir.create( path=loc, recursive=T, showWarnings=F )
          props = 1 
         if(p$series=='summer')  {mns = c('June','July','August')     ;     strat = c(440:495)}
         if(p$series=='georges') {mns = c('February','March','April');      strat = c('5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8','5Z9')}
         if(p$area=='EGOM')                                                 {strat = c(467:495); props=1}
         if(p$area=='Georges.Canada' & p$series == 'georges')              {strat = c('5Z1','5Z2')  }
         if(p$area=='Georges.US' & p$series =='georges')                   {strat = c('5Z3','5Z4','5Z5','5Z6','5Z7','5Z8')}
         if(p$area== 'LFA41' & p$series =='summer')                        {strat = c(472,473,477,478,481,482,483,484,485,480); props = 1}
         if(p$area== 'LFA41' & p$series =='summer' & p$define.by.polygons) {strat = c(472,473,477,478,481,482,483,484,485); props = c(0.2196,0.4415,0.7593,0.7151,0.1379,0.6991,0.8869,0.50897,0.070409)}
         if(p$area== 'adjacentLFA41' & p$series =='summer')                {strat = c(472,473,477,478,481,482,483,484,485,480); props = 1-c(0.2196,0.4415,0.7593,0.7151,0.1379,0.6991,0.8869,0.50897,0.070409,0)}
         if(p$area== 'LFA40' & p$series =='summer')                        {strat = c(476,477,480,481,482); props = c(0.02,0.0204,1,0.0904,0.2865)}
         if(p$area == 'LFA34' & p$series == 'summer')                      {strat = c(476,481,484,485,490,491,492,495); props = c(0.0268,0.3412,0.2972,0.9251,0.880,0.8443,0.09096,0.01133)}
         if(p$area == 'LFA35' & p$series =='summer')                       {strat = c(490,491,494,495); props = c(0.1190,0.0274,0.07246,0.9330)}
         if(p$area == 'LFA36' & p$series =='summer')                       {strat = c(491,492,493,494,495); props = c(0.0222,0.151,0.7457,0.9311,0.05379)}
         if(p$area == 'LFA38' & p$series =='summer')                       {strat = c(484,491,492,493); props = c(0.0315,0.104,0.6304,0.2167)}
         if(p$area == 'LFA35-38' & p$series =='summer')                    {strat = c(484,490,491,492,493,494,495); props = c(0.0315,0.119,0.154,0.781,0.9624,1.00,0.9899)}
        
         if(p$lobster.subunits==T &p$area=='Georges.Basin' & p$series=='summer') {strat = c(482,483); props = c(0.1462, 0.2696)}      
         if(p$lobster.subunits==T &p$area=='Crowell.Basin' & p$series=='summer') {strat = c(482,483,484,485); props = c(0.1963,0.1913,0.3935,0.0483)}   
         if(p$lobster.subunits==T &p$area=='SE.Browns' & p$series=='summer')    {strat = c(472,473,475,477,478,481,482); props = c(0.2196,0.4415,0.00202,0.7592,0.7151,0.0868,0.0871)}  
         if(p$lobster.subunits==T &p$area=='SW.Browns' & p$series=='summer')    {strat = c(481,482,483,484,485); props=c(0.0509,0.2684,0.4358,0.1143,0.02197)}  
         if(p$lobster.subunits==T & p$area=='Georges.Bank' & p$series=='georges') {strat = c('5Z1','5Z2'); props = c(0.6813, 0.5474)}   
         if(p$lobster.subunits==T &p$area=='Georges.Basin' & p$series=='georges') {strat = c('5Z1','5Z2'); props = c(0.3187, 0.4537)}
         if(p$area == 'custom') {strat = p$strat; props=rep(1,length(strat))}

         if (exists( "libs", p)) {
            p0 = p;
         #   RLibrary( p$libs )
            p=p0
          }
        # if (exists( "libs", p)) RLibrary( p$libs )
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
            a = a[grep(p$area,a)]
            a = a[grep(p$series,a)]
            if(p$length.based) {
                a = a[grep(p$size.class[1],a)]
                a = a[grep(p$size.class[2],a)]
              }
            if(p$by.sex) {
              k = ifelse(p$sex==1,'male',ifelse(p$sex==2,'female','berried'))
              a = a[grep(k,a)]
            }

             load(file.path(loc,a))
             return(out)
             }

        o = groundfish.db('gs_trawl_conversions')
        set = o$gsinf
        cas = o$gscat
        stra = groundfish.db(DS='gsstratum')
        de = o$gsdet
        
        set$X = convert.dd.dddd(set$slong) *-1
        set$Y = convert.dd.dddd(set$slat)

        stra$NH = as.numeric(stra$area)*3.4299 #square NM to square km
        ii = which(months(set$sdate) %in% mns & set$strat %in% strat & set$type %in% c(1,5) & set$gear %in% c(3,9,15))
        set = set[ii,]

        cas = subset(cas,spec==2550)
    strata.files = list()
    out = data.frame(yr=NA,w.yst=NA,w.yst.se=NA,w.ci.yst.l=NA,w.ci.yst.u=NA,w.Yst=NA,w.ci.Yst.l=NA,w.ci.Yst.u=NA,n.yst=NA,n.yst.se=NA,n.ci.yst.l=NA,n.ci.yst.u=NA,n.Yst=NA,n.ci.Yst.l=NA,n.ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithLobster=NA,ObsLobs = NA,gini = NA,gini.lo =NA, gini.hi=NA,df.yst=NA)
  big.out = matrix(NA,nrow=p$nruns,ncol=length(seq(0.01,0.99,0.01))+1)
   
    mp=0
    np=1
    effic.out = data.frame(yr=NA,strat.effic.wt=NA,alloc.effic.wt=NA,strat.effic.n=NA,alloc.effic.n=NA)
    nopt.out =  list()

    for(iip in ip) {
      
            mp = mp+1
            yr = p$runs[iip,"yrs"]
            print ( p$runs[iip,] )
            iy = which(lubridate::year(set$sdate) %in% yr)
            iv = which(cas$spec==2550)
pi='base'

        if(p$define.by.polygons) {
               if(p$area=='LFA41') {
                l = l41 = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA41Offareas.csv'))
               pi = 'restratified'
            if(p$lobster.subunits) {
                       l = l41[which(l41$OFFAREA == p$area),]
                    } else {
                          print('All LFA41 subsetted by LFA Area')
                          l41 = PBSmapping::joinPolys(as.PolySet(l41),operation='UNION')
                        attr(l41,'projection') <- 'LL'
                        l41 = subset(l41, SID==1)
                    }
                        set$EID = 1:nrow(set)
                        a = PBSmapping::findPolys(set,l)
                       iz = which(set$EID %in% a$EID)
                       if(p$area=='adjacentLFA41') { 
                                  iz = which(set$EID %ni% a$EID)
                                  ir = which(set$strat %in% c(strat))  
                                  iz = intersect(iz,ir)
                                }
                  }
          if(p$area=='LFA40') {
                      LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
                      l = l41 = subset(LFAs,PID==40)
                     attr(l41,'projection') <- 'LL'
                     set$EID = 1:nrow(set)
                     a = PBSmapping::findPolys(set,l)
                       iz = which(set$EID %in% a$EID)
                    }
          if(p$area %in% c('LFA34','LFA35','LFA36','LFA38','LFA35-38')) {
                        LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
                        ppp = as.numeric(strsplit(p$area,"LFA")[[c(1,2)]])
                  if(p$area =='LFA35-38') ppp = c(35,36,38)
                        lll = subset(LFAs,PID %in% ppp)
                        l = subset(LFAs,SID==1)
                        attr(l,'projection') <- "LL"
                        set$EID = 1:nrow(set)
                        a = PBSmapping::findPolys(set,l)
                       iz = which(set$EID %in% a$EID)
                    }} else {
                              iz = which(set$strat %in% c(strat))
                    }
                se = set[intersect(iy,iz),]
                
                if(nrow(se)>0){
                se$EID = 1:nrow(se)
                ca = cas[iv,]
                se$z = (se$dmin+se$dmax) / 2 * 1.8288 #from fm to m
                vars.2.keep = c('mission','X','Y','setno','sdate','dist','strat','z','bottom_temperature','bottom_salinity','type')
                se = se[,vars.2.keep]
        p$lb = p$length.based
        if(p$by.sex & !p$length.based) {p$size.class=c(0,1000); p$length.based=T}

        if(!p$lb) { vars.2.keep =c('mission','setno','totwgt','totno','size_class','spec')
                    ca = ca[,vars.2.keep]
                }

        if(p$length.based){
                  dp = de[which(de$spec %in% 2550),]
                  ids = paste(se$mission,se$setno,sep="~")
                  dp$ids = paste(dp$mission,dp$setno,sep="~")
                  dp = dp[which(dp$ids %in% ids),]
                  flf = p$size.class[1]:p$size.class[2]
                  dp$clen2 = ifelse(dp$flen %in% flf,dp$clen,0)

              if(p$by.sex){
                iii  = which(is.na(dp$fsex) )
                dp$fsex[iii] = 0
               dp$clen2 = ifelse(dp$fsex %in% p$sex, dp$clen2, 0)
              }
              if(any(!is.finite(dp$fwt))) {
                  io = which(!is.finite(dp$fwt))
                  lobLW1 <- function(row) {
                    lobLW(CL=row[1],sex=row[2])
                  }
                  dp$fwt[io] = apply(dp[io,c('flen','fsex')],1,lobLW1) 
              }
                  
                  dp$pb = dp$fwt * dp$clen
                  dp$pb1 = dp$fwt * dp$clen2

                  dpp = data.frame(mission=NA,setno=NA,size_class=NA,pn=NA,pw=NA)
                  if(nrow(dp)>0) {
                  dpp = aggregate(cbind(clen,clen2,pb,pb1)~mission+setno+size_class,data=dp,FUN=sum)
                  dpp$pn = dpp$clen2/dpp$clen
                  dpp$pw = dpp$pb1/dpp$pb
                  dpp = dpp[,c('mission','setno','size_class','pn','pw')]
                  }
                  ca1 = merge(ca,dpp,by=c('mission','setno','size_class'))
                  ca1$totwgt = ca1$totwgt * ca1$pw
                  ca1$totno = ca1$totno * ca1$pn
                  vars.2.keep =c('mission','setno','totwgt','totno','size_class','spec')
                  ca = ca1[,vars.2.keep]
            
                }
             
                           if(nrow(ca)>=1) {
                          ca = aggregate(cbind(totwgt,totno)~mission+setno,data=ca,FUN=sum)
                          sc = merge(se,ca,by=c('mission','setno'),all.x=T)
                          sc[,c('totwgt','totno')] = bio.utilities::na.zero(sc[,c('totwgt','totno')])
                          io = which(stra$strat %in% unique(sc$strat))
                          st = stra[io,c('strat','NH')]
                          st = st[order(st$strat),]
                          st$Strata = st$strat
                          spr = data.frame(Strata = strat, Pr = props)
                          st = merge(st,spr)
                          if(p$reweight.strata) st$NH = st$NH * st$Pr #weights the strata based on area in selected region
                          
                          if(exists('temperature',p)) {sc = sc[!is.na(sc$bottom_temperature),] ; sc$totno = sc$bottom_temperature; sc$totwgt = sc$bottom_temperature; sc$type <- 1 }
                          if(nrow(sc)>0){
                          st = Prepare.strata.file(st)
                          sc1= sc
                          if(yr>2019) sc = sc[which(sc$type %in% 1:5),]
                          if(yr<=2019) sc = sc[which(sc$type %in% 1),]
                          
                          sc = Prepare.strata.data(sc)
                          strata.files[[mp]]  = list(st,sc1)
                          
                  sW = Stratify(sc,st,sc$totwgt)
                  sN = Stratify(sc,st,sc$totno)
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
                  bsW = summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='Percentile')
                  bsN = summary(boot.strata(sN,method='BWR',nresamp=1000),ci.method='Percentile')
                  nt  = sum(sW$Nh)/1000
                }
                     if(exists('big.ci',p)) {
                    big.out[mp,] = c(yr,summary(boot.strata(sN,method='BWR',nresamp=1000),ci.method='BC',big.ci=T))
                  }

                out[mp,] = c(yr,ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]]/1000,bsW[[1]][1]*nt,bsW[[1]][2]*nt,
                ssN[[1]],ssN[[2]],bsN[[1]][1],bsN[[1]][2],ssN[[3]]/1000,bsN[[1]][1]*nt,bsN[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),round(sum(sc$totno)),ssN$gini,bsN[[2]][1],bsN[[2]][2],ssN$df.yst)
                print(out[mp,'yr'])
              } else {
                out[mp,] = c(yr,rep(0,22))
                print(out[mp,'yr'])
              }
            }
          }
        }
      }
           if(p$strata.efficiencies) {
                 return(list(effic.out,nopt.out))
              }
          if(exists('big.ci',p)) {
              return(big.out)
            }
          
  
              lle = 'all'
              lbs = 'not'
              if(p$length.based) lle = paste(p$size.class[1],p$size.class[2],sep="-")
              if(p$by.sex)      lbs = ifelse(p$sex==1,'male',ifelse(p$sex==2,'female','berried'))
              if(length(lbs)>1) lbs = paste(lbs[1],lbs[2],sep='&')
           
              fn = paste('stratified',p$series,p$area,pi,'length',lle,lbs,'sexed','rdata',sep=".")
              fn.st = paste('strata.files',p$series,p$area,pi,'length',lle,lbs,'sexed','rdata',sep=".")
              
              if(save) {
              print(fn)
              save(out,file=file.path(loc,fn))
              save(strata.files,file=file.path(loc,fn.st))
              }
             if(p$strata.files.return) return(strata.files)
             return(out)

     }
  }
