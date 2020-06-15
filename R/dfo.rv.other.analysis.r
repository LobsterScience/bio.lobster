#' @title dfo.rv.other.analysis
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



dfo.rv.other.analysis <- function(DS='stratified.estimates', out.dir = 'bio.lobster', p=p, ip=NULL,save=T) {
    loc = file.path( project.datadirectory(out.dir), "other.analysis" )

    dir.create( path=loc, recursive=T, showWarnings=F )
          props = 1
         if(p$series=='summer')  {mns = c('June','July','August')     ; strat = c(440:495)}
         if(p$series=='georges') {mns = c('February','March','April'); strat = c('5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8','5Z9')}
         
         
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


        set = groundfish.db(DS='gsinf.odbc')
        cas = groundfish.db(DS='gscat.odbc')
        stra = groundfish.db(DS='gsstratum')
        de = groundfish.db(DS='gsdet.odbc')
        set$X = convert.dd.dddd(set$slong) *-1
        set$Y = convert.dd.dddd(set$slat)
      
        stra$NH = as.numeric(stra$area)/0.011801
        ii = which(months(set$sdate) %in% mns & set$strat %in% strat & set$type %in% c(1,5))
        print('Both set types 1 and 5 are saved in data frame but only 1 is used for stratified')
        set = set[ii,]

        io = which(is.na(cas$totwgt) | cas$totwgt==0 & cas$totno>0)
        cas[io,'totwgt'] <- 1
        
        io = which(is.na(cas$totno) & !is.na(cas$totwgt))
        cas[io,'totno'] = cas[io,'totwgt']/0.806 #mean weight of individual per tow taken from 1999 to 2015

        io = which(is.na(cas$sampwgt) & !is.na(cas$totwgt))
        cas[io,'sampwgt'] <- cas[io,'totwgt']
        strata.files = list()
    out = data.frame(yr=NA,w.yst=NA,w.yst.se=NA,w.ci.yst.l=NA,w.ci.yst.u=NA,w.Yst=NA,w.ci.Yst.l=NA,w.ci.Yst.u=NA,n.yst=NA,n.yst.se=NA,n.ci.yst.l=NA,n.ci.yst.u=NA,n.Yst=NA,n.ci.Yst.l=NA,n.ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithLobster=NA,ObsLobs = NA,gini = NA,gini.lo =NA, gini.hi=NA)
  big.out = matrix(NA,nrow=p$nruns,ncol=length(seq(0.01,0.99,0.01))+1)
   
    mp=0
    np=1
    effic.out = data.frame(yr=NA,strat.effic.wt=NA,alloc.effic.wt=NA,strat.effic.n=NA,alloc.effic.n=NA)
    nopt.out =  list()

    for(iip in ip) {
            mp = mp+1
            yr = p$runs[iip,"yrs"]
            print ( p$runs[iip,] )
            iy = which(year(set$sdate) %in% yr)
            iv = which(cas$spec==p$species)
            browser()
pi='base'
        
                              iz = which(set$strat %in% c(strat))
                
                se = set[intersect(iy,iz),]
                se$EID = 1:nrow(se)
                ca = cas[iv,]
                se$z = (se$dmin+se$dmax) / 2
                vars.2.keep = c('mission','X','Y','setno','sdate','dist','strat','z','bottom_temperature','bottom_salinity','type')
                se = se[,vars.2.keep]

        
        vars.2.keep =c('mission','setno','totwgt','totno','size_class','spec')
                    ca = ca[,vars.2.keep]

     
                      if(p$vessel.correction) {
                            ca$id = ca$mission
                  if(!exists('vessel.correction.fixed',p)) {
                            ca = correct.vessel(ca)
                            ca$totwgt = ca$totwgt * ca$cfvessel
                            ca$totno = ca$totno * ca$cfvessel
                            print('Totno and Totwgt are adjusted by Fannings Conversion Factors')
				}
                   if(exists('vessel.correction.fixed',p) & yr %in% 1970:1981) {
                              ca$totwgt = ca$totwgt * p$vessel.correction.fixed
                              ca$totno = ca$totno * p$vessel.correction.fixed
                              print(paste('Totno and Totwgt are adjusted by Conversion Factor of',p$vessel.correction.fixed))
                           } else {
                             print('Into Needler Years No Need for Vessel Correction')
                           }
                           }
             
                           if(nrow(ca)>=1) {
		                      ca = aggregate(cbind(totwgt,totno)~mission+setno,data=ca,FUN=sum)
                          sc = merge(se,ca,by=c('mission','setno'),all.x=T)
                          sc[,c('totwgt','totno')] = na.zero(sc[,c('totwgt','totno')])
                          sc$totno = sc$totno * 1.75 / sc$dist
                          sc$totwgt = sc$totwgt * 1.75 / sc$dist
                          io = which(stra$strat %in% unique(sc$strat))
                          st = stra[io,c('strat','NH')]
                          st = st[order(st$strat),]
                          st$Strata = st$strat
                          spr = data.frame(Strata = strat, Pr = props)
                          st = merge(st,spr)
                          if(p$reweight.strata) st$NH = st$NH * st$Pr #weights the strata based on area in selected region
                          
                          if(exists('temperature',p)) {sc = sc[!is.na(sc$bottom_temperature),] ; sc$totno = sc$bottom_temperature; sc$totwgt = sc$bottom_temperature }
                          if(nrow(sc)>0){
                          st = Prepare.strata.file(st)
                          sc1= sc
                          sc = sc[which(sc$type==1),]
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
                  bsW = summary(boot.strata(sW,method='BWR',nresamp=1000),ci.method='BC')
                  bsN = summary(boot.strata(sN,method='BWR',nresamp=1000),ci.method='BC')
                  nt  = sum(sW$Nh)/1000
                }
                 
                out[mp,] = c(yr,ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]]/1000,bsW[[1]][1]*nt,bsW[[1]][2]*nt,
                ssN[[1]],ssN[[2]],bsN[[1]][1],bsN[[1]][2],ssN[[3]]/1000,bsN[[1]][1]*nt,bsN[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),round(sum(sc$totno)),ssN$gini,bsN[[2]][1],bsN[[2]][2])
                print(out[mp,'yr'])
              } else {
                out[mp,] = c(yr,rep(0,22))
                print(out[mp,'yr'])
              }
            }
          }
        }
           if(p$strata.efficiencies) {
                 return(list(effic.out,nopt.out))
              }
          
  
              lle = 'all'
              lbs = 'not'
             
              fn = paste('stratified',p$series,p$species,p$area,pi,'length',lle,lbs,'sexed','rdata',sep=".")
              fn.st = paste('strata.files',p$series,p$species,p$area,pi,'length',lle,lbs,'sexed','rdata',sep=".")
              
              if(save) {
              print(fn)
              save(out,file=file.path(loc,fn))
              save(strata.files,file=file.path(loc,fn.st))
              }
             if(p$strata.files.return) return(strata.files)
             return(out)

   }

}
