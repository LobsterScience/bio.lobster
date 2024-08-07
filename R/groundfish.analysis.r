#' @export
groundfish.analysis <- function(DS='stratified.estimates',out.dir = 'bio.lobster', p=p, ip=NULL) {
    loc = file.path( project.datadirectory(out.dir), "analysis" )

    dir.create( path=loc, recursive=T, showWarnings=F )
         if(p$series=='summer')  {mns = c('June','July','August')     ; strat = c(440:495)}
         if(p$series=='4vswcod') {mns = c('February','March','April'); strat = c(398:411)}
         if(p$series=='georges') {mns = c('February','March','April'); strat = c('5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8','5Z9')}
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
            a = a[grep(paste(c(min(p$strat),max(p$strat)),collapse='.'),a)]
            if(exists('strata.files.return',p)){
                 if(p$length.based) {
                     it = grep(paste(p$size.class,collapse="-"),a)
                       load(file.path(loc,a[it]))
                      } else {
                        load(file.path(loc,a))
                      }
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

if(DS %in% c('mean.wt.at.length')) {
  p$strata.files.return=T
  de = groundfish.db(DS='gsdet.odbc')
  de = de[which(de$spec %in% p$spec & de$flen %in% p$size.class[1]:p$size.class[2]),]
  if(exists('by.sex',p)) {
        if(p$by.sex) {
            de = subset(de,fsex %in% p$sex)
        }
    }
  de$id = paste(de$mission,de$setno,sep=".")
  aout= groundfish.analysis(DS='stratified.estimates.redo',p=p,out.dir= out.dir)
  a = NULL
  out = data.frame(yr = p$years.to.estimate, meanWt = NA, sampleSize = NA)
    for(i in 1:length(aout)) {
      #this is really gross coding, I am very sorry, brain not working, weighted mean per strat by totno, population weighted for annual total.
        a = aout[[c(i,2)]]
        a$id = paste(a$mission,a$setno,sep=".")
        d = de[which(de$id %in% a$id),]
     lp = dim(subset(d,!is.na(fwt)))[1]
     if(length(na.omit(d$fwt))>3){
      if(any(d$mission=='TEL2005605' & d$spec==23)) {l = which(d$fshno==69); d$fwt[l] <- NA}
          d = aggregate(fwt~id,data=d,FUN=mean)
        a = merge(a,d,by=c('id'),all.x=T)
        b = aggregate(totno~strat,data=a,FUN=sum)
        a = merge(a,b,by='strat')
        a$wt = a$totno.x/a$totno.y
        b = as.data.frame(sapply(split(a,a$strat),function(x) weighted.mean(x$fwt,w=x$wt)))
        b$strat = rownames(b)
        names(b) = c('fwt','strat')
        g = aggregate(totno.x~strat,data=a,FUN=mean)
        h = data.frame(strat=aout[[c(i,1,1)]],NH=aout[[c(i,1,2)]])
        b = merge(b,g,by='strat')
        b = merge(b,h,by='strat')
        b$wt = b$totno.x * b$NH
        b = weighted.mean(b$fwt,w=b$wt,na.rm=T)
        out[i,'meanWt']  = b
        out[i,'sampleSize']  = lp
      }
    }
return(out)
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


        set = groundfish.db(DS='gsinf.odbc')
        cas = groundfish.db(DS='gscat.odbc')
        stra = groundfish.db(DS='gsstratum')
        de = groundfish.db(DS='gsdet.odbc')

        stra$NH = as.numeric(stra$area)/0.011801
        ii = which(months(set$sdate) %in% mns & set$strat %in% strat & set$type %in% c(1,5))
        print('Both set types 1 and 5 are saved in data frame but only 1 is used for stratified')
        set = set[ii,]

        io = which(is.na(cas$totwgt) | cas$totwgt==0 & cas$totno>0)
        cas[io,'totwgt'] <- 1

        io = which(is.na(cas$sampwgt) & !is.na(cas$totwgt))
        cas[io,'sampwgt'] <- cas[io,'totwgt']
        strata.files = list()
     out = data.frame(yr=NA,w.yst=NA,w.yst.se=NA,w.ci.yst.l=NA,w.ci.yst.u=NA,w.Yst=NA,w.ci.Yst.l=NA,w.ci.Yst.u=NA,n.yst=NA,n.yst.se=NA,n.ci.yst.l=NA,n.ci.yst.u=NA,n.Yst=NA,n.ci.Yst.l=NA,n.ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithSpp=NA,NObsSPP = NA,gini = NA,gini.lo =NA, gini.hi=NA)
 
    mp=0
    np=1
    effic.out = data.frame(yr=NA,strat.effic.wt=NA,alloc.effic.wt=NA,strat.effic.n=NA,alloc.effic.n=NA)
    nopt.out =  list()
    for(iip in ip) {
            mp = mp+1
            v = p$runs[iip,"v"]
            if(iip==1) v0=v
            if(v0!=v) { # if this species loop is done save the file and reset data frame to continue with next spp
                    lle = 'all'
                    if(p$length.based & !p$by.sex) lle = paste(p$size_class[1],p$size_class[2],sep="-")
                    if(p$length.based & p$by.sex) lle = 'by.length.by.sex'

                    fn = paste('stratified',v0,p$series,'strata',min(strat),max(strat),'length',lle,'rdata',sep=".")
                    fn.st = paste('strata.files',v0,p$series,'strata',min(strat),max(strat),'length',lle,'rdata',sep=".")

                    save(out,file=file.path(loc,fn))
                    save(strata.files,file=file.path(loc,fn.st))

                    print(fn)
                    rm(out)
                    rm(strata.files)

                #    out = data.frame(yr=NA,sp=NA,w.yst=NA,w.yst.se=NA,w.ci.yst.l=NA,w.ci.yst.u=NA,w.Yst=NA,w.ci.Yst.l=NA,w.ci.Yst.u=NA,n.yst=NA,n.yst.se=NA, n.ci.yst.l=NA,n.ci.yst.u=NA,n.Yst=NA,n.ci.Yst.l=NA,n.ci.Yst.u=NA,dwao=NA)
                    out = data.frame(yr=NA,w.yst=NA,w.yst.se=NA,w.ci.yst.l=NA,w.ci.yst.u=NA,w.Yst=NA,w.ci.Yst.l=NA,w.ci.Yst.u=NA,n.yst=NA,n.yst.se=NA,n.ci.yst.l=NA,n.ci.yst.u=NA,n.Yst=NA,n.ci.Yst.l=NA,n.ci.Yst.u=NA,dwao=NA,Nsets=NA,NsetswithSpp=NA,NObsSPP = NA,gini = NA,gini.lo =NA, gini.hi=NA)
 
                    strata.files = list()
                    mp=1
                    np = np + 1
                  }
            vv = v0 = v
            yr = p$runs[iip,"yrs"]
            print ( p$runs[iip,] )
            if(p$functional.groups) vv = p$yy[[which(names(p$yy)==v0)]]
            iv = which(cas$spec %in% vv)
            iy = which(year(set$sdate) %in% yr)

                se = set[iy,]
                ca = cas[iv,]
                if(nrow(se)>1){
                  se$z = (se$dmin+se$dmax) / 2
                
                vars.2.keep = c('mission','slat','slong','setno','sdate','dist','strat','z','bottom_temperature','bottom_salinity','type')
                se = se[,vars.2.keep]
                se$slong = convert.dd.dddd(se$slong)
                se$slat = convert.dd.dddd(se$slat)
        p$lb = p$length.based

        if(p$by.sex & !p$length.based) {p$size.class=c(0,1000); p$length.based=T}

        if(!p$lb) { vars.2.keep =c('mission','setno','totwgt','totno','size_class','spec')
                    ca = ca[,vars.2.keep]
                }

        if(p$length.based){
                  dp = de[which(de$spec %in% v0),]
                  ids = paste(se$mission,se$setno,sep="~")
                  dp$ids = paste(dp$mission,dp$setno,sep="~")
                  dp = dp[which(dp$ids %in% ids),]
                  flf = p$size.class[1]:p$size.class[2]
                  dp$clen2 = ifelse(dp$flen %in% flf,dp$clen,0)
              if(p$by.sex) dp$clen2 = ifelse(dp$fsex %in% p$sex, dp$clen2, 0)

              if(any(!is.finite(dp$fwt))) {
                  io = which(!is.finite(dp$fwt))
                  fit = nls(fwt~a*flen^b,de[which(de$spec==v0 & is.finite(de$fwt)),],start=list(a=0.001,b=3.3))
                  ab = coef(fit)
                  dp$fwt[io] = ab[1]*dp$flen[io]^ab[2]
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
                  if(exists('temperature',p)) {sc = sc[!is.na(sc$bottom_temperature),] ; sc$totno = sc$bottom_temperature; sc$totwgt = sc$bottom_temperature }
                  
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
             #   out[mp,] = c(yr,v,ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]]/1000,bsW[[1]][1]*nt,bsW[[1]][2]*nt,
             #   ssN[[1]],ssN[[2]],bsN[[1]][1],bsN[[1]][2],ssN[[3]]/1000,bsN[[1]][1]*nt,bsN[[1]][2]*nt,ssW$dwao)
                 out[mp,] = c(yr,ssW[[1]],ssW[[2]],bsW[[1]][1],bsW[[1]][2],ssW[[3]]/1000,bsW[[1]][1]*nt,bsW[[1]][2]*nt,
                ssN[[1]],ssN[[2]],bsN[[1]][1],bsN[[1]][2],ssN[[3]]/1000,bsN[[1]][1]*nt,bsN[[1]][2]*nt,ssW$dwao,sum(sW[['nh']]),sum(sW[['nhws']]),round(sum(sc$totno)),ssN$gini,bsN[[2]][1],bsN[[2]][2])
            

                print(out[mp,'v'])
              }} else {
               # out[mp,] = c(yr,v,rep(0,15))
                  out[mp,] = c(yr,rep(0,22))
             
                print(out[mp,'v'])
              }
            }
          }
  if(p$strata.efficiencies) {
                 return(list(effic.out,nopt.out))
              }
              lle = 'all'
              if(p$length.based) lle = paste(p$size.class[1],p$size.class[2],sep="-")
              fn = paste('stratified',v0,p$series,'strata',min(strat),max(strat),'length',lle,'rdata',sep=".")
              fn.st = paste('strata.files',v0,p$series,'strata',min(strat),max(strat),'length',lle,'rdata',sep=".")
             print(fn)
              save(out,file=file.path(loc,fn))
              save(strata.files,file=file.path(loc,fn.st))
             if(p$strata.files.return) return(strata.files)
             return(out)

   }

  if(DS %in% c('ab','ab.redo')) {

        det = groundfish.db('gsdet.odbc',p=p)
        set = groundfish.db('gsinf.odbc',p=p)
        ii = which(months(set$sdate) %in% mns & set$strat %in% strat & set$type == 1)
        set = set[ii,]
        set$ids = paste(set$mission,set$setno,sep=".")
        det$ids = paste(det$mission,det$setno,sep=".")
        ii = which(det$ids %in% set$ids)
        det = det[ii,]
        m=0
      out = data.frame(sex = NA,yr = NA, species = NA, alpha=NA,N.Measured=NA,N.Outside.of.Polygon=NA,a=NA,b=NA,a.lower=NA,      a.upper=NA,b.lower=NA,b.upper=NA)
  for(iip in ip) {
       v0 = v = p$runs[iip,"v"]
            yr = p$runs[iip,"yrs"]
            print ( p$runs[iip,] )
            iv = which(det$spec==v0)
            iy = which(det$year %in% yr)
            vars.2.keep =c('fwt','flen','fsex','spec','year')
            df = det[intersect(iv,iy),vars.2.keep]
        if(p$by.sex) {
          for(i in 1:2){
            m=m+1
           ds = na.omit(df)
           ds = ds[which(ds$fsex==i),]
           out[m,] = ab.nls(ds=ds,p=p)
        }
      }
        if(!p$by.sex) {
           m=m+1
           ds = na.omit(df)
           ds$fsex = 'all'
           out[m,] = ab.nls(ds=ds,p=p)
        }
      }
   return(out)
  }
}
