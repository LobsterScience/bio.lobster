#' @title groundfish.db.r
#' @description data extractions from groundfish database
#' @param \code{DS} :the selection of analysis, options include \code{stratified.estimates}
#' @param \code{p} : the parameter list which contains the specifics of the analysis at a minimum includes the season and area for analysis
#' @return saves or loads .rdata objects 
#' @examples
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

groundfish.db = function(  DS="gscat.odbc.redo", p=NULL, taxa="all", datayrs=NULL  ) {

  loc = file.path( project.datadirectory("bio.lobster"), "data","rvsurvey" )

  dir.create( path=loc, recursive=T, showWarnings=F )

if(grepl('odbc.redo', DS)) db.setup() #Chooses RODBC vs ROracle based on R version and installed packages. db.setup(RODBC=T) will force RODBC
  
  if (DS %in% c("odbc.redo") ) {

    # ODBC data dump of bio.groundfish tables
    groundfish.db( DS="gscat.odbc.redo", datayrs=datayrs )
    groundfish.db( DS="gsdet.odbc.redo", datayrs=datayrs )
    groundfish.db( DS="gsinf.odbc.redo", datayrs=datayrs )
    groundfish.db( DS='special.lobster.sampling.redo', datayrs=datayrs)
    #groundfish.db( DS="gshyd.profiles.odbc.redo", datayrs=datayrs )

  }

# ----------------------



  # --------------------



	if (DS %in% c( "gscat.odbc", "gscat.odbc.redo" ) ) {
	  fn.root =  file.path( project.datadirectory("bio.lobster"), "data","rvsurvey" ,"trawl", "gscat" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( is.null(DS) | DS=="gscat.odbc" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T )
				for ( fny in fl ) {
				load (fny)
        print(fny)
				out = rbind( out, gscat )
			}
			return (out)
    }

    #require(RODBC)
    #connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

		for ( YR in datayrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      # gscat = sqlQuery( connect,  paste(
      #        "select i.*, substr(mission,4,4) year " ,
      # "    from groundfish.gscat i " ,
      # "    where substr(MISSION,4,4)=", YR, ";"
      # ) )

			 gscat = connect.command(con,  paste(
			        "select i.*, substr(mission,4,4) year " ,
			 "    from groundfish.gscat i " ,
			 "    where substr(MISSION,4,4)=", YR, ""
			 ) )
			
      names(gscat) =  tolower( names(gscat) )
     # dontwant = c("length_type", "length_units", "weight_type",  "weight_units")
     # gscat = gscat[,which(!names(gscat)%in%dontwant)]
      print(fny)
      save(gscat, file=fny, compress=T)
			gc()  # garbage collection
			print(YR)
		}

    #odbcClose(connect)
    return (fn.root)

	}


  # --------------------



  if (DS %in% c("gscat", "gscat.redo"  ) ) {

    fn = file.path( loc,"gscat.rdata")

    if ( DS=="gscat" ) {
      load( fn )
      print('Not tow length corrected')
      return (gscat)
    }

    gscat = groundfish.db( DS="gscat.odbc" )
    gscat$year = NULL

    # remove data where species codes are ambiguous, or missing or non-living items
    xx = which( !is.finite( gscat$spec) )
    if (length(xx)>0) gscat = gscat[ -xx, ]


    min.number.observations.required = 3
    species.counts = as.data.frame( table( gscat$spec) )
    species.to.remove = as.numeric( as.character( species.counts[ which( species.counts$Freq < min.number.observations.required) , 1 ] ))

    ii = which( gscat$spec %in% species.to.remove )
    gscat = gscat[ -ii , ]
    gscat$id = paste(gscat$mission, gscat$setno, sep=".")
    gscat$id2 = paste(gscat$mission, gscat$setno, gscat$spec, sep=".")


    # filter out strange data
		ii = which( gscat$totwgt >= 9999 )  # default code for NAs --
    if (length(ii)>0) gscat$totwgt[ii] = NA

		ii = which( gscat$totwgt >= 5000 )  # upper limit of realistic kg/set
    if (length(ii)>0) gscat$totwgt[ii] = 5000

		jj = which( gscat$totwgt == 0 )
		if (length(jj)>0) gscat$totwgt[jj] = NA

		kk = which( gscat$totno == 0 )
    if (length(kk)>0) gscat$totno[kk] = NA

    ll = which( is.na(gscat$totno) & is.na(gscat$totwgt) )
    if (length(ll) > 0) gscat$totno[ ll ] = 1

    # as species codes have been altered, look for duplicates and update totals
    d = which(duplicated(gscat$id2))
    s = NULL
    for (i in d) {
      q = which(gscat$id2 == gscat$id2[i])
			gscat$totno[q[1]] = sum( gscat$totno[q], na.rm=T )
			gscat$totwgt[q[1]] = sum( gscat$totwgt[q], na.rm=T )
			gscat$sampwgt[q[1]] = sum( gscat$sampwgt[q], na.rm=T )
      s = c(s, q[2:length(q)])
    }
    if (length(s)>0) gscat = gscat[-s,]

    oo = which( duplicated( gscat$id2) )
    if ( length( oo )>0 ) {
      print( gscat[ oo , "id2"] )
      stop("Duplcated id2's in gscat"  )
    }

    mw = meansize.crude(Sp=gscat$spec, Tn=gscat$totno, Tw=gscat$totwgt )
    mw2 = meansize.direct()
    mw = merge(mw, mw2, by="spec", all=T, sort=T, suffixes=c(".crude", ".direct") )
    # directly determined mean size has greater reliability --- replace
    mm = which( is.finite(mw$meanweight.direct))
    mw$meanweight = mw$meanweight.crude
    mw$meanweight[mm] = mw$meanweight.direct[mm]
    mw = mw[which(is.finite(mw$meanweight)) ,]


    ii = which( is.na(gscat$totno) & gscat$totwgt >  0 )

    print( "Estimating catches from mean weight information... slow ~ 5 minutes")

    if (length(ii)>0) {
      # replace each number estimate with a best guess based upon average body weight in the historical record
      uu = unique( gscat$spec[ii] )
      for (u in uu ) {
        os =  which( mw$spec==u )
        if (length( os)==0 ) next()
        toreplace = intersect( ii, which( gscat$spec==u) )
        gscat$totno[toreplace] = gscat$totwgt[toreplace] / mw$meanweight[os]
      }
    }

    jj = which( gscat$totno >  0 & is.na(gscat$totwgt) )
    if (length(jj)>0) {
      # replace each number estimate with a best guess based upon average body weight in the historical record
      uu = unique( gscat$spec[jj] )
      for (u in uu ) {
        os =  which( mw$spec==u )
        if (length( os)==0 ) next()
        toreplace = intersect( jj, which( gscat$spec==u) )
        gscat$totwgt[toreplace] = gscat$totno[toreplace] * mw$meanweight[os]
      }
    }

    gscat = gscat[, c("id", "id2", "spec", "totwgt", "totno", "sampwgt" )] # kg, no/set

    save(gscat, file=fn, compress=T)
    return( fn )
  }

	if (DS %in% c( "gsdet.odbc", "gsdet.odbc.redo" ) ) {
    fn.root =  file.path( project.datadirectory("bio.lobster"), "data","rvsurvey" ,"trawl", "gsdet" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( DS=="gsdet.odbc" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  )
				for ( fny in fl ) {
				load (fny)
				out = rbind( out, gsdet )
			}
			return (out)
    }

    # require(RODBC)
    # connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

		for ( YR in datayrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      gsdet = connect.command( con,  paste(
      "select i.*, substr(mission,4,4) year" ,
      "    from groundfish.gsdet i " ,
      "    where substr(mission,4,4)=", YR, ""
      ) )
      names(gsdet) =  tolower( names(gsdet) )
      gsdet$mission = as.character( gsdet$mission )
      save(gsdet, file=fny, compress=T)
      print(fny)
			gc()  # garbage collection
			print(YR)
		}
    #odbcClose(connect)

    return (fn.root)

	}

  # ----------------------

  if (DS %in% c("gsdet", "gsdet.redo") ) {

  # --------- codes ----------------
  # sex: 0=?, 1=male, 2=female,  3=?
  # mat: 0=observed but undetermined, 1=imm, 2=ripening(1), 3=ripening(2), 4=ripe(mature),
  #      5=spawning(running), 6=spent, 7=recovering, 8=resting
  # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
  #      4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
  #      6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
  # --------- codes ----------------


    fn = file.path( loc,"gsdet.rdata")

    if ( DS=="gsdet" ) {
      load( fn )
      return (gsdet)
    }

    gsdet = groundfish.db( DS="gsdet.odbc" )
    gsdet$year = NULL

    oo = which(!is.finite(gsdet$spec) )
    if (length(oo)>0) gsdet = gsdet[-oo,]

    # remove data where species codes are ambiguous, or missing or non-living items
   
    gsdet$id = paste(gsdet$mission, gsdet$setno, sep=".")
    gsdet$id2 = paste(gsdet$mission, gsdet$setno, gsdet$spec, sep=".")
    gsdet = gsdet[, c("id", "id2", "spec", "fshno", "fsex", "fmat", "flen", "fwt", "age") ]
    names(gsdet)[which(names(gsdet)=="fsex")] = "sex"
    names(gsdet)[which(names(gsdet)=="fmat")] = "mat"
    names(gsdet)[which(names(gsdet)=="flen")] = "len"  # cm
    names(gsdet)[which(names(gsdet)=="fwt")]  = "mass" # g
    save(gsdet, file=fn, compress=T)

    return( fn )
  }


  # ----------------------


	if (DS %in% c( "gsinf.odbc", "gsinf.odbc.redo" ) ) {

    fn.root =  file.path( project.datadirectory("bio.lobster"), "data","rvsurvey" ,"trawl", "gsinf" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( is.null(DS) | DS=="gsinf.odbc" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  )
				for ( fny in fl ) {
        load (fny)
        out = rbind( out, gsinf )
			}
			return (out)
    }

    # require(RODBC)
    # connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

		for ( YR in datayrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      # gsinf = sqlQuery( connect,  paste(
      # "select * from groundfish.gsinf where EXTRACT(YEAR from SDATE) = ", YR, ";"
      # ) )
			
			gsinf = connect.command(con,  paste(
			"select * from groundfish.gsinf where EXTRACT(YEAR from SDATE) = ", YR, ""
			 ) )
			
      names(gsinf) =  tolower( names(gsinf) )
      save(gsinf, file=fny, compress=T)
      print(fny)
			gc()  # garbage collection
			print(YR)
		}

    #odbcClose(connect)
    return (fn.root)

	}



# ----------------------


  if (DS %in% c("gsinf", "gsinf.redo" ) ) {
    fn = file.path( loc, "gsinf.rdata")

    if ( DS=="gsinf" ) {
      load( fn )
      return (gsinf)
    }

    gsinf = groundfish.db( DS="gsinf.odbc" )
    names(gsinf)[which(names(gsinf)=="type")] = "settype"

      # fix some time values that have lost the zeros due to numeric conversion
    gsinf$time = as.character(gsinf$time)

    tz.odbc = "America/Halifax"  ## need to verify if this is correct
    tz.groundfish = "UTC"

    # by default it should be the correct timezone ("localtime") , but just in case
    tz( gsinf$sdate) = tz.odbc
    gsinf$sdate = with_tz( gsinf$sdate, tz.groundfish )

    gsinf$edate = gsinf$etime
    tz( gsinf$edate) = tz.odbc
    gsinf$edate = with_tz( gsinf$edate, tz.groundfish )


    # fix sdate - edate inconsistencies .. assuming sdate is correct
    gsinf$timediff.gsinf = gsinf$edate - gsinf$sdate
    oo = which( abs( gsinf$timediff.gsinf)  > dhours( 4 ) )
    if (length(oo)>0) {
      print( "Time stamps sdate and etime (renamed as edate) are severely off (more than 4 hrs):" )
      print( gsinf[oo,] )
      if (FALSE) {
        hist( as.numeric(  gsinf$timediff.gsinf[-oo]), breaks=200 )
        abline (v=30*60, col="red")  # expected value of 30 min
        abline (v=90*60, col="red")  # after 90 min
        abline (v=150*60, col="red")  # after 150 min
      }
    }
    uu = which( gsinf$timediff.gsinf < 0 ) # when tow end is before start
    gsinf$edate[uu]  = NA  # set these to NA until they can be corrected manually
    gsinf$timediff.gsinf[uu] =NA
    print( "Time stamps sdate and etime (renamed as edate) are severely off: edate is before sdate:" )
    print( gsinf[uu,] )

    if (FALSE)  hist( as.numeric(  gsinf$timediff.gsinf), breaks=200 )

    uu = which( gsinf$timediff.gsinf > dminutes(50) & gsinf$timediff.gsinf < dminutes(50+60) ) # assuming 50 min is a max tow length
    if (length(uu)>0) {
      gsinf$edate[uu] = gsinf$edate[uu] - dhours(1) ### this is assuming sdate is correct ... which might not be the case
      if (FALSE) {
        hist( as.numeric(  gsinf$timediff.gsinf[-oo]), breaks=200 )
      }
    }
    gsinf$timediff.gsinf = gsinf$edate - gsinf$sdate
    uu = which( gsinf$timediff.gsinf > dminutes(50) ) # assuming 50 min is a max tow length
    gsinf$edate[uu]  = NA  # set these to NA untile they can be corrected manually
    gsinf$timediff.gsinf[uu] =NA
      if (FALSE) {
        hist( as.numeric(  gsinf$timediff.gsinf), breaks=200 )
        abline (v=30*60, col="red")  # expected value of 30 min
        abline (v=90*60, col="red")  # after 90 min
        abline (v=150*60, col="red")  # after 150 min
      }

    gsinf$yr = lubridate::year( gsinf$sdate)

    gsinf$mission = as.character( gsinf$mission )
    gsinf$strat = as.character(gsinf$strat)
    gsinf$strat[ which(gsinf$strat=="") ] = "NA"
    gsinf$id = paste(gsinf$mission, gsinf$setno, sep=".")
    d = which(duplicated(gsinf$id))
    if (!is.null(d)) write("error: duplicates found in gsinf")

    gsinf$lat = gsinf$slat/100
    gsinf$lon = gsinf$slong/100
    gsinf$lat.end = gsinf$elat/100
    gsinf$lon.end = gsinf$elong/100

    if (mean(gsinf$lon,na.rm=T) >0 ) gsinf$lon = - gsinf$lon  # make sure form is correct
    if (mean(gsinf$lon.end,na.rm=T) >0 ) gsinf$lon.end = - gsinf$lon.end  # make sure form is correct

    gsinf = convert.degmin2degdec(gsinf, vnames=c("lon", "lat") )
    gsinf = convert.degmin2degdec(gsinf, vnames=c("lon.end", "lat.end") )

    gsinf$dist_km = gsinf$dist * 1.852  # nautical mile to km
    gsinf$dist_pos = geosphere::distGeo( gsinf[, c("lon","lat")], gsinf[, c("lon.end", "lat.end")])/1000

    ii = which( abs( gsinf$dist_km) > 10 ) # 10 km is safely too extreme
    if (length(ii)> 0) {
      gsinf$dist_km[ii] =  gsinf$dist_pos[ii]
    }

    ii = which( abs( gsinf$dist_pos) > 10 ) # 10 km is safely too extreme
    if (length(ii)> 0) {
      gsinf$dist_pos[ii] = gsinf$dist_km[ii]
      # assuming end positions are incorrect. This may not be a correct assumption!
      gsinf$lon.end[ii] = NA
      gsinf$lat.end[ii] = NA
    }


  ## !! GPS position-based distances do not always match the distance recorded
  ## plot( dist_pos ~ dist_km, gsinf, ylim=c(0,60))

    gsinf$cftow = 1.75/gsinf$dist  # not used
    ft2m = 0.3048
    m2km = 1/1000
    nmi2mi = 1.1507794
    mi2ft = 5280
    gsinf$sakm2 = (41 * ft2m * m2km ) * ( gsinf$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
			oo = which( !is.finite(gsinf$sakm2 ))
				gsinf$sakm2[oo] = median (gsinf$sakm2, na.rm=T)
			pp = which( gsinf$sakm2 > 0.09 )
				gsinf$sakm2[pp] = median (gsinf$sakm2, na.rm=T)
    gsinf$bottom_depth = rowMeans( gsinf[, c("dmax", "depth" )], na.rm = TRUE )  * 1.8288  # convert from fathoms to meters
    ii = which( gsinf$bottom_depth < 10 | !is.finite(gsinf$bottom_depth)  )  # error
    gsinf$bottom_depth[ii] = NA
		gsinf = gsinf[, c("id", "yr", "sdate", "edate", "time", "strat", "area", "speed", "dist_km", "dist_pos",
                      "cftow", "sakm2", "settype", "gear", "geardesc", "lon", "lat", "lon.end", "lat.end",
                      "surface_temperature","bottom_temperature","bottom_salinity", "bottom_depth")]

    save(gsinf, file=fn, compress=T)
    return(fn)
  }


# -------------


	if (DS %in% c( "gshyd.profiles.odbc" , "gshyd.profiles.odbc.redo" ) ) {

    fn.root =  file.path( project.datadirectory("bio.lobster"), "data","rvsurvey" ,"trawl", "gshyd" )
		dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

		out = NULL
    if ( is.null(DS) | DS=="gshyd.profiles.odbc" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  )
				for ( fny in fl ) {
				load (fny)
				out = rbind( out, gshyd )
			}
			return (out)
    }

    # require(RODBC)
    # connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

		for ( YR in datayrs ) {
			fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      # gshyd = sqlQuery( connect,  paste(
      # "select i.*, j.YEAR " ,
      # "    from groundfish.gshyd i, groundfish.gsmissions j " ,
      # "    where i.MISSION(+)=j.MISSION " ,
      # "    and YEAR=", YR, ";"
      # ) )
      
      gshyd = connect.command(con,  paste(
        "select i.*, j.YEAR " ,
        "    from groundfish.gshyd i, groundfish.gsmissions j " ,
        "    where i.MISSION(+)=j.MISSION " ,
        "    and YEAR=", YR, ";"
      ) )
      
      names(gshyd) =  tolower( names(gshyd) )
      if(all(is.na(gshyd$mission))) {
      	#if gshyd is not loaded and the odf files are obtained AMC
	        fy <- file.path(project.datadirectory("bio.temperature"), "data", "archive", "ctd",YR)
	        o <- compileODF(path=fy)
	        gshyd <- makeGSHYD(o)
      }
      gshyd$mission = as.character( gshyd$mission )
      save(gshyd, file=fny, compress=T)
      print(fny)
			gc()  # garbage collection
			print(YR)
		}
		#odbcClose(connect)

    return ( fn.root )

	}

# ----------------------



  if (DS %in% c("gshyd.profiles", "gshyd.profiles.redo" ) ) {
    # full profiles
    fn = file.path( loc,"gshyd.profiles.rdata")
    if ( DS=="gshyd.profiles" ) {
      load( fn )
      return (gshyd)
    }

    gshyd = groundfish.db( DS="gshyd.profiles.odbc" )
    gshyd$id = paste(gshyd$mission, gshyd$setno, sep=".")
    gshyd = gshyd[, c("id", "sdepth", "temp", "sal", "oxyml" )]
    save(gshyd, file=fn, compress=T)
    return( fn )
  }


# ----------------------



  if (DS %in% c("gshyd", "gshyd.redo") ) {
    # hydrographic info at deepest point
    fn = file.path( loc,"gshyd.rdata")
    if ( DS=="gshyd" ) {
      load( fn )
      return (gshyd)
    }
    gshyd = groundfish.db( DS="gshyd.profiles" )
    nr = nrow( gshyd)

    # candidate depth estimates from profiles
    deepest = NULL
    t = which( is.finite(gshyd$sdepth) )
    id = unique(gshyd$id)
    for (i in id) {
      q = intersect( which( gshyd$id==i), t )
      r = which.max( gshyd$sdepth[q] )
      deepest = c(deepest, q[r])
    }
    gshyd = gshyd[deepest,]
    oo = which( duplicated( gshyd$id ) )
    if (length(oo) > 0) stop( "Duplicated data in GSHYD" )

    gsinf = groundfish.db( "gsinf" )
    gsinf = gsinf[, c("id", "bottom_temperature", "bottom_salinity", "bottom_depth" ) ]
    gshyd = merge( gshyd, gsinf, by="id", all.x=T, all.y=F, sort=F )

    ## bottom_depth is a profile-independent estimate .. asuming it has higher data quality
    ii = which(!is.finite( gshyd$bottom_depth ))
    if (length(ii)>0) gshyd$bottom_depth[ii] = gshyd$sdepth[ii]
    gshyd$sdepth = gshyd$bottom_depth        #overwrite
    ii = which( gshyd$sdepth < 10 )
    if (length(ii)>0) gshyd$sdepth[ii] = NA

    ii = which( is.na( gshyd$temp) )
    if (length(ii)>0) gshyd$temp[ii] =  gshyd$bottom_temperature[ii]

    jj = which( is.na( gshyd$sal) )
    if (length(jj)>0) gshyd$sal[jj] =  gshyd$bottom_salinity[jj]
    gshyd$sal[gshyd$sal<5 ] = NA

    gshyd$bottom_depth = NULL
    gshyd$bottom_temperature = NULL
    gshyd$bottom_salinity = NULL


    save(gshyd, file=fn, compress=T)
    return( fn )
  }

# ----------------------



  if (DS %in% c("gshyd.georef", "gshyd.georef.redo") ) {
    # hydrographic info georeferenced
    fn = file.path( loc,"gshyd.georef.rdata")
    if ( DS=="gshyd.georef" ) {
      load( fn )
      return (gshyd)
    }
    gsinf = groundfish.db( "gsinf" )
    gsinf$timestamp = gsinf$sdate
    gsinf$yr = lubridate::year( gsinf$timestamp)
    gsinf$longitude = gsinf$lon
    gsinf$latitude = gsinf$lat
    gsinf = gsinf[ , c( "id", "lon", "lat", "yr", "timestamp" ) ]
    gshyd = groundfish.db( "gshyd.profiles" )
    gshyd = merge( gshyd, gsinf, by="id", all.x=T, all.y=F, sort=F )
    gshyd$sal[gshyd$sal<5]=NA
    save(gshyd, file=fn, compress=T)
    return( fn )
  }


  # ----------------------


  if (DS %in% c("gsstratum", "gsstratum.obdc.redo") ) {
    fn = file.path( loc,"gsstratum.rdata")
    if ( DS=="gsstratum" ) {
      load( fn )
      return (gsstratum)
    }
    # require(RODBC)
    # connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user,
    #     pwd=oracle.personal.password, believeNRows=F)
    #gsstratum =  sqlQuery(connect, "select * from groundfish.gsstratum", as.is=T)
    gsstratum =  connect.command(con, "select * from groundfish.gsstratum", as.is=T)
    #odbcClose(connect)
    names(gsstratum) =  tolower( names(gsstratum) )
    save(gsstratum, file=fn, compress=T)
    print(fn)
    return( fn )
  }


  # ----------------------


  if (DS %in% c("gsgear", "gsgear.odbc.redo") ) {
    fn = file.path( loc,"gsgear.rdata")
    if ( DS=="gsgear" ) {
      load( fn )
      return (gsgear)
    }
    #require(RODBC)
    #connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user,
     #   pwd=oracle.personal.password, believeNRows=F)
    #gsgear =  sqlQuery(connect, "select * from groundfish.gsgear", as.is=T)
    gsgear =  connect.command(con, "select * from groundfish.gsgear", as.is=T)
    #odbcClose(connect)
    names(gsgear) =  tolower( names(gsgear) )
    save(gsgear, file=fn, compress=T)
    print(fn)
    return( fn )
  }



    if (DS %in% c("special.lobster.sampling.redo", "special.lobster.sampling") ) {

    fn = file.path( loc, "lobster.special.sampling.rdata")
    if ( DS=="special.lobster.sampling" ) {
      load( fn )
      return ( set )
    }

      #    require(RODBC)
      #connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      # set =  sqlQuery(connect, " select G.MISSION,G.SETNO,G.SPEC,G.SIZE_CLASS,G.SPECIMEN_ID,G.FLEN,G.FWT, G.FSEX,  G.CLEN, 
      #                             max(case when key= 'Spermatophore Presence' then value else NULL END) Sperm_Plug,
      #                             max(case when key= 'Abdominal Width' then value else NULL END) Ab_width,
      #                             max(case when key= 'Egg Stage' then value else NULL END) Egg_St,
      #                             max(case when key= 'Clutch Fullness Rate' then value else NULL END) Clutch_Full
      #                             from
      #                                 (select mission, setno, spec, size_class, specimen_id, flen, fwt, fsex, fmat, fshno, agmat, remarks, age, clen from groundfish.gsdet) G,
      #                                 (select mission, spec, specimen_id, lv1_observation key, data_value value  from groundfish.gs_lv1_observations
      #                                     where spec=2550) FC
      #                                 where 
      #                                   G.mission = FC.mission (+) and
      #                                   G.spec = FC.spec and
      #                                   G.specimen_id = FC.specimen_id (+)
      #                                     group by G.MISSION,G.SETNO,G.SPEC,G.SIZE_CLASS,G.SPECIMEN_ID,G.FLEN,G.FWT, G.FSEX,  G.CLEN;", as.is=T)
      # 
      set =  connect.command(con, "select G.MISSION,G.SETNO,G.SPEC,G.SIZE_CLASS,G.SPECIMEN_ID,G.FLEN,G.FWT, G.FSEX,  G.CLEN, 
                                  max(case when key= 'Spermatophore Presence' then value else NULL END) Sperm_Plug,
                                  max(case when key= 'Abdominal Width' then value else NULL END) Ab_width,
                                  max(case when key= 'Egg Stage' then value else NULL END) Egg_St,
                                  max(case when key= 'Clutch Fullness Rate' then value else NULL END) Clutch_Full
                                  from
                                      (select mission, setno, spec, size_class, specimen_id, flen, fwt, fsex, fmat, fshno, agmat, remarks, age, clen from groundfish.gsdet) G,
                                      (select mission, spec, specimen_id, lv1_observation key, data_value value  from groundfish.gs_lv1_observations
                                          where spec=2550) FC
                                      where 
                                        G.mission = FC.mission (+) and
                                        G.spec = FC.spec and
                                        G.specimen_id = FC.specimen_id (+)
                                          group by G.MISSION,G.SETNO,G.SPEC,G.SIZE_CLASS,G.SPECIMEN_ID,G.FLEN,G.FWT, G.FSEX,  G.CLEN", as.is=T)
                                #odbcClose(connect)
                                set = toNums(set,2:13)
                                save ( set, file=fn, compress=F )
      }
    }


