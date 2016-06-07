

  # lobster habitat analysis
    
    loadfunctions( "groundfish" , functionname="load.groundfish.environment.r" )
    loadfunctions( "snowcrab" , functionname="initialise.local.environment.r" )

    p$init.lobster = loadfunctions( c("spacetime", "utility", "parallel", "habitat", "groundfish", "taxonomy", "snowcrab", "lobster" ) )

    dir.lob = file.path( project.datadirectory("lobster"), "habitat" )
    workdir = file.path( project.datadirectory("lobster"), "R" )
    setwd(workdir)
    
    p = list()
    p = spatial.parameters( p, "SSE" ) 
    p$threshold.quantile = 0.01  # quantile at which to consider zero-valued abundance from data with abundance > 0 or more than 1 individual caught
    p$bathy.DS = "SSE"

    # taxa codes
    gstaxa = taxonomy.db( "gstaxa" )  ## taxonomic codes
    lob.codes = gstaxa$spec[ grep ("lobster", gstaxa$namecom, ignore.case=T )  ]
    p$taxa.codes = lob.codes[ - which( lob.codes == 1315 ) ] # 1315 is lobster eggs

    refresh.data = F
    if (refresh.data) {
      lob = lobster.db( "lfa41.redo", p=p)
      sc = lobster.db( "snowcrab.redo", p=p)
      gf = lobster.db( "groundfish.redo", p=p)
      L = lobster.db( "complete.redo", p=p)
    } 

    L = lobster.db( "complete" )
   
   # ---- convert to planar coords and discretize to allow merging of habitat info ...
    L = lonlat2planar( L, proj.type=p$internal.projection )  # utm20, WGS84 (snowcrab geoid) 
    L$plon = grid.internal( L$plon, p$plons )
    L$plat = grid.internal( L$plat, p$plats )

    plot( plat ~ plon, L, type="n" )
    points( plat ~ plon, L[ which( L$source=="groundfish" & L$PA==1 ), ], col="orange", pch=20 )
    points( plat ~ plon, L[ which( L$source=="snowcrab" & L$PA==1 ), ], col="blue", pch=20 )
    points( plat ~ plon, L[ which( L$source=="lobster" & L$PA==1 ), ], col="red", pch=20 )


    # look up temperature , z, dZ , etc and habitat traits where missing



  #----------------------------
    # model using GAM
    
    outdir = file.path( dir.lob, "models" )
    dir.create(path=outdir, recursive=T, showWarnings=F)
    fn = file.path( outdir, "habitat.gam.full.rdata" )
   
    require(mgcv)
    require(arm)
     
    .M.gam = formula( Y ~  s( yr ) + s( t )  + s( tamp) + s( wmin) + s( plon, plat) + s( z) 
        + s( substrate.mean) + s( ddZ) +s( dZ)  ) 
      Q = gam( .M.gam, data=set, na.action="na.pass", family=binomial() )
      AIC (Q) # = 3367.65
      summary(Q)
      # p = predict( Q, set, type="response")
      # cor(p,set$Y)^2 # = 0.41
      save( Q, file=fn, compress=T )
    if (plotdata) plot(Q, all.terms=T, rug=T, jit=T, seWithMean=T, trans=invlogit, pers=T , scale=0)
   


    vclass = "lobster" 

    model.habitat( model.type="gam.full.redo", threshold.quantile=threshold.quantile, yearlevels=p$yearswithTdata, predictionYears=p$years.to.model, plotdata=F )
    model.abundance( model.type="gam.full.redo", threshold.quantile=threshold.quantile,  yearlevels=p$yearswithTdata, vclass=vclass, predictionYears=p$years.to.model, plotdata=F )
     
  
  #----------------------------
  # do spatial predictions using GAM
    p = make.list( list( y=p$years.to.model ), Y=p )
    parallel.run( predict.discretised.habitat, 
      DS="habitat.redo",   model.type="gam.full", vclass=vclass, pyears=p$yearswithTdata,  init.files=lobster.init ) 
      debug.plot = F
      if (debug.plot) {
        yr = 2008
        # predict.discretised.habitat( DS="habitat.redo",   model.type="gam.full", vclass=vclass, pyears=yr )  # ~ 15 min/1 yr
        x = predict.discretised.habitat(DS="habitat", model.type="gam.full", vclass=vclass, pyears=yr )
        x11(); levelplot( x$fit~ plon+plat, habitat.db( DS="baseline", bathy.DS=p$bathy.DS ), aspect="iso" )
      }
      
   

