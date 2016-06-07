


# setting up base maps
require(PBSmapping)
	
		# GSHHG - A Global Self-consistent, Hierarchical, High-resolution Geography Database
		# download binary files from http://www.soest.hawaii.edu/pwessel/gshhg
		# unzip Downloads/gshhg-bin-2.3.4.zip -d ecomod_data/lobster/data/gshhs
		
		# East Coast Canada 
		limits <- list(x = c(360-74,360-47), y = c(40,52))

		# Low Resolution
		shorelineLR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "gshhs_l.b"),xlim=limits$x, limits$y, maxLevel=4)
		riversLR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_rivers_l.b"),xlim=limits$x, limits$y)
		bordersLR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_borders_l.b"),xlim=limits$x, limits$y) # no borders
		write.csv(shorelineLR,file.path( project.datadirectory("lobster"), "data", "gshhs", "shorelineLR.csv"),row.names=F)
		write.csv(riversLR,file.path( project.datadirectory("lobster"), "data", "gshhs", "riversLR.csv"),row.names=F)
		write.csv(bordersLR,file.path( project.datadirectory("lobster"), "data", "gshhs", "bordersLR.csv"),row.names=F)

		# Medium Resolution
		shorelineMR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "gshhs_i.b"),xlim=limits$x, limits$y, maxLevel=4)
		riversMR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_rivers_i.b"),xlim=limits$x, limits$y)
		bordersMR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_borders_i.b"),xlim=limits$x, limits$y) # no borders
		write.csv(shorelineMR,file.path( project.datadirectory("lobster"), "data", "gshhs", "shorelineMR.csv"),row.names=F)
		write.csv(riversMR,file.path( project.datadirectory("lobster"), "data", "gshhs", "riversMR.csv"),row.names=F)
		write.csv(bordersMR,file.path( project.datadirectory("lobster"), "data", "gshhs", "bordersMR.csv"),row.names=F)

		# High Resolution
		shorelineHR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "gshhs_h.b"),xlim=limits$x, limits$y, maxLevel=4)
		riversHR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_rivers_h.b"),xlim=limits$x, limits$y)
		bordersHR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_borders_h.b"),xlim=limits$x, limits$y) # no borders
		write.csv(shorelineHR,file.path( project.datadirectory("lobster"), "data", "gshhs", "shorelineHR.csv"),row.names=F)
		write.csv(riversHR,file.path( project.datadirectory("lobster"), "data", "gshhs", "riversHR.csv"),row.names=F)
		write.csv(bordersHR,file.path( project.datadirectory("lobster"), "data", "gshhs", "bordersHR.csv"),row.names=F)

		# Ultra Resolution
		shorelineUR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "gshhs_f.b"),xlim=limits$x, limits$y, maxLevel=4)
		riversUR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_rivers_f.b"),xlim=limits$x, limits$y)
		bordersUR <- importGSHHS (file.path( project.datadirectory("lobster"), "data", "gshhs", "wdb_borders_f.b"),xlim=limits$x, limits$y) # no borders
		write.csv(shorelineUR,file.path( project.datadirectory("lobster"), "data", "gshhs", "shorelineUR.csv"),row.names=F)
		write.csv(riversUR,file.path( project.datadirectory("lobster"), "data", "gshhs", "riversUR.csv"),row.names=F)
		write.csv(bordersUR,file.path( project.datadirectory("lobster"), "data", "gshhs", "bordersUR.csv"),row.names=F)

			
	# Bathymetry from topex	
	loadfunctions("lobster")
	createBathyLines()
	#createBathyLines(redo=F)
	# from ecomod_data/bathymetry/interpolated/canada.east.Z.interpolated.lonlat.grid.rdata
	createBathyLines(input.fp=file.path( project.datadirectory("lobster"), "data", "maps","bathy"), input.fn="bathy",redo=F,dneg=F)
	createBathyLines(input.fp=file.path( project.datadirectory("lobster"), "data", "maps","bathy"), input.fn="bathy",redo=F,dneg=F, interval=1,batch=10,range=c(1,100),save.by=100,output.fn="bathy1Poly")
