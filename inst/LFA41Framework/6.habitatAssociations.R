#habitat associations
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

#DFO RV Analysis

      p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$years.to.estimate = c(1970:2015)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = F #this subsets 
	  p$strata.files.return =T      
	  p$plot.name = 'habitatAssociationsDFOsummerbase.pdf'
     
     out = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)

 	figure.habitat.associations(out,p=p)

#restratified
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'LFA41'
      p$reweight.strata = T #this subsets 
	  p$strata.files.return =T      
	  p$plot.name = 'habitatAssociationsDFOsummerrestratified.pdf'
     
     out = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)

 	figure.habitat.associations(out,p=p)

#restratified adjacent
      p$define.by.polygons = T
      p$lobster.subunits=F
      p$area = 'adjacentLFA41'
      p$reweight.strata = T #this subsets 
	  p$strata.files.return =T      
	  p$plot.name = 'habitatAssociationsDFOsummeradjacentrestratified.pdf'
     
     out = dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)

 	figure.habitat.associations(out,p=p)


#dfo georges
     p$series =c('georges')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(1987:2015)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
	 p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'Georges.Canada'
      p$reweight.strata = F #this subsets 
    	p$strata.files.return =T      
	  p$plot.name = 'habitatAssociationsgeorges.pdf'
       
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
      
figure.habitat.associations(out,p=p)



#nefsc

   p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=T
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      

# Spring survey All stations including adjacent
                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'LFA41'
            	        p = make.list(list(yrs=p$years.to.estimate),Y=p)
  	  p$plot.name = 'habitatAssociationsNEFSCspringbase.pdf'
                    
                        out= nefsc.analysis(DS='stratified.estimates.redo',p=p)
				
				figure.habitat.associations(out,p=p)

#fall
					  p$season = 'fall'
				  	  p$plot.name = 'habitatAssociationsNEFSCfallbase.pdf'
                      out= nefsc.analysis(DS='stratified.estimates.redo',p=p)
				
				figure.habitat.associations(out,p=p)


#nefsc restratified

      p$reweight.strata = T #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=T
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      

# Spring survey All stations including adjacent
                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = T
                        p$lobster.subunits=F
                        p$area = 'LFA41'
            	        p = make.list(list(yrs=p$years.to.estimate),Y=p)
  	  p$plot.name = 'habitatAssociationsNEFSCspringrestratified.pdf'
                    
                        out= nefsc.analysis(DS='stratified.estimates.redo',p=p)
				
				figure.habitat.associations(out,p=p)

#fall
					  p$season = 'fall'
				  	  p$plot.name = 'habitatAssociationsNEFSCfallrestratified.pdf'
                      out= nefsc.analysis(DS='stratified.estimates.redo',p=p)
				
				figure.habitat.associations(out,p=p)


#nefsc restratified adjacent

    p$reweight.strata = T #this subsets 
      p$years.to.estimate = c(1969:2015)
      p$length.based = T
      p$size.class= c(50,300)
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=T
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      


                        p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = T
                        p$lobster.subunits=F
                        p$area = 'adjacentLFA41'
            	        p = make.list(list(yrs=p$years.to.estimate),Y=p)
  	  p$plot.name = 'habitatAssociationsNEFSCspringadjacentrestratified.pdf'
                    
                        out= nefsc.analysis(DS='stratified.estimates.redo',p=p)
				
				figure.habitat.associations(out,p=p)

#fall
					  p$season = 'fall'
				  	  p$plot.name = 'habitatAssociationsNEFSCadjacentfallrestratified.pdf'
                      out= nefsc.analysis(DS='stratified.estimates.redo',p=p)
				
				figure.habitat.associations(out,p=p)
