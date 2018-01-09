# bayesian chage point analysis for defining reference points.



require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()

require(bcp)

	p = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')
	p$YR = as.numeric(substr(p$SYEAR,6,9))
	
	g = subset(g,YR>=1980 & YR<2017)
	p = subset(p,YR>=1980 & YR<2017)

      b = bcp(g$LFA27,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  

  	  b = bcp(g$LFA28,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  
  	  b = bcp(g$LFA29,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  

  	  b = bcp(g$LFA30,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  

  load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))

  