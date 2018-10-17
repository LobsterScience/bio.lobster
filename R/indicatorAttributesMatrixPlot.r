	#' @export
	indicatorAttributesMatrixPlot <-  function(x=nn,groupings='default',attribute=c('ecosystemIndicators')) {

			syms = c()
			if(attribute=='ecosystemIndicators'){
				if(groupings=='default'){
					groupings = data.frame(nam = c('ResourcePotential','EcosystemStrFunc','EcosystemStabRes','Biodiversity'), sym = c(intToUtf8(9947), intToUtf8(9956), intToUtf8(9711), intToUtf8(9632)))

				}
					for(i in 1:length(x)){
					if(x[i] %in% c('MeanTrophicLevel.L')){
							syms = c(syms,groupings$sym[which(groupings$nam == 'ResourcePotential')])
					}
					if(x[i] %in% c('BiomassSkates','BiomassFlatfish','BiomassInvertebrates')) {
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'ResourcePotential')],groupings$sym[which(groupings$nam == 'EcosystemStrFunc')]))		
					}
					if(x[i] %in% c('MeanLifespan','InverseCVBiomass','BiomassTL2','Intrinsicvulnerabilityindex.L','BiomassGadoids')) {
							syms = c(syms,groupings$sym[which(groupings$nam == 'EcosystemStabRes')])		
					}
					if(x[i] %in% c('Heips','MargalefRichness')) {
							syms = c(syms,groupings$sym[which(groupings$nam == 'Biodiversity')])		
					}
					if(x[i] %in% c('CommunityCondition','BTGPiscivore','MeanLengthAbundance','CCPiscivore','CCZoopiscivore','CCMediumBenthivore','CCLargeBenthivore','MeanTrophicLevel','BTGZoopiscivore')) {
							syms = c(syms,groupings$sym[which(groupings$nam == 'EcosystemStrFunc')])		
					}
					if(x[i] %in% c('Biomass','LargeFishIndicator')) {
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'EcosystemStrFunc')],groupings$sym[which(groupings$nam == 'EcosystemStabRes')]))		
					}
				}
			}
		return(syms)
	} 

