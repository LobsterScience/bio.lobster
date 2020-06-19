	#' @export
	indicatorAttributesMatrixPlot <-  function(x=nn,groupings='default',attribute=c('ecosystemIndicators')) {
			syms = c()
			if(attribute=='ecosystemIndicators'){
				if(groupings=='default'){
options(stringsAsFactors=F)
					groupings = data.frame(nam = c('ResourcePotential','EcosystemStrFunc','EcosystemStabRes','Biodiversity','FishingPressure'), 
					sym = c(intToUtf8(9698),  intToUtf8(9673), intToUtf8(9651),intToUtf8(10055),intToUtf8(9645)))#c(intToUtf8(9924),intToUtf8(9962),intToUtf8(9982),intToUtf8(9752)))

				}
					for(i in 1:length(x)){
					if(x[i] %in% c('MeanTrophicLevel.L')){
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'ResourcePotential')],groupings$sym[which(groupings$nam == 'FishingPressure')]))
					}
					if(x[i] %in% c('BiomassSkates','BiomassFlatfish')) {
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'ResourcePotential')],groupings$sym[which(groupings$nam == 'EcosystemStrFunc')]))		
					}
					if(x[i] %in% c('BiomassInvertebrates')) {
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'ResourcePotential')],groupings$sym[which(groupings$nam == 'EcosystemStrFunc')],groupings$sym[which(groupings$nam == 'FishingPressure')]))		

					}
	
					if(x[i] %in% c('MeanLifespan','InverseCVBiomass','BiomassTL2','Intrinsicvulnerabilityindex.L')) {
							syms = c(syms,groupings$sym[which(groupings$nam == 'EcosystemStabRes')])		
					}					
					if(x[i] %in% c('BiomassGadoids')) {
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'ResourcePotential')],groupings$sym[which(groupings$nam == 'EcosystemStabRes')]))	
					}

					if(x[i] %in% c('Heips','MargalefRichness')) {
							syms = c(syms,groupings$sym[which(groupings$nam == 'Biodiversity')])		
					}
					if(x[i] %in% c('CommunityCondition','BTGPiscivore','MeanLengthAbundance','CCPiscivore','CCZoopiscivore','CCMediumBenthivore','CCLargeBenthivore','MeanTrophicLevel','BTGZoopiscivore')) {
							syms = c(syms,groupings$sym[which(groupings$nam == 'EcosystemStrFunc')])		
					}
					if(x[i] %in% c('LargeFishIndicator')) {
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'EcosystemStrFunc')],groupings$sym[which(groupings$nam == 'EcosystemStabRes')]))		
					}
					if(x[i] %in% c('Biomass')) {
							syms = c(syms,paste(groupings$sym[which(groupings$nam == 'EcosystemStrFunc')],groupings$sym[which(groupings$nam == 'EcosystemStabRes')],groupings$sym[which(groupings$nam == 'ResourcePotential')]))		
						}
					if(x[i] %in% c('DiversityTargetSpp.L','FPClupeids.L','LLargePelagic.L','FishingPressure.L','LSkates.L','MarineTrophicIndex.L','Landings.L','LFlatfish.L')) {
							syms = c(syms,groupings$sym[which(groupings$nam == 'FishingPressure')])		
					}
				}
			}
		return(syms)
	} 

