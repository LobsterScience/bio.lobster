#' @export
readGearSuccessRaw <- function(file){
		options(stringsAsFactors=F)	
lty=1:3
cBr21 = as.data.frame(do.call(rbind,list(
			lty = c(1,3),
			trip = c(4,12),
			set_no = c(13,16),
			GearCd_id = c(17,18),
			Gear_Group_No = c(19,22),
			Gear_Components_Grouped = c(23,26),
			Gear_Component_No = c(27,30),
			ConditionCD_ID = c(31,32),
			SpecCd_ID = c(39,42),
			BaitSourceCD_ID = c(43,44),
			SpeciesCd_Id = c(45,48),
			Total_Num_Kept  = c(49,52),
			Total_Num_Discard = c(53,56)
			)))

cBr22 = as.data.frame(do.call(rbind,list(
			lty = c(1,3),
			trip = c(4,12),
			set_no = c(13,16),
			GearCd_id = c(17,18),
			Gear_Group_No = c(19,22),
			Gear_Component_No = c(23,26),
			SpecCd_ID = c(27,30),
			MrphCd_Id = c(31,34),
			MrphVCd_id = c(35,36),
			NumberCaught = c(37,40)
			)))

 

		outSet = list(); ms=0
		outMorph = list(); mo=0
		
		f = readLines(file)
		n = length(f)
for(j in 1:n) {
		  tmp <- f[j]
  		   sw = substr(tmp,lty[1],lty[2])
  		   decodes = cBr21
  		   oo=c()
  		   if(sw == 22) decodes = cBr22; 
  		   for(i in 1:nrow(decodes)){
				  oo = c(oo,trimws(substr(tmp,decodes[i,1],decodes[i,2])))
  		   	}
			if(sw==22){mo=mo+1;outMorph[[mo]] = oo }	
			if(sw==21){ms=ms+1;outSet[[ms]] = oo }	
			
			}

			outM = as.data.frame(do.call(rbind,outMorph))
			outS = as.data.frame(do.call(rbind,outSet))	
			if(nrow(outM)>1) names(outM) = rownames(cBr22)
			if(nrow(outS)>1) names(outS) = rownames(cBr21)
			
			browser()			
			
			return(list(outM, outS))
}

##setwd('~/tmp/GearSuccess')
##fil = dir()[grep('.dat',dir())]
##outSet = list(); outMorph = list()
##for(i in 1:length(fil)){
##x = readGearSuccessRaw(fil[i])
##outMorph[[i]] = x[[1]]
##outSet[[i]] = x[[2]]
##}
##outMorph = as.data.frame(do.call(rbind, outMorph))
##outSet = as.data.frame(do.call(rbind, outSet))
##write.csv(outSet,'CompiledSetData.csv')
##write.csv(outMorph,'CompiledMorphData.csv')
