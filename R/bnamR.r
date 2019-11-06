#' bnamR
#' Read in and reshape the bnam model output file. This currentely requires a specific format of bnam output (from Brickman Oct 2019). 
#' @param redo = if redo = T redo the reshape, else load in the previously reshaped file
#' @param bnam_location = location of the file (Brickman Oct 2019 structure)
#' @param outfile = where you want the output to live
#' @value list of three objects. locP = locations of bottom temperature data with the pertient model info. bTs = predicted bottom temperatures one row per location (rownames for matrix match the locP EID. timeS = time steps for each column of the bTs matrix.)
#' @author Adam Cook
#' @examples
#' anc = bnam(redo = T)
#' @export

bnamR = function(redo=F , bnam_location = '~/Downloads/BNAM_Tbtm_1990_2018.mat', outfile = file.path(project.datadirectory('bio.lobster'),'data','bnams.reshape.rds')) {
	if(redo){
		require(R.matlab)
		a = readMat(bnam_location)
	
		plot(a$nav.lon,a$nav.lat,pch=ifelse(a$land.mask==0,'','.'))

		locs = data.frame(X=c(a$nav.lon), Y=c(a$nav.lat),Depth=c(a$Bathy.depth), land=c(a$land.mask),EID = 1:length(a$nav.lon))
		g = names(a)
		gI = grep('Tbtm',g)
		m = 0
		bTs = list()
		for(i in gI){
			m=m+1
			out = c()
			for(j in 1:12){
					out = cbind(out,c(a[[i]][j,,]))
				}
				if(m==1)	out = cbind(1:length(a$nav.lon),out)
					out = na.omit(out)
					bTs[[m]] = out
					names(bTs[[m]]) = g[i]
			}

#bottom temperatures pruned to ocean only transformed to one location per row with full time series 
			bTs = do.call(cbind,bTs)
			ocean = unique(bTs[,1])
			locsP = subset(locs, EID %in% ocean)
			timeS = seq(as.numeric(a[['yr0']]), as.numeric(a[['yr1']]+11/12), by=1/12)
			locsP = locsP[order(locsP$EID),]
			bTs = bTs[order(bTs[,1]),]
			locsP$EID = 1:nrow(locsP)
			bTs[,1] = 1:nrow(bTs)
		saveRDS(list(locsP = locsP,bTs = bTs,timeS = timeS),file=outfile)
		}
			return(readRDS(outfile))
}
