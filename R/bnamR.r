#' bnamR
#' Read in and reshape the bnam model output file. This currentely requires a specific format of bnam output (from Brickman Oct 2019). 
#' @param redo = if redo = T redo the reshape, else load in the previously reshaped file
#' @param bnam_location = location of the file (Brickman Oct 2019 structure)
#' @param outfile = where you want the output to live
#' @author Adam Cook
#' @examples
#' anc = bnam(redo = T)
#' @export

bnamR = function(redo=F , bnam_location = '~/Downloads/BNAM_Tbtm_1990_2018.mat', outfile = file.path(project.datadirectory('bio.lobster'),'data','BNAM','bnams.reshape.rds'),standard=T) {
	if(standard){
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
	} 
if(!standard){
		if(redo){
		require(R.matlab)
		x = dir(bnam_location,full.names=T)
		a = readMat(x[grep('mat',x)])
		locs = data.frame(X = c(a$Lons),Y = c(a$Lats))
		dir.create(file.path(bnam_location,'extracted'),showWarnings=F)
		d = file.path(bnam_location,'extracted')
		untar(x[grep('tar',x)],exdir = d)
		i = dir(d,full.names=T)
		ix = c(3,7,8,9,10,11,12,13,14,4,5,6)

		for(j in i){
			untar(j, exdir=d)
			unlink(j)	
			}
		
		i = dir(d,full.names=T)
		outSal = outTemp = locs
		for(j in 1:length(i)){ 
				w = dir(i[j], full.names=T)
				os = ot = locs
				for(p  in w){
					l = readMat(p)
					if(grepl('BT',p)){
					ot = cbind(ot,c(l[[1]]))
					names(ot)[ncol(ot)] = strsplit(tail(strsplit(p,"/")[[1]],n=1),"\\.mat")[[1]]				
					}
					if(grepl('BS',p)){
					os = cbind(os,c(l[[1]]))
					names(os)[ncol(os)] = strsplit(tail(strsplit(p,"/")[[1]],n=1),"\\.mat")[[1]]				
						}
					}
					outTemp = cbind(outTemp,ot[,ix])
					outSal = cbind(outSal,os[,ix])
					
						}
				timeS = seq(1959,2020,by=1/12 ); timeS = timeS[-length(timeS)]
				saveRDS(list(locsP = x,bTs = outTemp[,c(-1,-2)],bSs = outSal[,c(-1,-2)], timeS = timeS),file=outfile)

		}
	}
			return(readRDS(outfile))

}
