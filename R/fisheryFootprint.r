#' @export
fisheryFootprint <- function(x,res=0.1,var = 'CPUE',gini = T, propArea=NULL){
			fn = file.path(project.datadirectory('bio.lobster'),'figures')
			xr = bufferRange(x$X)
			yr = bufferRange(x$Y)
						
			grid<-makeGrid(x=seq(xr[1],xr[2],res), y=seq(yr[1],yr[2],res), projection="LL")
			yy = sort(unique(x$fishingYear))
			
			locData =findCells(x,grid)
			x = merge(x,locData,by='EID',all.x=T)
			if(var == 'CPUE') {
				x = completeFun(x,c('ADJCATCH','NUM_OF_TRAPS'))
				xy = aggregate(cbind(ADJCATCH,NUM_OF_TRAPS)~PID+SID+fishingYear,data=x,FUN=sum)
				xy$lL = xy$ADJCATCH / xy$NUM_OF_TRAPS
			   lab = 'CPUE'
			   xq = quantile(xy$lL,c(0.25,0.4,0.55,0.7,0.85,0.95))	
			   xq = round(xq*4)/4
			   xll = xq
			   ti = 'CPUE kg/TH'
			 	}
			if(var == 'LANDINGS') {
				
				x$ADJCATCH = x$ADJCATCH / 1000 #convt to tons
				xy = aggregate(ADJCATCH~PID+SID+fishingYear,data=x,FUN=sum)
				xy$lL = xy$ADJCATCH 
				lab = 'Landings'
			xq = quantile(xy$lL,c(0.25,0.4,0.55,0.7,0.85,0.95))	
			xq = round(xq*2)/2
			xll = xq
			ti = "Landings t"
			}		
			if(var == 'EFFORT') {
				x$NUM_OF_TRAPS = x$NUM_OF_TRAPS / 1000
				xy = aggregate(NUM_OF_TRAPS~PID+SID+fishingYear,data=x,FUN=sum)
				xy$lL = xy$NUM_OF_TRAPS
				lab = 'Effort'
				xq = quantile(xy$lL,c(0.25,0.4,0.55,0.7,0.85,0.95))	
				xq = round(xq*5)/5
				xll = xq
				ti = "Effort x000's TH"
			}		
			
			
			
				lbrks<-length(xq)
				cols<-c(brewer.pal(lbrks+1,'Reds'))
				giOut = list()
				piOut = list()
				m=0
		for(y in yy) {
					xx = xy[which(xy$fishingYear==y),]
					xx$Z = xx$lL
					xx<-as.PolyData(xx)
					
					xx<-makeProps(xx,xq,"col",cols)
					xx[which(xx$Z<xq[1]),'col'] <- cols[1]				
					xx[which(xx$Z>xq[length(xq)]),'col'] <- cols[length(cols)]				
			pdf(file.path(fn,paste('fisheryFootprint',lab,y,'pdf',sep=".")))
			LobsterMap(title=y,'41',labels='nn',addSummerStrata=F,poly.lst = list(grid,xx))
			
			contLegend('bottomright',bty='n',cex=0.8,lvls = xll,Cont.data = data.frame(col=cols),title=ti)
 			dev.off()
 			m=m+1
 	#add in gini index
			if(gini) {				
				xx = xx[which(is.finite(xx$Z)),]
				giOut[[m]] = c(giniFootprint(xx$Z),y)
				}
			if(!is.null(propArea)) {
					xx = xx[order(xx$lL),]
 					xx$prp = cumsum(xx$lL) / sum(xx$lL)
 					piOut[[m]] = c(which.min(abs(xx$prp - propArea)) / nrow(xx),y)
 				}
 			}
				gI = do.call(rbind,giOut)
				pI = do.call(rbind,piOut)
							
 			return(list(gI,pI))
 			}	