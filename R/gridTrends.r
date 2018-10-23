#' @export
gridTrends = function(data,grids,yrs,variable="CPUE",fun=median){
	
	grid.lst=as.list(	rep(NA,length(grids)))

	for(i in 1:length(yrs)){
		griddata = lobGridPlot(subset(logsInSeason,SYEAR==yrs[i],c("LFA","GRID_NUM",variable)),FUN=fun)$pdata

		for(j in 1:length(grids)){
			grid.lst[[j]][i] = griddata$Z[griddata$SID==grids[j]]
		}

	}
	return(grid.lst)

}