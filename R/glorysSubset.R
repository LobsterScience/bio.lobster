#' @export
glorysSubset <- function(glorysfile=file.path(project.datadirectory('bio.lobster'),'data','GLORYS','GLORYS1993'),
	polygon=Isob100,redo=F){
				require(satin) 
				require(PBSmapping)
				y1 = read.cmems(glorysfile)
				nL = length(y1)	

				x1 = y1[[1]]
				nlats = length(x1@lat)
				nlons = length(x1@lon)
				g = expand.grid(x1@lon,x1@lat)
				names(g) = c('X','Y')
				g$EID = 1:nrow(g)
				g$lonI = rep(1:nlons,times=nlats)
				g$latI = rep(1:nlats,each=nlons)
				ds = x1@period[[1]]
				g = g[findPolys(g,polygon)$EID,] #this gets us the locs to use for all other elements 
				a1 = list()
		for(i in 1:nL){
				out = list()
				x = y1[[i]]
				na = x@attribs$title
			for(j in 1:nrow(g)){
						u = g[j,]
						out[[j]] = x@data[u$latI, u$lonI, ,1]
					}
				a2 = do.call(rbind,out)
				colnames(a2) = paste('X',1:365,sep="")
				a2 = cbind(g,a2)
				r = gather(a2, day, na, X1:X365)
				names(r)[7] = na
				r$date = rep(ds,each=nrow(g))
				a1[[i]] = r
				}
				out = merge_lists(a1)
				return(out)
		}
