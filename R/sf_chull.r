#' This function generates a convext hull around a set of points
#' @param x is your sf point set to create a convex hull
#' @param buffer is your buffer dist (meaningless unless crs is UTM)
#' @return a buffered polygon of all points
#' @export

sf_chull <- function(x, buffer=.25){
  if(!inherits(x,c('sf','sfc'))) stop('needs to be an sf object')
  x1 = st_geometry(x)
  x1 = x1[!duplicated(x1)]
  x3 = st_buffer(x1,dist=buffer)
  x4 = st_union(x3)
  return(x4)
}