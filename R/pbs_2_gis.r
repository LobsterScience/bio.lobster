#' @export

pbs_2_gis = function(dat,cords = c('X','Y'),crx=4326,groups=T){
  gr = 1  
  if(groups & !any(names(dat)=='label')) stop('grouping column must be called "label"')
    if(groups) gr = unique(dat$label)
    out = list()
    for(i in 1:length(gr) ){
          if(groups) x = subset(dat,label==gr[i])
          if(!groups) x = dat
          x=x[order(x$POS),]
           x = st_as_sf(x,coords=cords,crs=crx)
    
      out[[i]] <- x %>%
      group_by(label) %>%
      summarise(geometry = st_combine(geometry)) %>%
        st_convex_hull()
    } 
    out = bio.utilities::list.names.to.columns(out)
    
}

