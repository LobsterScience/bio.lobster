#' @export
#' 
identifyLPU <- function(){
      require(dplyr)
      #using landings since 1975
      tod =lubridate::today()
      myr = lubridate::year(tod)
  
      a = lobster.db('annual.landings')
      a = subset(a,!is.na(YR)& YR>1975 & YR<myr)
      sa = a %>% gather(key='LFA',value='Landings',-YR)
      sa = subset(sa,LFA<'LFA33')
      sa = subset(sa,LFA %ni% 'LFA31')
      sa = sa[order(sa$LFA,sa$YR),]
      a = lobster.db('seasonal.landings')
      a$SYEAR = as.numeric(substring(a$SYEAR,6,9))
      a = subset(a,!is.na(SYEAR)& SYEAR>1975& SYEAR<myr)
      sa1 = a %>% pivot_longer(cols=starts_with('LFA'),names_to="LFA",values_to='Landings')
      names(sa1)[1] = "YR"
      
      o = bind_rows(sa,sa1)
      o1 = subset(o,LFA %ni% c('LFA38B','LFA28'))
      
      o <- o1 %>%
        pivot_wider(
          id_cols = LFA,          
          names_from = YR,        
          values_from = Landings  
        ) %>%
        tibble::column_to_rownames('LFA') 
      
      tom = scale(t(as.matrix(o)))
     require(tsfeatures)
     require(dplyr)
     require(cluster)
     require(factoextra)
     require(dtwclust)
     
    #dynamic time warp
    
    d_dtw <- proxy::dist(t(tom), method = "DTW")  
    hc <- hclust(d_dtw, method = "ward.D2")
    
    # Cut into k clusters
    k <- 4
    groups <- cutree(hc, k)
    
    # Plot dendrogram
    plot(hc, hang = -1, main = "DTW-based hierarchical clustering")
    rect.hclust(hc, k = k, border = 2:5)
}
