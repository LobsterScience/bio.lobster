#' @export
pseudoR2rlm <- function(rlmmod,data){
  rss = deviance(rlmmod)
  tss = (n-1)*vy
  r2 = 1-(rss    /tss)
  
  return(1-((n-1)/(n-nparam) * (r2)))
  }