#' @export
pseudoR2nls <- function(nlsmod,response){
  n = length(response)    
  vy = var(response)
  nparam = length(coef(nlsmod))
  rss = deviance(nlsmod)
  tss = (n-1)*vy
  r2 = 1-(rss    /tss)
  
  return(1-((n-1)/(n-nparam) * (r2)))
  }