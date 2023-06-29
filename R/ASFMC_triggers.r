#' @export
ASMFC_triggers <- function(x,yrs){
  #https://asmfc.org/uploads/file/64651dabAmLobsterAddendumXXVII_May2023.pdf
  
  xss = zoo::rollmean(x,k=3,align='left')
  xss = xss/max(xss)
  return(list(index=xss,yrs=yrs[c(-1,-2)]))
  
  }