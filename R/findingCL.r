#' @export

findingCL = function(x,y,m=7,sz=c(65,110)){
      yi = findPeaks(y)
      xi = x[yi]
      
      xii = which(xi>sz[1] & xi<sz[2])
      yii = yi[min(xii)]
      
    return(c(index=yii,x=x[yii]))
  
}