#' fixSensorLatLon
#' @export

fixSensorLatLon <- function(x){
  #dealing with missing zeros from the mm part of the deg mm.ssss
  w = unlist(strsplit(unlist(strsplit(x, "[\\.]+"))," "))
  w = w[-length(w)]
  if(nchar(w[2])<2) w[2]=paste(0,as.character(w[2]),sep="")
  w = paste(paste(w[1],w[2],sep=""),w[3],sep=".")

    return(bio.utilities::convert.dd.dddd(as.numeric( w)))
}