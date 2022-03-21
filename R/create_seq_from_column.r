#' @export

create_seq_from_column = function(x){
    #x is a column with data you want to reclassify with breaks
  gh = which(c(FALSE, tail(x,-1) != head(x,-1)))
  o = c()
  if(length(gh)==0) {
    o = rep(1,times=length(x))
  } else{ 
  for(i in 1:(length(gh)+1)){
    if(i==1) o=c(o,rep(i,times=(gh[i]-1)))
    if(i>1 && i<(length(gh)+1)) o=c(o,rep(i,times=((gh[i]-1)-gh[i-1])+1))
    if(i==length(gh)+1) o=c(o,rep(i,times=(length(x)-(gh[i-1])+1)))
      }                                      
    }
    return(o)
  }