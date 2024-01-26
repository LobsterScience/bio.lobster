#' @export

zero.na <- function(x){

    for (i in 1:length(x[1, ])) {
      if (length(which(x[, i]==0)) > 0) {
        x[which(x[, i]==0), i] <- NA
      }
    }
    return(x)
  }
