#' @export 
rbindFill <- function(x) {
 # takes a list of named vectors and combines into dataframe
 #https://stackoverflow.com/questions/17308551/do-callrbind-list-for-uneven-number-of-column
    nam <- sapply(x, names)
    unam <- unique(unlist(nam))
    len <- sapply(x, length)
    out <- vector("list", length(len))
    for (i in seq_along(len)) {
        out[[i]] <- unname(x[[i]])[match(unam, nam[[i]])]
    }
    setNames(as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE), unam)
}