#' @export
 clipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
       con <- pipe("xclip -selection clipboard -i", open="w")
      write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
      close(con)
 }
