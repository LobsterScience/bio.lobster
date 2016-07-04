#' @export
identifyPoints<-function(dat,X='X',Y='Y'){

x<-as.vector(unlist(dat[X]))
y<-as.vector(unlist(dat[Y]))
rows<-identify(x,y)
dat[rows,]

}
