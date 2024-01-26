#' @export
identifyPoints<-function(dat,X='X',Y='Y',tl=.25){

x<-as.vector(unlist(dat[X]))
y<-as.vector(unlist(dat[Y]))
rows<-identify(x,y,tol=tl)
dat[rows,]

}
