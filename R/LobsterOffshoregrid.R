#' @export
LobsterOffshoregrid <- function(Data,lvls,bcol="YlGnBu",border=1,FUN=mean,place=2,rev=F,cuts=F) {  
  
  require(PBSmapping)
  require(RColorBrewer)
 #browser()
  Data<-na.omit(Data)
  names(Data)[1:2]<-c("Z","OFFAREA")
  Data$OFFAREA<-as.numeric(as.factor(Data$OFFAREA))
  tmp<-with(Data,tapply(Z,OFFAREA,FUN))

  pdata  <- merge(data.frame(OFFAREA=unique(Data$OFFAREA)), data.frame(OFFAREA=names(tmp),Z=tmp),all=T)
  pdata  <- with(pdata,data.frame(PID=OFFAREA,Z=Z))
 
  if(missing(lvls))lvls<-round(seq(min(pdata$Z),max(pdata$Z),l=9),place)
  cols   <- brewer.pal(length(lvls),bcol) 
  if(rev) cols = rev(cols)
  lvls<-c(lvls,max(lvls)*100)

  pdata  <- makeProps(pdata, lvls, "col", cols) 
  l = lvls[-1]
  if(cuts) pdata$cuts = l[match(pdata$col,cols)]
  pdata$border<-border
  
  
  grid<-read.csv(file.path( project.datadirectory('bio.lobster'), "data","maps","LFA41Offareas.csv"))
  
  
  list(grid=grid,pdata=pdata,lvls=lvls[-length(lvls)],col=cols)
  
}