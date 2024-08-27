sourcery<-function(Rdir = NULL){
  if(is.null(Rdir)) {
    #check for R folder
    if (dir.exists(file.path(getwd(),"R"))){
      Rdir <- file.path(getwd(),"R")
      message("sourcing R folder")
    } else if (length(list.files(pattern = "*.r$", ignore.case = T))>0){
      Rdir <- getwd()
      message("sourcing wd")
    }
    if (is.null(Rdir))stop("Please provide an Rdir containing R files or an R folder")
  }
  R.utils::sourceDirectory(Rdir, modifiedOnly=F, env=.GlobalEnv)
}