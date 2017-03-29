compileODF <- function(path=loc) {
		f 	<- file.path(path)
		fi 	<- dir(f,full.names=T)
		fi 	<- fi[grep('.ODF',fi)]
		fi 	<- fi[grep("D",fi)]
		
		for(i in 1:length(fi)) {
			o <- parse.odf.file(fi[i])
		if(i==1) {
		out <- o
		}else {
			out <- rbind(out,o)		
			}				
		}
		return(out)
	}