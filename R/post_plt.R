#' @export
post_plt <- function(model.out, priors, years, nr=4, nc=2, wd=8, ht=11, post.labs=NULL, graphic='R',xl.type=1, multi=T,path=''){

	if(graphic=='pdf')pdf(file.path(path,"post_single.pdf"), width = wd, height = ht, pointsize = 16)
	if(graphic=='R')x11(wd, ht)
	par(mfrow = c(nr, nc), mar = c(2, 3, 1, 1), omi = c(0.4, 0.6, 0, 0.2))
	
	posts<-model.out$sims.list[lapply(model.out$median,length)==1]
	
	for (i in 1:(length(posts))){
		
		if(names(posts)[i]!="deviance"){
			if(names(posts)[i]%in%names(priors)){
				if(priors[[names(posts)[i]]]$d=="dbeta")xl=c(0,1)
				else if(priors[[names(posts)[i]]]$d%in%c("dlnorm","dgamma"))xl<-c(0,max(posts[[i]]))
				else xl<-range(posts[[i]])
				x<-seq(xl[1], xl[2], l = 500)
				p<-get(priors[[names(posts)[i]]]$d)(x,  priors[[names(posts)[i]]]$a,  priors[[names(posts)[i]]]$b)
			}
			else {
				xl<-range(posts[[i]])
				#yl=range(posts[[i]])
			}
			
			hist(posts[[i]], breaks = 25, main = "", prob = T, ylab = "", las = 1, mgp=c(1,0.4,0), tcl=-0.3, xlab="",cex.axis=1.2,xlim=xl)
			if(names(posts)[i]%in%names(priors))lines(x, p, col = 'red')
			mtext(names(posts)[i], 1, 2, cex=1)
			if(i%in%(nr*nc*1:5))mtext("Posterior density", 2, 2, outer = T, adj = 0.5, cex=1.25)
		}

	}
	mtext("Posterior density", 2, 2, outer = T, adj = 0.5, cex=1.25)
	if(graphic!="R")dev.off()	
		
	
	if(multi){
		multiposts<-model.out$sims.list[lapply(model.out$median,length)>1]
		if(missing(years))years<-model.out$data$iyr
		
		if(graphic=='pdf')pdf(file.path(path,"post_annual.pdf"), width = wd, height = ht, pointsize = 12)
		for (i in 1:length(posts)){
			if(graphic=='R')windows(wd, ht)
			par(mfrow = c(ceiling(model.out$data$NY/ceiling(sqrt(model.out$data$NY))), ceiling(sqrt(model.out$data$NY))), mar = c(2, 4, 2, 1), omi = c(0.4, 0.4, 0, 0.2))
			
			for(y in 1:ncol(multiposts[[i]])){
				if(names(multiposts)[i]%in%names(priors)&&xl.type==1){
					if(priors[[names(multiposts)[i]]]$d=="dbeta")xl=c(0,1)
					else if(priors[[names(multiposts)[i]]]$d%in%c("dlnorm","dgamma"))xl<-c(0,max(multiposts[[i]]))
					else xl<-range(posts[[i]])
				}
				else {
					if(xl.type==1)xl<-range(multiposts[[i]])
					if(xl.type==2)xl<-range(multiposts[[i]][,y])
				}
				x<-seq(xl[1], xl[2], l = 1000)
		#browser()
				
				if(names(multiposts)[i]%in%names(priors)){
					if(length(priors[[names(multiposts)[i]]]$a)==1)p<-get(priors[[names(multiposts)[i]]]$d)(x,  priors[[names(multiposts)[i]]]$a,  priors[[names(multiposts)[i]]]$b)
					if(length(priors[[names(multiposts)[i]]]$a)>1)p<-get(priors[[names(multiposts)[i]]]$d)(x,  priors[[names(multiposts)[i]]]$a[y],  priors[[names(multiposts)[i]]]$b[y])
				}
				
				hist(multiposts[[i]][,y], breaks = 30, main = as.character(years[y]), prob = T, las = 1,ylab="",xlim= xl)
				if(names(multiposts)[i]%in%names(priors))lines(x, p, col = 'red')
			}
			mtext("Posterior density", 2, 1, outer = T, adj = 0.5, cex=1.25)
			mtext(names(multiposts)[i], 1, 1, outer = T, adj = 0.5, cex=1.25)
		}
		if(graphic!="R")dev.off()	
	}



}
	


