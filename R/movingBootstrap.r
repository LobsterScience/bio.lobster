#' @export
movingBootstrap <- function(x,span=seq(2,5,1)) {
	#x is the data from nefsc.analysis or dfo.rv.analysis

			m=0
			res = list()
			fy = list()
			for(j in max(span):length(x)) {
				m=m+1
				outputs <- c()
				obj <- list()
				class(obj) <- 'boot'
				
				for(i in span) {
	         	a = x[c((j-i+1):j)]
			    l = do.call(rbind,lapply(a,"[[",2))
				y = data.frame(st = unlist(lapply(lapply(a,"[[",1),'[[',1)),Nh=unlist(lapply(lapply(a,"[[",1),'[[',2)))
				y = as.data.frame(unique(cbind(y$st,y$Nh)))
				if(any(names(l)=='GMT_YEAR')) yr = max(unique(l$GMT_YEAR))
				st = list()
				st$Strata = y[,1]
				st$NH = y[,2]
  				  
  				  st = Prepare.strata.file(st)
                  sc = Prepare.strata.data(l)
                  sN = Stratify(sc,st,sc$TOTNO)
                  ssN = summary(sN)
				  bsN = boot.strata(sN,method='BWR',nresamp=5000)$boot.means
				  outputs = c(outputs,bsN)
				  }
				 obj$boot.means = outputs
				res[[m]] <- c(summary(obj,gini=F)[[1]],yr)
				fy[[m]] <- c(summary(obj,big.ci=T,gini=F),yr)
				}
				res = data.frame(do.call(rbind,res))
				names(res) = c('Qlow','Qhigh','Med','yr')
				fy = data.frame(do.call(rbind,fy))

				return(list(res,fy))
				}