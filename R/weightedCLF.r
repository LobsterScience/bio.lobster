#' @export
weightedCLF <- function(x, logs, weighting.scheme = c('none','time','space','time.space'),probs = c(0.025,0.5,0.975),grouping = NULL) {
	
	#x = aCC from CohortAnalysis
	#logs = pruned data for entire fishery and year
	#weighting is by landings
	if(!is.null(grouping)) {
		if(names(grouping)=='WOS') {
				wos <- unlist(grouping)
				x = subset(x,WOS %in% wos)
				}
		if(names(grouping)=='GRID_NUM') {
				wos <- unlist(grouping)
				x = subset(x,GRID_NUM %in% wos)
				}
			}

	out = list()
	if(any(weighting.scheme=='none')) {

			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			lens = colSums(x[,i])
			lens = as.numeric(rep(j,times=lens))
			hist(lens,breaks=j)

			out$none = list(vec = lens,mean = mean(lens),sd = sd(lens), quants = quantile(lens,probs=probs))
	}

	if(any(weighting.scheme=='space')) {
		#ignores actual logbook links
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			y = cbind(x[,i],GRID_NUM=x$GRID_NUM)
			h = split(y,f=x$GRID_NUM)
			h = lapply(h,FUN=function(x) colSums(x[,1:(ncol(x)-1)]))
			h = as.data.frame(do.call(rbind,h))
			h$GRID_NUM = rownames(h)
			
			lo = aggregate(WEIGHT_KG~GRID_NUM,data=logs,FUN=sum)
			h = merge(h,lo)
			h$WEIGHT_KG = h$WEIGHT_KG / 10000
			oo = c()
			i = which(!is.na(as.numeric(names(h))))
			for(b in 1:nrow(h)){
					oo = c(oo, as.numeric(rep(j,times=h[b,i] * round(h[b,'WEIGHT_KG']))))
					}
			hist(oo,breaks=j)
			out$space = list(vec = oo,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs))
		}

	if(any(weighting.scheme=='time')) {
			#ignores actual logbook links
			if(all(is.na(x$WOS))) x$WOS=1
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			y = cbind(x[,i],WOS=x$WOS)
			h = split(y,f=x$WOS)
			h = lapply(h,FUN=function(x) colSums(x[,1:(ncol(x)-1)]))
			h = as.data.frame(do.call(rbind,h))
			h$WOS = rownames(h)
			
			lo = aggregate(WEIGHT_KG~WOS,data=logs,FUN=sum)
			h = merge(h,lo)
			h$WEIGHT_KG = h$WEIGHT_KG / 10000
			oo = c()
			i = which(!is.na(as.numeric(names(h))))
			for(b in 1:nrow(h)){
					oo = c(oo, as.numeric(rep(j,times=h[b,i] * round(h[b,'WEIGHT_KG']))))
					}
			hist(oo,breaks=j)
			out$time = list(vec = oo,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs))
		}

	if(any(weighting.scheme=='time.space')) {
		#incorporates logbook links
		if(all(is.na(x$WOS))) x$WOS=1
		if(any(!is.na(x$WEIGHT_KG))){
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			y = cbind(x[,i],WOS=x$WOS, GRID_NUM=x$GRID_NUM, WEIGHT_KG=x$WEIGHT_KG )
			h = split(y,f=list(x$WOS,x$GRID_NUM))
			h = lapply(h,FUN=function(x) colSums(x[,1:(ncol(x)-3)]*x$WEIGHT_KG))
			h = as.data.frame(do.call(rbind,h))
			h$ID = rownames(h)
			h = cbind(do.call(rbind,strsplit(h$ID,"\\.")),h)
			names(h)[1: 2] <- c('WOS','GRID_NUM')
			
			lo = aggregate(WEIGHT_KG~WOS+GRID_NUM,data=logs,FUN=sum)
			h = merge(h,lo)
			h$WEIGHT_KG = h$WEIGHT_KG / 10000
			oo = c()
			i = which(!is.na(as.numeric(names(h))))
			for(b in 1:nrow(h)){
					oo = c(oo, as.numeric(rep(j,times=h[b,i] * round(h[b,'WEIGHT_KG']))))
					}
			hist(oo,breaks=j)
			out$time.space = list(vec = oo,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs))
		}
	}
return(out)

}