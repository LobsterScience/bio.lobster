#' @export
weightedCLF <- function(x, logs=NULL, weighting.scheme = c('none','time','space','time.space'),probs = c(0.025,0.5,0.975),grouping = NULL,returnLF = F,returnHistStats=F) {
	
	#x = aCC from atSeaLogbookLinker
	#logs = pruned data for entire fishery and year
	#weighting is by landings
	
	if(!is.null(grouping)) {
		for(i in 1:length(grouping)){
				gr = grouping[i]
		if(names(gr)=='WOS') {
				wos <- unlist(gr)
				x = subset(x,WOS %in% wos)
				}
		if(names(gr)=='GRID_NUM') {
				wos <- unlist(gr)
				x = subset(x,GRID_NUM %in% wos)
				}
			}
		}
		
		jk=NULL
		if(any(names(x)=='WEIGHT_KG')) jh = (nrow(x[!is.na(x$WEIGHT_KG),])/nrow(x))
	out = list(LFA = unique(x$LFA), Year = unique(x$SYEAR),Grouping=grouping,NTrips = length(unique(x$TRIPNO)),N_Grids = length(unique(x$GRID_NUM)),TotalLobsters = sum(x$NLobster), TotalTraps = sum(x$TrapsSampled),prop.linked.to.trips=jk)
	if(nrow(x)>1){
	if(is.null(logs)) weighting.scheme = 'none' #if no log book info then don't weight at all
	if(any(weighting.scheme=='none')) {
	#ignores actual logbook links
			wq = x[,c("TrapsSampled","TrapsSampledwLobster","NLobster","NMaleLobster","NFemaleLobster","NBerriedLobster", "NCullLobster" ,"NVnotchedLobster")]
			
			f1 = with(wq,sum(TrapsSampledwLobster)/sum(TrapsSampled))
			f2 = with(wq,sum(NLobster)/sum(TrapsSampled))
			f3 = with(wq,sum(NFemaleLobster)/sum(NMaleLobster + NFemaleLobster))
			f4 = with(wq,sum(NBerriedLobster)/sum(NLobster))
			f5 = with(wq,sum(NCullLobster)/sum(NLobster)) 
			f6 = with(wq,sum(NVnotchedLobster)/sum(NLobster))
			
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i])) 
			lens = colSums(x[,i])
			lens = as.numeric(rep(j,times=lens))
			hist(lens,breaks=j)
			vec=NULL
			if(returnLF) vec = lens
			if(returnHistStats) {return(lens)}
			out$none = list(vec = vec,mean = mean(lens),sd = sd(lens), quants = quantile(lens,probs=probs), proportion.of.sampled.traps.w.lobster = f1, catch.rate.n = f2, prop.female = f3, prop.berried = f4, prop.cull = f5, prop.vnotched = f6  )
	}

	if(any(weighting.scheme=='space')) {
		#ignores actual logbook links

			lo = aggregate(WEIGHT_KG~GRID_NUM,data=logs,FUN=sum)
			wq = aggregate(cbind(TrapsSampled,TrapsSampledwLobster,NLobster,NMaleLobster,NFemaleLobster,NBerriedLobster, NCullLobster ,NVnotchedLobster)~GRID_NUM,data=x,FUN=sum)
			wq = merge(wq,lo,by='GRID_NUM')
			f1 = with(wq,sum(TrapsSampledwLobster/TrapsSampled * WEIGHT_KG)/sum(WEIGHT_KG))
			f2 = with(wq,sum(NLobster/TrapsSampled * WEIGHT_KG)/sum(WEIGHT_KG))
			f3 = with(wq,sum(NFemaleLobster/(NMaleLobster + NFemaleLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f4 = with(wq,sum(NBerriedLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f5 = with(wq,sum(NCullLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f6 = with(wq,sum(NVnotchedLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			y = cbind(x[,i],GRID_NUM=x$GRID_NUM)
			h = split(y,f=x$GRID_NUM)
			h = lapply(h,FUN=function(x) colSums(x[,1:(ncol(x)-1)]))
			h = as.data.frame(do.call(rbind,h))
			h$GRID_NUM = rownames(h)
			h = merge(h,lo)
			if(nrow(h)>1){
			h$WEIGHT_KG = h$WEIGHT_KG / 10000
			oo = c()			
			i = which(!is.na(as.numeric(names(h))))
			for(b in 1:nrow(h)){
					oo = c(oo, as.numeric(rep(j,times=h[b,i] * round(h[b,'WEIGHT_KG']))))
					}
			hist(oo,breaks=j)
			vec=NULL
			if(returnLF) vec = oo
			out$space = list(vec = vec,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs), proportion.of.sampled.traps.w.lobster = f1, catch.rate.n = f2, prop.female = f3, prop.berried = f4, prop.cull = f5, prop.vnotched = f6  )
		}
	}

	if(any(weighting.scheme=='time')) {
		
			#ignores actual logbook links
			if(all(is.na(x$WOS))) x$WOS=1
			
			lo = aggregate(WEIGHT_KG~WOS,data=logs,FUN=sum)
			wq = aggregate(cbind(TrapsSampled,TrapsSampledwLobster,NLobster,NMaleLobster,NFemaleLobster,NBerriedLobster, NCullLobster ,NVnotchedLobster)~WOS,data=x,FUN=sum)
			wq = merge(wq,lo,by='WOS')
			f1 = with(wq,sum(TrapsSampledwLobster/TrapsSampled * WEIGHT_KG)/sum(WEIGHT_KG))
			f2 = with(wq,sum(NLobster/TrapsSampled * WEIGHT_KG)/sum(WEIGHT_KG))
			f3 = with(wq,sum(NFemaleLobster/(NMaleLobster + NFemaleLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f4 = with(wq,sum(NBerriedLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f5 = with(wq,sum(NCullLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f6 = with(wq,sum(NVnotchedLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			y = cbind(x[,i],WOS=x$WOS)
			h = split(y,f=x$WOS)
			h = lapply(h,FUN=function(x) colSums(x[,1:(ncol(x)-1)]))
			h = as.data.frame(do.call(rbind,h))
			h$WOS = rownames(h)
			
			lo = aggregate(WEIGHT_KG~WOS,data=logs,FUN=sum)
			h = merge(h,lo)
			if(nrow(h)>1){
			h$WEIGHT_KG = h$WEIGHT_KG / 10000
			oo = c()
			i = which(!is.na(as.numeric(names(h))))
			for(b in 1:nrow(h)){
					oo = c(oo, as.numeric(rep(j,times=h[b,i] * round(h[b,'WEIGHT_KG']))))
					}
			hist(oo,breaks=j)
			vec=NULL
			if(returnLF) vec = oo
			out$time = list(vec = vec,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs), proportion.of.sampled.traps.w.lobster = f1, catch.rate.n = f2, prop.female = f3, prop.berried = f4, prop.cull = f5, prop.vnotched = f6  )
			}
		}

if(any(weighting.scheme=='time.space')) {
		#incorporates logbook links
		if(all(is.na(x$WOS))) x$WOS=1
			lo = aggregate(WEIGHT_KG~WOS+GRID_NUM,data=logs,FUN=sum)

if(any(!is.na(x$WEIGHT_KG))){
			wq = aggregate(cbind(TrapsSampled*WEIGHT_KG,TrapsSampledwLobster*WEIGHT_KG,NLobster*WEIGHT_KG,NMaleLobster*WEIGHT_KG,NFemaleLobster*WEIGHT_KG,NBerriedLobster*WEIGHT_KG, NCullLobster *WEIGHT_KG,NVnotchedLobster*WEIGHT_KG)~WOS+GRID_NUM,data=x,FUN=sum)
			names(wq)[3:ncol(wq)] = c("TrapsSampled","TrapsSampledwLobster","NLobster","NMaleLobster","NFemaleLobster","NBerriedLobster", "NCullLobster" ,"NVnotchedLobster")
			wq = merge(wq,lo,by=c('WOS','GRID_NUM'))
			f1 = with(wq,sum(TrapsSampledwLobster/TrapsSampled * WEIGHT_KG)/sum(WEIGHT_KG))
			f2 = with(wq,sum(NLobster/TrapsSampled * WEIGHT_KG)/sum(WEIGHT_KG))
			f3 = with(wq,sum(NFemaleLobster/(NMaleLobster + NFemaleLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f4 = with(wq,sum(NBerriedLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f5 = with(wq,sum(NCullLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
			f6 = with(wq,sum(NVnotchedLobster/(NLobster) * WEIGHT_KG)/sum(WEIGHT_KG))
	
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			y = cbind(x[,i],WOS=x$WOS, GRID_NUM=x$GRID_NUM, WEIGHT_KG=x$WEIGHT_KG )
			y$WEIGHT_KG[which(is.na(y$WEIGHT_KG))] <- 20
			h = split(y,f=list(x$WOS,x$GRID_NUM))
			h = lapply(h,FUN=function(x) colSums(x[,1:(ncol(x)-3)]*x$WEIGHT_KG))
			h = as.data.frame(do.call(rbind,h))
			h$ID = rownames(h)
			h = cbind(do.call(rbind,strsplit(h$ID,"\\.")),h)
			names(h)[1: 2] <- c('WOS','GRID_NUM')
			
			lo = aggregate(WEIGHT_KG~WOS+GRID_NUM,data=logs,FUN=sum)
			h = merge(h,lo)
			h$WEIGHT_KG = h$WEIGHT_KG / 1000
			oo = c()
			i = which(!is.na(as.numeric(names(h))))
			for(b in 1:nrow(h)){
					oo = c(oo, as.numeric(rep(j,times=h[b,i] * round(h[b,'WEIGHT_KG']))))
					}
			hist(oo,breaks=j)
			vec=NULL
			if(returnLF) vec = oo
			out$time.space = list(vec = vec,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs), proportion.of.sampled.traps.w.lobster = f1, catch.rate.n = f2, prop.female = f3, prop.berried = f4, prop.cull = f5, prop.vnotched = f6  )
		}
	}
}
return(out)

}