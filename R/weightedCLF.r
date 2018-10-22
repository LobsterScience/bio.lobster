#' @export
weightedCLF <- function(x, probs = c(0.025,0.5,0.975),grouping = NULL,returnLF = F,repper = 100,at.sea.samples=F,fsrs.commercial.samples=F,port.samples=F,fsrs.recruit.samples=F,lab=NULL) {
if(at.sea.samples){	
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
		outa = NULL
		
	if(nrow(x)>1){
	
	#		f1 = with(x,sum(TrapsSampledwLobster/TrapsSampled * PropLand)/sum(PropLand))
			f2 = with(x,sum(NLobster/TrapsSampled * PropLand)/sum(PropLand))
			f3 = with(x,sum(NFemaleLobster/(NMaleLobster + NFemaleLobster) * PropLand)/sum(PropLand))
			f4 = with(x,sum(NBerriedLobster/(NLobster) * PropLand)/sum(PropLand))
			f5 = with(x,sum(NCullLobster/(NLobster) * PropLand)/sum(PropLand))
			f6 = with(x,sum(NVnotchedLobster/(NLobster) * PropLand)/sum(PropLand))
			f7 = with(x,sum(PropLand))
			f8 = with(x,sum(NFemaleShortLobster/(NmaleShortLobster + NFemaleShortLobster) * PropLand)/sum(PropLand))
			f9 = with(x,sum(NFemalelegalLobster/(NmalelegalLobster + NFemalelegalLobster) * PropLand)/sum(PropLand))
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			x$PropLand = x$PropLand / max(x$PropLand) * repper
			oo = c()			
			for(b in 1:nrow(x)){
					oo = c(oo, as.numeric(rep(j,times=x[b,i] * round(x[b,'PropLand']))))
					}
			hist(oo,breaks=j)
			vec=NULL
			if(returnLF) vec = oo
		outa = list(LFA = unique(x$LFA), Year = unique(x$SYEAR),NWeeks = length(unique(x$WOS)), NTrips = length(unique(x$TRIPNO)),N_Grids = length(unique(x$GRID_NUM)),TotalLobsters = sum(x$NLobster), TotalTraps = sum(x$TrapsSampled), vec = vec,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs),PropLandings = f7, catch.rate.n = f2, prop.female = f3, prop.berried = f4, prop.cull = f5, prop.vnotched = f6 , prop.female.short = f8, prop.female.legal = f9 )
		}
	}
if(fsrs.commercial.samples | fsrs.recruit.samples)	{
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
		outa = NULL
	
	if(nrow(x)>1){
			f2 = with(x,sum(NLobster/TrapsSampled * PropLand)/sum(PropLand))
			f3 = with(x,sum(NFemaleLobster/(NMaleLobster + NFemaleLobster) * PropLand)/sum(PropLand))
			f4 = with(x,sum(NBerriedLobster/(NLobster) * PropLand)/sum(PropLand))
			f6 = with(x,sum(NVnotchedLobster/(NLobster) * PropLand)/sum(PropLand))
			f7 = with(x,sum(PropLand))
			f8 = with(x,sum(NFemaleShortLobster/(NmaleShortLobster + NFemaleShortLobster) * PropLand)/sum(PropLand))
			f9 = with(x,sum(NFemalelegalLobster/(NmalelegalLobster + NFemalelegalLobster) * PropLand)/sum(PropLand))
			
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			x$PropLand1 = x$PropLand / max(x$PropLand) * repper
			oo = c()			
			for(b in 1:nrow(x)){
					oo = c(oo, as.numeric(rep(j,times=x[b,i] * round(x[b,'PropLand1']))))
					}
			hist(oo,breaks=j,main=lab)
			vec=NULL
			if(returnLF) vec = oo
		outa = list(LFA = unique(x$LFA), Year = unique(x$SYEAR),NWeeks = length(unique(x$WOS)),NTrips = length(unique(x$ids)),N_Grids = length(unique(x$GRID_NUM)),TotalLobsters = sum(x$NLobster), TotalTraps = sum(x$TrapsSampled), PropLandings = sum(x$PropLand),vec = vec,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs), catch.rate.n = f2, prop.female = f3, prop.berried = f4, prop.vnotched = f6 ,prop.female.short=f8,prop.female.legal=f9 )
		}
	}

if(port.samples)	{
		if(!is.null(grouping)) {
		for(i in 1:length(grouping)){
				gr = grouping[i]
		if(names(gr)=='WOS') {
				wos <- unlist(gr)
				x = subset(x,WOS %in% wos)
				}
			}
		}
		outa = NULL
		
	if(nrow(x)>1){
			f3 = with(x,sum(NFemaleLobster/(NMaleLobster + NFemaleLobster) * PropLand)/sum(PropLand))
			f4 = with(x,sum(PropLand))
			i = which(!is.na(as.numeric(names(x))))
			j = as.numeric(names(x[i]))
			x$PropLand = x$PropLand / max(x$PropLand) * repper
			oo = c()			
			for(b in 1:nrow(x)){
					oo = c(oo, as.numeric(rep(j,times=x[b,i] * round(x[b,'PropLand']))))
					}
			hist(oo,breaks=j)
			vec=NULL
			if(returnLF) vec = oo
		outa = list(LFA = unique(x$LFA), Year = unique(x$SYEAR),NSamples =sum(x$SAMPLES_BY_PORT) ,NWeeks = length(unique(x$WOS)), N_Ports =length(unique(x$PORT)) ,TotalLobsters = sum(x$NLobster), vec = vec,mean = mean(oo),sd = sd(oo), quants = quantile(oo,probs=probs), prop.female = f3 ,PropLandings = f4)
		}
}
		return(outa)
}