#' @export

IndicatorplotsMultDataSets <- function(atSea=NULL, port=NULL, fsrs=NULL,indicator, fsrs.rec = NULL, out.dir='bio.lobster',mls=NULL,secondary.axis=F,save=T) {
		#using the three year aggregated LCAs

			fd = file.path(project.figuredirectory(out.dir),'AtSeaIndictors')
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
		if(indicator == 'Median.Size') y = 'quants.50%'
		if(indicator == 'Maximum.Size') y = 'quants.97.5%'
		if(indicator == 'Sex.Ratio') y = 'prop.female'
		if(indicator == 'Proportion.Berried') y = 'prop.berried'
		if(indicator == 'Proportion.Landings') y = 'PropLandings'
		if(indicator == 'New.Recruits') y = 'new.rec'
		
		lf = c("27" , "28" , "29" , "30" , "31A" ,"31B" ,"32" , "33" )
		outs = list()
		for(i in lf){
			fname = paste('CombinedData',indicator,i,'.png',sep="")
			cols = c()
			nn = c()
			yt = c()
			xp = c()
			lt = c()
				if(!is.null(atSea)){ o = subset(atSea,LFA==i); o = rename.df(o,y,'Indi'); if(nrow(o)>0) { cols = c(cols,'black'); nn = c(nn,'AtSea'); yt = c(yt,o$Year);  lt = c(lt,1); if(y=='PropLandings') {o$Indi[o$Indi>1] <- 1}; xp = c(xp,o$Indi)}}
				if(!is.null(port)) {p = subset(port,LFA==i); p = rename.df(p,y,'Indi');if(nrow(p)>0 & any(names(p)=='Indi')) { cols = c(cols,'red'); nn = c(nn,'Port'); yt = c(yt,p$Year); lt = c(lt,2);if(y=='PropLandings') {p$Indi[p$Indi>1] <- 1}; xp = c(xp,p$Indi)}}
				if(!is.null(fsrs)) {r = subset(fsrs,LFA==i); r = rename.df(r,y,'Indi');if(nrow(r)>0) { cols = c(cols,'blue'); nn = c(nn,'FSRS'); yt = c(yt,r$Year); lt = c(lt,3); if(y=='PropLandings') {r$Indi[r$Indi>1] <- 1};xp = c(xp,r$Indi)}}
				if(!is.null(fsrs.rec)) {v = subset(fsrs.rec,LFA==i); v = rename.df(v,y,'Indi');if(nrow(v)>0) { cols = c(cols,'orange'); nn = c(nn,'FSRSRec'); yt = c(yt,v$Year); lt = c(lt,4); if(y=='PropLandings') {v$Indi[v$Indi>1] <- 1};xp = c(xp,v$Indi)}}
			if(save)	png(file=file.path(fd,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
				#dealing with the 0.5s for legal and sublegal fsrs codes ugh
	
				if(!secondary.axis & indicator %in% c('Median.Size','Maximum.Size') & c(!is.null(fsrs) | !is.null(fsrs.rec))) {
					if(c(nrow(r)>0 | nrow(v)>0)) {				
							a = read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","FSRS_SIZE_CODES.csv"))
							a$xbar = apply(a[,c('MIN_S','MAX_S')],1,mean)
						
							if(nrow(v)>0){ 
								v$in2 = v$Indi
								N = unique(v$Indi)
							if(any(N %ni% a$SIZE_CD)){
									hi = N[which(N %ni% a$SIZE_CD)]
								for(k in 1:length(hi)) {
											vv = v[which(v$Indi==hi[k]),]
											mm = subset(mls,LFA==i & Year %in% vv$Year)
										for(w in 1:length(mm)) {
												mo = a[which(a$SIZE_CD==hi[k]-0.5),'MAX_S']
												jo = mean(c(mo,as.numeric(unique(mm[w,'MLS_MM']))))
												v[which(v$Indi==hi[k] & v$Year ==mm$Year[w]),'Indi'] <- jo
												}
											}
										}
								for(w in 1:nrow(v)){
									if(v[w,'Indi']>70) next()
									v$Indi[w] = a[match(v$Indi[w], a$SIZE_CD),'xbar']
									}
									if(any(na.omit(v$Indi)>130)){iii = which(v$Indi>130); v$Indi[iii] <- NA}
								xp = c(xp,v$Indi)
								}

								if(nrow(r)>0){ 
									r$in2 = r$Indi
								N = unique(r$Indi)
							if(any(N %ni% a$SIZE_CD)){
									hi = N[which(N %ni% a$SIZE_CD)]
								for(k in 1:length(hi)) {
											vv = r[which(r$Indi==hi[k]),]
											mm = subset(mls,LFA==i & Year %in% vv$Year)
										for(w in 1:nrow(mm)) {
												mo = a[which(a$SIZE_CD==hi[k]-0.5),'MAX_S']
												jo = mean(c(mo,as.numeric(unique(mm[w,'MLS_MM']))))
												r[which(r$Indi==hi[k] & r$Year ==mm$Year[w]),'Indi'] <- jo
												}
											}
										}
								for(w in 1:nrow(r)){
									if(r[w,'Indi']>70) next()
									r$Indi[w] = a[match(r$Indi[w], a$SIZE_CD),'xbar']
									}
									
									if(any(na.omit(r$Indi)>130)){iii = which(r$Indi>130); r$Indi[iii] <- NA}
								xp = c(xp,r$Indi)
								}
						}

							}	
				xr = as.numeric(range(yt,na.rm=T))
				yr = as.numeric(range(xp,na.rm=T))
						if(indicator=='Median.Size') yr[1] = 65
						if(indicator=='Maximum.Size') yr[1] = 80
					
				

				if(secondary.axis) par(mar=c(4,5,2,5))
							
				plot(1,1,type='n',xlab='Year',ylab = indicator,main=paste('LFA',i),xlim=xr,ylim=yr)
				
				if(!is.null(atSea)& nrow(o)>0) with(o,lines(Year,Indi,lty=1,col='black',lwd=2,pch=16,type='b'))
				if(!is.null(port)& nrow(p)>0 & any(names(p)=='Indi')) with(p,lines(Year,Indi,lty=2,col='red',lwd=2,pch=16,type='b'))
				if(indicator %ni% c('Median.Size','Maximum.Size') & !is.null(fsrs) & nrow(r)>0) with(r,lines(Year,Indi,lty=3,col='blue',lwd=2,pch=16,type='b'))
				if(indicator %ni% c('Median.Size','Maximum.Size') & !is.null(fsrs.rec)) {
					if(nrow(v)>0) with(v,lines(Year,Indi,lty=4,col='orange',lwd=2,pch=16,type='b'))
				}
			

				if(!secondary.axis & indicator %in% c('Median.Size','Maximum.Size') & c(!is.null(fsrs) | !is.null(fsrs.rec))) {
					if(c(nrow(r)>0 | nrow(v)>0)) {
					
							a = read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","FSRS_SIZE_CODES.csv"))
							a$xbar = apply(a[,c('MIN_S','MAX_S')],1,mean)
							if(nrow(v)>0) {lines(v$Year,v$Indi,type='b',lty=3,col='orange',pch=16)}
							if(nrow(r)>0) {lines(r$Year,r$Indi,type='b',lty=4,col='blue',pch=16)}
							}
						}

				if(secondary.axis & indicator %in% c('Median.Size','Maximum.Size') & c(!is.null(fsrs) | !is.null(fsrs.rec))) {
						if( c(nrow(r)>0 | nrow(v)>0)) {
							par(new=T)
							if(indicator =='Median.Size') {ylims=c(8,12); yll = c('71-75','76-80','81-90','91-100','101-110')}
							if(indicator =='Maximum.Size'){ylims=c(11,15) ; yll = c('91-100','101-110','111-120','121-130','131-300')}
								
						if(nrow(r)>0)	plot(r$Year,r$Indi,type='b',lty=3,xaxt='n', yaxt='n',ylab='',xlab='',col='blue',ylim=ylims,lwd=2,xlim=xr)
						if(nrow(v)>0 & nrow(r)==0)	plot(v$Year,v$Indi,type='b',lty=3,xaxt='n', yaxt='n',ylab='',xlab='',col='orange',ylim=ylims,lwd=2,xlim=xr)
						if(nrow(v)>0 & nrow(r)>0)	lines(v$Year,v$Indi,type='b',lty=4,xaxt='n', yaxt='n',ylab='',xlab='',col='orange',ylim=ylims,lwd=2,xlim=xr)
							axis(side=4,at=seq(ylims[1],ylims[2],length=5),labels=yll,srt=90)
							mtext(side=4,indicator,line=3,col='black')
			
				}
			}
				legend('bottomright',legend=nn,col=cols,lty=lt,lwd=2,bty='n',cex=0.8)
	if(save)			dev.off()
				print(fname)
				coll = c('LFA','Year','Indi','ID')
				o$ID = 'atSea'
				oo = o[,coll]
				

				if(c(!is.null(fsrs) | !is.null(fsrs.rec))) {
							if(exists('r')) {if(nrow(r)>0) {r$ID = 'fsrs.comm';oo = rbind(oo,r[,coll])}}
							if(exists('v')) {if(nrow(v)>0)  {v$ID = 'fsrs';oo = rbind(oo,v[,coll])}}
							
						}
				
				if(nrow(p)>0 & indicator %ni% 'Proportion.Berried'){ p$ID = 'port';oo = rbind(oo,p[,coll])}
				outs[[i]] <- oo
		}
		return(outs)
	}