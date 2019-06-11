 
	p = bio.lobster::load.environment()
	la()

	p$current.assessment.year = p$current.assessment.year - 1 ########### check the year ############### !!!!!!!!!!!
   	    
   	    figdir = file.path(project.datadirectory("bio.lobster"),"figures")
 	
    	p$lfas = c("27", "28", "29", "30", "31A", "31B", "32","33","34","35","36","38") # specify lfas for data summary
    	p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32","33E","33W","34","35","36","38") # specify lfas for data summary
		
		CPUE.data<-CPUEModelData(p,redo=T)
		cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1982:2018,graphic='R')$annual.data
		
		mu = with(subset(cpueData,YEAR<2017&YEAR>1989),tapply(CPUE,LFA,median))	
		mu = mu[1:8]

	x11(width=7,height=8)
	par(mfrow=c(6,2),mar=c(0,0,0,0),omi=c(0.3,0.6,0.2,0.2),las=1,tcl=0.3)

	for(i in 1:length(p$lfas)){

			crd = subset(cpueData,LFA==p$lfas[i],c("YEAR","CPUE"))
			crd = merge(crd,data.frame(YEAR=min(crd$YEAR):max(crd$YEAR)),all=T)
			usr = mu[i] * 0.8
			lrp = mu[i] * 0.4

				# plot
			x = ifelse(i%in%c(11,12),'s','n')
			y = ifelse(i%in%seq(1,11,2),'s','n')

			CatchRatePlot(data = crd ,usr = usr,lrp=lrp,lfa = p$lfas[i],fd=figdir,ylim=c(0,2.8),xlim=c(1985,2018),rm=F,title.line=-1,xaxt=x,yaxt=y,regions=T)


		}
		mtext("CPUE (kg / trap haul)",2,outer=T,las=0,line=2.6)

	savePlot(file.path(figdir,'scofoCPUE.png'),type='png')


	x11(width=17,height=8)

# Stacked Bar Plot with Colors and Legend
ann.land = lobster.db('annual.landings')
sea.land = lobster.db('seasonal.landings')
land = cbind(subset(ann.land,YR>1987,-c(8,10:15)), sea.land[13:43,-1],subset(ann.land,YR>1987,15))
landings=t(land[,-1])

barplot(landings, main="Total Landings (t)",  xlab="", col=rainbow_hcl(nrow(landings)), names.arg=1988:2018, legend = rownames(landings),args.legend=list(x='topleft',bty='n'))

	savePlot(file.path(figdir,'scofoLAND.png'),type='png')
