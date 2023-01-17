#Rmarkdown Script
	require(tint)
	require(rmarkdown)
	require(PBSmapping)
	require(bio.lobster)
	require(bio.base)
	require(bio.utilities) 
	require(rio)

#Ensure you have run error checks in vent.study.r
	
#local=T
yr="2022"
fd= file.path('C:/bio.data/bio.lobster/requests/vent')
setwd(fd)

cleanRmd <- function(RmdName = 'VentReport', RmdFolder = 'Markdown') {
					unlink(file.path(RmdFolder,paste(RmdName,'_cache',sep="")),recursive=T)
					unlink(file.path(RmdFolder,paste(RmdName,'_files',sep="")),recursive=T)
					unlink(file.path(RmdFolder,paste(RmdName,'.tex',sep="")))
					cat( paste('Clean',RmdName,"\n",sep=" "))
					}

cleanRmd()
	
vent=read.csv("C:/bio.data/bio.lobster/data/vent/Trap.csv")
Fi=unique(vent$Fisher)

	for(i in 1:length(Fi)) {
				f=Fi[i]
	     	if(!dir.exists(file.path(yr,'reports',f))) dir.create(file.path('reports',f))
				
				rmarkdown::render('Markdown/BycatchTripValidation.Rmd',quiet=T)
				file.rename(from = file.path('Markdown','BycatchTripValidation.pdf'), to = file.path('Reports',grp[1],nm))
				cleanRmd()
				file.rename(from = file.path('NewTrips',Fi[i]), to = file.path('TripsRun',grp[1],Fi[i]))
				rm(dat)
			}
		