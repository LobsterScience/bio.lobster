#' @export
recruitment.trap.db<-function(DS , outdir = project.datadirectory('bio.lobster'), p,Y=1999:2015) {
	
	fout = file.path(outdir,p$current.assessment.year,'R')
	dir.create(fout,recursive=T,showWarnings=F)

	if(DS %in% c('raw.redo','raw.load')) {
		ffout = file.path(fout,'raw')
		dir.create(ffout,recursive=T,showWarnings=F)

			if(DS=='raw.redo') {
					for(y in Y) {
						RODBCconn = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
						rc<- sqlQuery(RODBCconn, paste("SELECT * FROM fsrs_lobster.FSRS_LOBSTER_VW where HAUL_YEAR = ",y,";",sep=""))
						names(rc) <- tolower(names(rc))
						save(rc,file=file.path(ffout,paste(y,'.rdata',sep="")))
						print(y)
					}
			}
			#default is to load

			out = NULL
			for(y in Y) {

				load(file.path(ffout,paste(y,'.rdata',sep="")))
				out = rbind(out,rc)
			}
			return(out)
		}

	if(DS %in% c('clean.redo','clean.load')) {

		ffout = file.path(fout,'clean')
		dir.create(ffout,recursive=T,showWarnings=F)

		if(DS=='clean.redo') {
						fchecks = file.path(ffout,'checks')
						dir.create(ffout,recursive=T,showWarnings=F)
				for(y in Y) {
						raw = recruitment.trap.db(DS='raw.load',Y=Y,p=p)

						raw$date = as.POSIXct(raw$haul_date)
						raw$julian = as.integer(julian(raw$date))

						#Duplicate records
								raw$Unique<-paste(raw$vessel_cd,as.character(raw$haul_date),sep = " ")
								Depth.dif<-sapply(split(raw$depth,raw$Unique),function(x){length(unique(x))}) #Should only be one record: LFA 34 - 1230 2001-11-30 (0.5 SD) 
								
								a<-names(Depth.dif)[which(Depth.dif==2)] #Should only be one record: LFA 34 - 1230 2001-11-30 (0.5 SD)
								dups<-raw[which(raw$Unique %in% a),]
								
								if(nrow(dups)>1) {
									write.csv(dups, file=file.path(fchecks,paste(y,'duplicated.records.csv',sep=""))) 
									print('duplicate records saved')
									} else {
								print('No duplicates')
									}

						#inout of lfa / grid polygons...

						#number of individuals are reasonable

						#etc etc etc

						#once done all the cleaning

						save(clean,file=file.path(ffout,paste(y,'.rdata',sep="")))
						print(y)
					}

			}


			#default is to load the clean data
			out = NULL
					for(y in Y) {
						load(file.path(ffout,paste(y,'.rdata',sep="")))
						out = rbind(out,clean)
					}
			return(out)
		

		}
	}