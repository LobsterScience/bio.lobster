
#### this script reads the marport text files, extracts useful data and converts this into a workable dataframe. 
#### For converting this ouput to Esonar format, use the Marport2Esonar script.

options(stringsAsFactors=F)
require(bio.lobster)
require(bio.utilities)
require(devtools)
load_all(file.path(code_root,"bio.lobster"))
fpath = file.path(project.datadirectory('bio.lobster'),'data','survey')

##### choose year of marport data to be summarized here. Adjust file paths if folder structure has changed.:
year = 2015

if(year == 2015){marpath= file.path(fpath,'2015','marport')}
if(year == 2016){marpath= file.path(fpath,'ILTS_2016_Marport')}


fm = dir(marpath,full.names=T)

gps = list()
sensors = list()
m = 0
for(i in fm) {
	print(i)
	m = m+1
	file = i
	out = LobsterMarport(file=file)
 	gps[[m]] = out[[2]]
 	sensors[[m]] =  out[[1]]
 	
}



gps = do.call(rbind,gps)
sensors = do.call(rbind,sensors)

####### output is to survey folder
save(sensors, file=file.path(project.datadirectory('bio.lobster'),'data','survey',paste0('marport.',year,'.rdata.')))
save(gps, file=file.path(project.datadirectory('bio.lobster'),'data','survey',paste0('marport.gps.',year,'.rdata.')))


##### To convert output to Esonar format, run the following:

marport2esonar(year=year)

