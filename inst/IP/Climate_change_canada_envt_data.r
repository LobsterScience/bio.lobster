#Climate change canada envt data
 
##https://www.canada.ca/en/environment-climate-change/services/climate-change/canadian-centre-climate-services/display-download/advanced-tools.html

##https://www.canada.ca/en/environment-climate-change/services/climate-change/canadian-centre-climate-services/display-download/technical-documentation-adjusted-climate-data.html#toc5

 options(stringsAsFactors=F)

require(rvest)
require(bio.utilities)
require(imputeTS)

url = 'https://dd.weather.gc.ca/climate/observations/daily/csv/NS/'
ss = read_html(url)
out =ss %>% html_nodes("a") %>% html_text() 
#this is hart island inside of chedabucto bay

g = out[(grepl('8202318',out))]
savr = '~/tmp/HartIsl'
 dir.create(savr)
for( i in 1:length(g)){
 download.file(file.path(url,g[i]),destfile=file.path(savr,g[i]))
}

x = list()
for(i in 1:length(g)){
	x[[i]] = read.csv(file.path(savr,g[i]),header=F)
	x[[i]] = x[[i]][-1,]
}
out = do.call(rbind,x)
gr = read.csv(file.path(savr,g[i]),header=F)
names(out) = gsub(" ", "_", gr[1,])
out = toNums(out, c('Spd_of_Max_Gust_(km/h)','Dir_of_Max_Gust_(10s_deg)')
#this turns all winds <30km/h to NA

out$WindSp = out[,'Spd_of_Max_Gust_(km/h)']
out$WindD = out[,'Dir_of_Max_Gust_(10s_deg)']

out$Date = as.POSIXct(out[,'Date/Time'])


x = ts(out$WidSp[out$Year>2010],frequency=365)
plot(decompose(x))


##compare with sable wind data

dr = file.path(project.datadirectory('bio.lobster'),'data','wind')
a = read.table(file=file.path(dr, 'sable_daily.txt'),skip=13,header=T)
la()
#meterological direction is where it comes from to where it is going with N as 0, first line of data states direction 62.5 which is ENE wind meterologically
#(and thus -ve u and -ve v)
# this data is incorrectly coded for direction (i.e. west is 270 instead of 0 as is typical)-- correcting the direction and recalculating U and Vspeeds  so need to use
# the wspd and dir (which are correct) to fix the Uspd and Vspd where Uspd is the xdirection and Vspd is the ydirection


a$mathDir = 270 - a$Dir

a$Uspd = a$Wspd * sin(a$mathDir*pi/180)
a$Vspd = a$Wspd * cos(a$mathDir*pi/180)

a$Date = as.Date(with(a, paste(Year, Mo, Dy, sep="-")), '%Y-%m-%d')


#
beaver island off 31b is 8200558