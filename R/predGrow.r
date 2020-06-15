#' @title predGrow
#' @description growth matrix loading from bergeron 2011
#' @export
predGrow <- function(p,mat=F) {
	grow=NULL
	with(p,{
	if(mat) {	
		g = dir(file.path(project.datadirectory('bio.lobster'),'data'))
		i = grep(area,g)
		j = grep('Growth',g)
		i = intersect(i,j)
		g = g[i]
			if(length(g)!=2) stop('Need to have two growth matrices one male one female')
		
		i = grep('Fem',g)
		load(file=file.path(project.datadirectory('bio.lobster'),'data',g[i]))
		g = g[-i]
		fGM =a 

		load(file=file.path(project.datadirectory('bio.lobster'),'data',g))
		mGM = a
		return(list(male = mGM, female=fGM))
		}
	if(!mat) {
		browser()
		if(area=='BoF'){
			grow = data.frame(sex=c(1,1,1,1,1,2,2,2,2,2),cll=c(20,40,60,80,100,120,140,20,40,60,80,100,120,140),clu=c(39,59,79,99,119,139,300,39,59,79,99,119,139,300),s=c(0.12,0.14,0.18,0.17,0.16,0.17,0.15,0.12,0.14,.17,.16,.12,.09,.06)) #smallest sizes come from GOM growth --no BOF growth data for small 
		}
		if(area=='GOM'){
			grow = data.frame(sex=c(1,1,1,1,1,2,2,2,2,2),cll=c(20,40,60,80,100,20,40,60,80,100),clu=c(39,59,79,99,300,39,59,79,99,300),s=c(.12,.14,.16,.15,.16,.13,.14,.15,.14,.13))
		}	
		return(grow)
		}
	
	})
}