#' @export
simSummary = function(runs=c("LS70","LS75","Base","LS90"), lfas = c("27N", "29",  "30",  "31A", "31B", "32",  "33W"),lab=NULL, fn=NULL){

	a=c(0.000608,0.001413,0.00482)
	b=c(3.0583,2.8746,2.638)

	eggs = matrix(NA,length(runs),length(lfas))
	landnum = matrix(NA,length(runs),length(lfas))
	landkg = matrix(NA,length(runs),length(lfas))


		for(r in 1:length(runs)){
	if(!is.null(fn))load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0(fn,runs[r],".rdata")))
	if(is.null(fn))	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","sim",paste0("sim",lab,"Results",runs[r],".rdata")))

			for(i in 1:length(lfas)){
				lens = rlist$plist[[lfas[i]]]$lens
				bins = lens+2.5
				mwv = a[1]*bins^b[1]
				fwv = a[2]*bins^b[2]
				eggs[r,i] = sum(rlist$flist[[lfas[i]]]$totalEggs)
				landnum[r,i] = sum(rlist$flist[[lfas[i]]]$totalRemovals) + sum(rlist$mlist[[lfas[i]]]$totalRemovals)
				landkg[r,i] = (sum(sweep(rlist$flist[[lfas[i]]]$totalRemovals,2,FUN='*',fwv)) + sum(sweep(rlist$mlist[[lfas[i]]]$totalRemovals,2,FUN='*',mwv)))/1000
			}

		}
	

	list(eggs=eggs, landnum=landnum, landkg=landkg)



}