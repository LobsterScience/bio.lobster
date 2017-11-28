#' @export
CPUEModelData = function(p){

	logs = lobster.db("process.logs")
	vlogs = lobster.db("process.vlog")

	TempModel = TempModel()

	cpue.data = list()

	for( i in 1:length(p$subareas)){

		tmp = subset(logs,subarea == p$subareas[i],c("DATE_FISHED","SYEAR","TOTAL_WEIGHT_KG","NUM_OF_TRAPS","subarea","GRID_NUM"))

		cpue.data[[i]] = tmp

	}





}