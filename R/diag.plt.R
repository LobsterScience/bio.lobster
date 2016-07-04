#' @export
diag.plt <- function(data, years=1981:2007, name="diag",graphic="R",path=''){

# diagnostics
if(graphic=="pdf")pdf(file.path(path,paste(name, "pdf", sep=".")), width = 8.5, height = 11, pointsize=16)
if("sPresid"%in%names(data$median))	{
if(graphic=="R")windows(width = 8.5, height = 11)
if("sUresid"%in%names(data$median))par(mfrow=c(4, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
else par(mfrow=c(3, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))

	ymin <- min(sapply(2:length(years), function(i){with(data$sims.list, quantile(sPresid[,i-1], 0.025))}))
	ymax <- max(sapply(2:length(years), function(i){with(data$sims.list, quantile(sPresid[,i-1], 0.975))}))
	plot(years, data$median$sPresid, las = 1, pch = 16, ylab = "",  ylim = c(-3, 3), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
	axis(4, lab = F, tcl = -0.3)
	mtext("Process", 2, 2.5)
	mtext("Residuals (standardized)", 3, 0.5, cex =1.25)
	abline(h = 0, lty = 2)
	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(sPresid[,i], 0.025), quantile(sPresid[,i], 0.975))))})
	
	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIresid[,i], 0.025))}))
	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIresid[,i], 0.975))}))
	plot(years, data$median$sIresid, las = 1, pch = 16, ylab = "",  ylim = c(-3, 3), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
	axis(4, lab = F, tcl = -0.3)
	mtext("Fully-recruited Biomass", 2, 2.5)
	abline(h = 0, lty = 2)
	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(sIresid[,i], 0.025), quantile(sIresid[,i], 0.975))))})

	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIRresid[,i], 0.025))}))
	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIRresid[,i], 0.975))}))
	plot(years, data$median$sIRresid, las = 1, pch = 16, ylab = "",  ylim = c(-3, 3), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
	axis(4, lab = F, tcl = -0.3)
	mtext("Recruit Biomass", 2, 2.5)
	abline(h = 0, lty = 2)
	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(sIRresid[,i], 0.025), quantile(sIRresid[,i], 0.975))))})

	if("sUresid"%in%names(data$median)){
		ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sUresid[,i], 0.025))}))
		ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sUresid[,i], 0.975))}))
		plot(years, data$median$sUresid, las = 1, pch = 16, ylab = "",  ylim = c(-3, 3), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3,xlim=range(years))
		axis(4, lab = F, tcl = -0.3)
		mtext("CPUE", 2, 2.5)
		abline(h = 0, lty = 2)
		sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(sUresid[,i], 0.025), quantile(sUresid[,i], 0.975))))})
	}
}
if("Presid"%in%names(data$median))	{
if(graphic=="R")windows(width = 8.5, height = 11)
if("sUresid"%in%names(data$median))par(mfrow=c(4, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
else par(mfrow=c(3, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
	

	ymin <- min(sapply(2:length(years), function(i){with(data$sims.list, quantile(Presid[,i-1], 0.025))}))
	ymax <- max(sapply(2:length(years), function(i){with(data$sims.list, quantile(Presid[,i-1], 0.975))}))
	plot(years, data$median$Presid, las = 1, pch = 16, ylab = "",  ylim = c(-1, 1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
	axis(4, lab = F, tcl = -0.3)
	mtext("Process", 2, 2.5)
	mtext("Residuals", 3, 0.5, cex =1.25)
	abline(h = 0, lty = 2)
	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(Presid[,i], 0.025), quantile(Presid[,i], 0.975))))})
	
	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(Iresid[,i], 0.025))}))
	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(Iresid[,i], 0.975))}))
	plot(years, data$median$Iresid, las = 1, pch = 16, ylab = "",  ylim = c(-1, 1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
	axis(4, lab = F, tcl = -0.3)
	mtext("Fully-recruited Biomass", 2, 2.5)
	abline(h = 0, lty = 2)
	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(Iresid[,i], 0.025), quantile(Iresid[,i], 0.975))))})

	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(IRresid[,i], 0.025))}))
	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(IRresid[,i], 0.975))}))
	plot(years, data$median$IRresid, las = 1, pch = 16, ylab = "",  ylim = c(-1, 1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
	axis(4, lab = F, tcl = -0.3)
	mtext("Recruit Biomass", 2, 2.5)
	abline(h = 0, lty = 2)
	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(IRresid[,i], 0.025), quantile(IRresid[,i], 0.975))))})

	if("sUresid"%in%names(data$median)){
		ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sUresid[,i], 0.025))}))
		ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sUresid[,i], 0.975))}))
		plot(years, data$median$sUresid, las = 1, pch = 16, ylab = "",  ylim = c(-1, 1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3,xlim=range(years))
		axis(4, lab = F, tcl = -0.3)
		mtext("CPUE", 2, 2.5)
		abline(h = 0, lty = 2)
		sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), with(data$sims.list, c(quantile(sUresid[,i], 0.025), quantile(sUresid[,i], 0.975))))})
	}
}
if(graphic!="R")dev.off()
	
}	