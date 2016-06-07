waa.plt <- function(waa.t, waa.tm1, waa.lm,nr=1,graphic="R",path='',wd=5,ht=5){
if(graphic=="pdf") pdf(file.path(path,"waa.pdf"), width =wd, height = ht)
if(graphic=="R") windows(width = wd, height = ht)
	plot(waa.tm1, waa.t, ylim=c(0,max(waa.t,na.rm=T)),xlim=c(0,max(waa.tm1,na.rm=T)),las = 1, xlab = "Weight at age t-1", ylab = "Weight at age t")

	abline(waa.lm)
	axis(4, lab = F)
	b1 <- round(waa.lm$coef[1], 2)
	b2 <- round(waa.lm$coef[2], 2)
	text(5, 45, substitute(alpha * " = " * b1 * ", " * rho * " = " * b2), adj = 0, cex = 1.25)

if(graphic!="R")dev.off()
}


#[3:15]