require(bio.lobster)

p = bio.lobster::load.environment()

p$LS = 82.5

p$StartPop = 1000
p$nt = 20
p$lens = seq(50,200,5)
p$timestep = 365
p$GrowthFactorMean = 1.15
p$GrowthFactorSD = 0.05

p$F = c(rep(0,length(which(p$lens<p$LS))),rep(0.2,length(which(p$lens>p$LS))))
p$M = rep(0.1,length(p$lens))

res = simMolt(p)

round(res$finalPop)
