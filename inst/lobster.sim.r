require(bio.lobster)

p = bio.lobster::load.environment()

p$LS = 82.5					# legal size (mm)

p$StartPop = 1000
p$nt = 20					# number of timesteps
p$lens = seq(50,200,5)		# carapace length bins (mm)
p$timestep = 365  			# in days
p$GrowthFactorMean = 1.15	
p$GrowthFactorSD = 0.05


res = simMolt(p)

round(res$finalPop)
