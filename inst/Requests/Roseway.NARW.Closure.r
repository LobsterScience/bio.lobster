
p = bio.lobster::load.environment()

                
 LobsterMap('SWN', labels= c('grid', 'lfa')) 

 #Import Roseway Critical Habitat 
 crit=read.csv("roseway.critical.csv")
 addLines(crit, col="green", lwd="3")
 
 #Import Roseway Closure 
 closure=read.csv("roseway.csv")
 addLines(closure, col="red", lty='dashed', lwd='2')
