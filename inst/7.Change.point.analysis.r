#Change point analysis

require(bio.lobster)
require(bcp)
#using output from 6.stratified.analysis

aout= nefsc.analysis(DS='stratified.estimates',p=p)

i = which(aout$n.yst.se>0)

inv.cv = mean(aout$n.yst[i] / aout$n.yst.se[i]) #used as the parameter representing prior probability on change and is treated as the signal to noise ratio

b = bcp(aout$n.yst, w0 = 0.2, p0 = 0.2)

plot(b)

a = dicusum(aout$n.yst,1:30,k = 0.3)

plot(a)