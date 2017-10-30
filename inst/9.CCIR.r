#9. CCIR.r

require(bio.lobster)
load_all('~/git/bio.ccir')
require(bio.utilities)
require(car)
require(rstan)
la()
if(redo.data) {
	lobster.db('fsrs.redo') #this requires ODBC connection
	lobster.db('ccir.redo') #this does not
}


#Need to use the extended versions of the script for CCIR. This 'extension' provides a correction to exploitation rates when the minimum legal sizes are increasing. If this is not applied then exploitation rates will increase simply due to the decreased number of lobsters available in larger size classes.
#the work horses behind this were developed by Jaques Allard and Ross Claytor. Have had to improve data flow and streamline outputs to allow for integration into current data streams and analyses.

#there are a few columns of data that are expected by the CCIR extended.
#DateField  --> Date 
#RefNotExtended --> Reference group -- not extended to account for the changing MLS
#RefAndExtended --> This is the reference group and includes those lobsters that were exploitable but became sublegal as MLS changed == typically this size was chosen at 76-80mm or size 9 (but not expl)
#NONRefAndExtended --> these are included in the exploitable size class but are not exploited - legal = 1
#Exp --> fully expolitable size class used as our index between upper and low bounds

#LFA 27 
#FSRS codes
# SIZE_CD MIN_S MAX_S
#   0     0    50
#   1     0    10
#   2    11    20
#   3    21    30
#   4    31    40
#   5    41    50
#   6    51    60
#   7    61    70
#   8    71    75
#   9    76    80
#  10    81    90
#  11    91   100
#  12   101   110
#  13   111   120
#  14   121   130
#  15   131   300
#  16   100   300

					
#LFA 27 <- lower bounds on ref class = 8, upper bounds on ref class will change over time from 8 to 10 as MLS changes; Exp class lower bound fixed at 9 and upper bound at 10.5

#Need to Run CCIR on a reginonal (sub LFA) basis, one sex at a time. Can combine exploitation rates as weighted average across regions by landings
#
#define your parameter file for subsetting the data

#control list for ccir estimates

inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
 load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
lobster.db('ccir')

dat = ccir_compile_data(x = ccir_data,area.defns = Groupings, size.defns = inp)

out.binomial = list()
			attr(out.binomial,'model') <- 'binomial'
out.beta = list()
			attr(out.beta,'model') <- 'beta'

for(i in 93:length(dat)) {
	ds = dat[[i]]
	ds$method = 'binomial'
	x = ccir_stan_run(dat = ds)
	ccir_stan_plots(x,type='predicted')
 	ccir_stan_plots(x,type='exploitation')
 	ccir_stan_plots(x,type='traceplot')
 	ccir_stan_plots(x,type='prior.posterior')
 	if(length(ds$p) == length(ds$Temp))ccir_stan_plots(x,type='Temp.by.Expl')

  out.binomial[[i]] <- ccir_stan_summarize(x)

	ds$method = 'beta'
	x = ccir_stan_run(dat = ds)
	ccir_stan_plots(x,type='predicted')
 	ccir_stan_plots(x,type='exploitation')
 	ccir_stan_plots(x,type='traceplot')
 	ccir_stan_plots(x,type='prior.posterior')

  out.beta[[i]] <- ccir_stan_summarize(x)
	}

ouBet = ccir_collapse_summary(out.beta)
ouBin = ccir_collapse_summary(out.binomial)

save(ouBet,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','comiledBetaModels.rdata'))
save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','comiledBinomialModels.rdata'))

#load each ccir stan summarize for binomials

			dd = file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary')
			fl = dir(dd)
			 i = grep('binomial',fl)
			 j = grep('LFA',fl)
			fl = fl[intersect(i,j)]

			out = list()
				for( i in 1:length(fl)){
					out[[i]] <- load(file.path(dd,fl[i]))
				}

## High correlation between recruit:exp ratio and temp-- to ensure that this change is due to fishing and not temp, compare the start of year ratios and temp within LFAs

	#x is the list of out from ccir_stan_summarize
		 LFA = unlist(t(sapply(dat,"[",'LFA')))
		 Te = sapply(t(sapply(dat,'[','Temp')),function(x) mean( x[1:5]))
         Sex = unlist(t(sapply(dat,"[",'Sex')))
         N =  sapply(t(sapply(dat,'[','N')),function(x) sum( x[1:5]))
         E =  sapply(t(sapply(dat,'[','E')),function(x) sum( x[1:5]))
		 Gr = unlist(sapply(sapply(dat,"[",'Grid'),function(x) paste(x,collapse=".")))
		da = data.frame(LFA=LFA, Grid = Gr,Sex=Sex,Temp = Te, N=N, E= E,R = N-E, p = E/N)		 
		da$id = paste(da$LFA,da$Grid,da$Sex,sep="-")

		ij = unique(da$id)

		for(i in 1:length(j)){
			hy = subset(da,id==ij[i])
			x11()
			plot(hy$Te,hy$p,type='p',main = ij[i])
			legend('topleft',paste('correlation =', round(cor.test(hy$Temp,hy$p)[[4]],2)),bty='n',cex=0.8)
			}



##Tester for Runs
          require(rstan))
          load('~/git/bio.ccir/inst/trial.data.rdata')

          Total = apply(SublegalLegal,1,sum)
          CumLegal <- cumsum(SublegalLegal[,2])
          CumLegal <- c(0,CumLegal[1:length(CumLegal)-1])
            
          p = SublegalLegal[,2]/Total
          n = length(p)

          i = which(p==1)
          p[i] = 0.99
          i = which(p==0)
          p[i] = 0.01
          dts = as.Date(as.numeric(rownames(SublegalLegal)),origin = "1970-01-01")

          n = length(p)
          newp = seq(0.01,0.99,by=0.01) 
          np = length(newp)

          dat = list(n = n, Cuml=CumLegal/CumLegal[n], p=as.numeric(p), newp = newp , np=np ,dates = dts, Nrec = sum(SublegalLegal[,1]),Nexp = sum(SublegalLegal[,2]))

x = ccir_stan_run(dat = dat, method = 'beta')


#Can I recapture the parameters

  a=3
  b=-2.67
  d=1
  x=seq(0.01,0.99,0.01)
  phi=9.12
  
  a = simulateCCIR(a,b,d,phi,x)

       dat = list(n = nrow(a), Cuml=a[,1], p=a[,2], newp = a[,1] , np=nrow(a) ,dates = 1:nrow(a), Nrec = 11,Nexp = 11)

x = ccir_stan_run(dat = dat, method = 'beta')
