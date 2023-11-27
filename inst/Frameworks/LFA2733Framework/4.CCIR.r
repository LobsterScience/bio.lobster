#9. CCIR.r

require(bio.lobster)
#load_all('~/bio/bio.ccir')
require(bio.ccir)

require(bio.utilities)
require(car)
require(rstan)
la()

redo.data = FALSE

if(redo.data) {
	lobster.db('fsrs.redo') #this requires ODBC connection
	lobster.db('ccir.redo') #this does not
}

#Groupings for seasons, and regions -- Hard coded---Need to be manually updated
inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
 load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
 load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
lobster.db('ccir')

logs = lobster.db('process.logs')
logs$LFA = ifelse(logs$LFA == '31A','31a',logs$LFA)
logs$LFA = ifelse(logs$LFA == '31B','31b',logs$LFA)


dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings, size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn

#dat = ccir_compile_data(x = ccir_data,area.defns = Groupings, size.defns = inp, season.defns = Seasons, sexs = c(1,2) #Not for stock assessment, but for Simulation models
redo.models =T 
if(redo.models) {
							#model options
					out.normal = list() #this is the Claytor and Allard Model
								attr(out.normal,'model') <- 'normal'
					out.binomial = list()
								attr(out.binomial,'model') <- 'binomial'
					out.binomial.fishery.land = list()
								attr(out.binomial.fishery.land,'model') <- 'binomial.fishery.land'
					out.beta = list()
								attr(out.beta,'model') <- 'beta'
					out.logit.binomial = list()
									attr(out.logit.binomial,'model') <- 'logit.binomial'
					out.logit.binomial.cov = list()
									attr(out.logit.binomial.cov,'model') <- 'logit.binomial.cov'
					mm = c('binomial')#,'binomial.fishery.land')

for(i in 1:length(dat)) {
print(i)
				ds = dat[[i]]

				if(any(mm == 'normal')){
				ds$method = 'normal'
				x = ccir_stan_run(dat = ds)
				ccir_stan_plots(x,type='predicted')
			 	ccir_stan_plots(x,type='exploitation')
			 	ccir_stan_plots(x,type='traceplot')
			 	ccir_stan_plots(x,type='prior.posterior')
#			 	if(length(ds$p) == length(ds$Temp))ccir_stan_plots(x,type='Temp.by.Expl')1
				
			  out.normal[[i]] <- ccir_stan_summarize(x)
				}


		if(any(mm == 'logit.binomial')){
				ds$method = 'logit.binomial'
				x = ccir_stan_run(dat = ds)
				ccir_stan_plots(x,type='predicted')
			 	ccir_stan_plots(x,type='exploitation')
			 	ccir_stan_plots(x,type='traceplot')
			 	ccir_stan_plots(x,type='prior.posterior')
#			 	if(length(ds$p) == length(ds$Temp))ccir_stan_plots(x,type='Temp.by.Expl')
			  out.logit.binomial[[i]] <- ccir_stan_summarize(x)
				}
		if(any(mm == 'binomial')){
				ds$method = 'binomial'
				x = ccir_stan_run(dat = ds,save=F)
			#	ccir_stan_plots(x,type='predicted')
			 #	ccir_stan_plots(x,type='exploitation')
			 #	ccir_stan_plots(x,type='traceplot')
			 #	ccir_stan_plots(x,type='prior.posterior')
#			 	if(length(ds$p) == length(ds$Temp))ccir_stan_plots(x,type='Temp.by.Expl')
			  out.binomial[[i]] <- ccir_stan_summarize(x)
				}

		if(any(mm == 'binomial.fishery.land')){
				ds$method = 'binomial.fishery.land'
				if(ds$Yr>2003) {
				if(!all(is.na(ds$land))){	
				x = ccir_stan_run(dat = ds)
				ccir_stan_plots(x,type='predicted')
			 	ccir_stan_plots(x,type='exploitation')
			 #	ccir_stan_plots(x,type='traceplot')
			 #	ccir_stan_plots(x,type='prior.posterior')
#			 	if(length(ds$p) == length(ds$Temp))ccir_stan_plots(x,type='Temp.by.Expl')
			  out.binomial.fishery.land[[i]] <- ccir_stan_summarize(x)
						}
					}
				}


		if(any(mm == 'logit.binomial.cov')){
				ds$method = 'logit.binomial.cov'
				x = ccir_stan_run(dat = ds)
				ccir_stan_plots(x,type='predicted')
			 	ccir_stan_plots(x,type='exploitation')
			 	ccir_stan_plots(x,type='traceplot')
			 	ccir_stan_plots(x,type='prior.posterior')
#			 	if(length(ds$p) == length(ds$Temp))ccir_stan_plots(x,type='Temp.by.Expl')
				
			  out.logit.binomial.cov[[i]] <- ccir_stan_summarize(x)

			if(any(mm == 'beta')){
				ds$method = 'beta'
				x = ccir_stan_run(dat = ds)
				ccir_stan_plots(x,type='predicted')
				ccir_stan_plots(x,type='exploitation')
				ccir_stan_plots(x,type='traceplot')
				ccir_stan_plots(x,type='prior.posterior')
				 out.beta[[i]] <- ccir_stan_summarize(x)
				}

				}
			}
			#ouBet = ccir_collapse_summary(out.beta)
			#ouLoBin = ccir_collapse_summary(out.logit.binomial)
			
			ouBin = ccir_collapse_summary(out.binomial)
			attr(ouBin,'model') <- 'binomial' 
			
			ouBinFish = ccir_collapse_summary(out.binomial.fishery.land)
			attr(ouBinFish,'model') <- 'binomial.fishery.land'
			
			#save(ouBet,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBetaModels.rdata'))
			save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels.rdata'))
			save(ouBinFish,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialFisheryModels.rdata'))
			#save(ouLoBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledlogitBinomialModels.rdata'))
			
			
		}
#load each ccir stan summarize for binomials

load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialFisheryModels.rdata'))

outs = list()
	kl = unique(ouBin$Grid) 
	for(i in 1:length(kl)) {
		u = subset(ouBin, Grid == kl[i])
		outs[[i]] <- ccir_timeseries_exploitation_plots(u)
	}

#	iu = c(grep('LFA 27',outs),grep('LFA 33',outs))
#	outs = outs[-c(iu)]
	o = do.call(rbind,outs)
	ooo = subset(o,select=c(Yr,ERfl,ERfm,ERfu,LFA))
#remove lfa 33 and 27 as treated as one

#Split LFAs combined for one exploitation rate
outs = list()
SLfa = c(27,33)
lan = lobster.db('process.logs')
for(i in 1:length(SLfa)) {
	u = subset(ouBin, LFA == SLfa[i])
	g = unique(u$Grid)
	g = strsplit(g,"\\.")
	o = aggregate(WEIGHT_KG~SYEAR,data=subset(lan,GRID_NUM %in% g[[1]]),FUN=sum)
	names(o)[2] = g[[1]][1]
	o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(lan,GRID_NUM %in% g[[2]]),FUN=sum)
	names(o2)[2] = g[[2]][1]
	o = merge(o,o2)
	names(o)[1] = 'Yr'
	outs[[i]] <- ccir_timeseries_exploitation_plots(u,combined.LFA=T,landings=o)
	}
	oo = do.call(rbind,outs)

oi = rbind(ooo,oo)
save(oi,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))
###Model comparison time series exploitation plots

	kl = unique(ouBinFish$Grid) 
	for(i in 1:length(kl)) {
		u = subset(ouBin, Grid == kl[i])
		w = subset(ouBinFish, Grid == kl[i])
		ccir_timeseries_exploitation_plots_model_comparison(u,w)
	}


##Exploitation and Changing Landings

lSe = lobster.db('seasonal.landings')
lSe$YR = as.numeric(substr(lSe$SYEAR,6,9))
lA = lobster.db('annual.landings')

#from above
		o = rbind(ooo,oo)
		o$LFA = ifelse(o$LFA == 'LFA 27 Combined','LFA 27',o$LFA)
		o$LFA = ifelse(o$LFA == 'LFA 33 Combined','LFA 33',o$LFA)


outs = do.call(rbind,outs)

x = outs[[1]]
y = subset(lA,select=c(YR,LFA27))
y$D1 = c(diff(y[,2]),0)
a = merge(x,y,by.x='Yr',by.y = 'YR')
with(a,plot(ERfm,LFA27))

x = outs[[2]]
y = subset(lSe,select=c(SYEAR,LFA33))
y$YR = as.numeric(substr(lSe$SYEAR,6,9))
y$D1 = c(diff(y[,2]),0)
a = merge(x,y,by.x='Yr',by.y = 'YR')
with(a,plot(ERfm,LFA33))



######NOT USED for FRAMEWORK FROM HERE DOWN######


#partial exploitation rates and logbook effort
logs = lobster.db('process.logs')
gg = dir(file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary'),full.names=T)

gg = gg[grep('Sex.1.5',gg)] #sexes combined
outs = list()
for(i in 1:length(gg)){
	print(i)
	load(gg[i])
	outs[[i]] = ccir_groupings_landings_effort(x=logs,ccir_model_summary=out)
}
	outs = rbindFill(outs)
	outs = toNums(outs,4:21)
	outs$CredInt = outs[,'ERu.97.5%'] - outs[,'ERl.2.5%']


#Partial exploitation rates
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
          require(devtools)
          load_all('~/git/bio.ccir/')

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
