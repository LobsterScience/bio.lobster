##Updated CCIR and Recruitment Trap CPUEs for 2019-2020 data


p = bio.lobster::load.environment()
la()

assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!
p$current.assessment.year = p$current.assessment.year - 1 ########### check the year ############### !!!!!!!!!!!


# define place for figures to go
figdir = file.path(project.datadirectory("bio.lobster"),"figures","Updates","LFA33", assessment.year)
dir.create(figdir)

p$lfas = "33" # specify lfas for data summary
#p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary
# CCIR ###############

lobster.db('ccir.redo')


load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
#lobster.db('ccir')

logs = lobster.db('process.logs')

require(bio.ccir)
require(rstan)

#sept 22
load('~/tmp/2020_ccir_data.rdata')
ccir_data = subset(ccir_data,YEAR<=2021)
inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))

load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[7], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn

out.binomial = list()
attr(out.binomial,'model') <- 'binomial'
for(i in 1:length(dat)) {
  ds = dat[[i]]
  ds$method = 'binomial'
  x = ccir_stan_run(dat = ds,save=F)
  out.binomial[[i]] <- ccir_stan_summarize(x)
}
ouBin = ccir_collapse_summary(out.binomial)
ouBin$LFA = NA
ouBin$LFA[1:20] <- '33W'
ouBin$LFA[1:20] <- '33E'

attr(ouBin,'model') <- 'binomial'
#ouBin$Yr = ouBin$Yr +1
save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))
#load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels2732.rdata'))

u = ouBin
g = unique(u$Grid)
g = strsplit(g,"\\.")
o = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[1]]),FUN=sum)
names(o)[2] = g[[1]][1]
o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[2]]),FUN=sum)
names(o2)[2] = g[[2]][1]
o = merge(o,o2)
names(o)[1] = 'Yr'
oo <- ccir_timeseries_exploitation_plots(ouBin,combined.LFA=T,landings=o)
