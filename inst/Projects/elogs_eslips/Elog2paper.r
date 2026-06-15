require(bio.lobster)
require(devtools)
require(ggplot2)
la()

p =list()
p$yrs=2025
#lobster.db('logs.redo',pH=p)

db.setup()
el = connect.command(con,'select * from marfissci.lobster_elog_20251112')
lobster.db('logs')
logs$DATE_FISHED = as.Date(logs$DATE_FISHED) 

logs$ID = paste(logs$LICENCE_ID,logs$DATE_FISHED)
el$ID = paste(el$LICENCE_ID,el$DATE_FISHED)
el = bio.utilities::rename.df(el,'CUNNER_WEIGHT_WEIGHT_GRID_B','CUNNER_WEIGHT_GRID_B')

lo = subset(logs, ID %in% unique(el$ID))
out=list()
id = unique(lo$ID)
for(i in 1:length(id)){
    k = subset(lo,ID %in% id[i])
    j = subset(el,ID %in% id[i])
    k$src = 'paper'
    j$src = 'elec'
    jk = rbind(k,j)
    m = c(grep('GRID',names(jk)),grep('WEIGHT_LBS',names(jk)),grep('NUM_OF_TRAPS',names(jk)))
    
    diff <- as.numeric(as.character(unlist(jk[1, m]))) -
      as.numeric(as.character(unlist(jk[2, m])))
    names(diff) = names(jk)[m]
    oo = c(id[i],diff)
    names(oo)[1] = 'ID'
    out[[i]] = oo
}
out = as.data.frame(do.call(rbind,out))
out = bio.utilities::toNums(out,cols=2:ncol(out))

los = subset(lo,select=c(ID,VR_NUMBER, VESSEL_NAME,SUBMITTER_NAME,LICENCE_ID,LFA, DATE_FISHED))
out1 = merge(los,out)

ggplot(out1,aes(x=WEIGHT_LBS))+geom_histogram()+facet_wrap(~LFA,scales='free_y')
