##Length Weight Request
require(tidyverse)

p = bio.lobster::load.environment()
#la()
p = bio.lobster::load.environment()

figdir = file.path(project.datadirectory("bio.lobster","requests","FSRS",p$current.assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )


sexes=c(1:3)
CL=1:27
MLW=data.frame(matrix(NA, nrow = 27, ncol = 3))
MLW[,1]=CL
names(MLW)=c("bin", "sex", "wt.grm")

for (s in sexes){
    m=MLW
    B = lobLW(CL, sex=s, fsrs=T)
    MLW<-as.data.frame(cbind(CL,s,B))
    names(MLW)=c("bin","sex","wt.grm")
    #if (s==1) {names(MLW)=c("bin","male.grams")}
    #if (s==2) {names(MLW)=c("bin","female.grams")}
    #if (s==3) {names(MLW)=c("bin","berried.grams")}
    #MLW$wt.grm=as.numeric(MLW$wt.grm)
        MLW=rbind(m, MLW)
    MLW=MLW[!is.na(MLW$sex),]
    rm(m)
  }
MLW$wt.grm=as.numeric(as.character(MLW$wt.grm))
MLW$s.bin=NA
MLW$s.bin=paste(MLW$sex, MLW$bin, sep=":")
#MLW$s.bin=as.factor(MLW$s.bin)
#MLW$wt.grm=format(MLW$wt.grm,scientific=FALSE)


lobster.db('fsrs') #fsrs.redo if needed

fsrs$s.bin=NA
fsrs$s.bin=paste(fsrs$SEX, fsrs$SIZE_CD, sep=":")
#fsrs$s.bin=as.factor(fsrs$s.bin)
fsrs$u.trap=paste(fsrs$VESSEL_CD, fsrs$HAUL_DATE, fsrs$TRAP_NO, sep=":")

yrs=c(2023)
fs=fsrs[fsrs$SYEAR %in% yrs,]
num.of.traps=length(unique(fs$u.trap))

fs=fs[is.finite(fs$SEX),] #removes empty traps
fs=fs[is.finite(fs$SIZE_CD),] #removes empty traps

merged_df <- merge(fs, MLW, by = "s.bin", all.x = TRUE, all.y=FALSE) # Merge based on SEx & SIZE_CD fields (combined into s.bin) 
kept=merged_df[merged_df$SEX %in% c(1:2) & merged_df$SHORT %in% "0" & merged_df$V_NOTCHED %in% "0",]

tot.kg=sum(kept$wt.grm/1000)
cpu=tot.kg/num.of.traps
print(paste0("the mean CPUE in FSRS traps for ", yrs, " was ",round(cpu, 2), " kg/trap"))

#How many days hauled by license
day.haul=aggregate(fs, HAUL_DATE~VESSEL_CD, FUN=function(x) length(unique(x)))
days.hauled=mean(day.haul$HAUL_DATE)
days.hauled