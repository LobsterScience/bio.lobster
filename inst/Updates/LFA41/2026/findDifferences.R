s25 <- read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2025/indicators/NEFSC.spring.restratified.commercial.csv")
s26 <- read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/NEFSC.spring.restratified.commercial.csv")

range(s25$w.Yst, na.rm=TRUE)
range(s26$w.Yst, na.rm=TRUE)

summary(s25$w.Yst)
summary(s26$w.Yst)

max(s25$w.Yst, na.rm=TRUE) / max(s26$w.Yst, na.rm=TRUE)


f25 <- read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2025/indicators/NEFSC.fall.restratified.commercial.csv")
f26 <- read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/assessments/LFA41/2026/indicators/NEFSC.fall.restratified.commercial.csv")

range(f25$w.Yst, na.rm=TRUE)
range(f26$w.Yst, na.rm=TRUE)

summary(f25$w.Yst)
summary(f26$w.Yst)

max(f25$w.Yst, na.rm=TRUE) / max(f26$w.Yst, na.rm=TRUE)


spring.comp <- merge(
  s25[,c("yr","w.Yst")],
  s26[,c("yr","w.Yst")],
  by="yr",
  suffixes=c(".2025",".2026")
)

spring.comp$ratio <- spring.comp$w.Yst.2026 / spring.comp$w.Yst.2025

summary(spring.comp$ratio)


fall.comp <- merge(
  f25[,c("yr","w.Yst")],
  f26[,c("yr","w.Yst")],
  by="yr",
  suffixes=c(".2025",".2026")
)

fall.comp$ratio <- fall.comp$w.Yst.2026 / fall.comp$w.Yst.2025

summary(fall.comp$ratio)


head(s25)
head(s26)

tail(s25)
tail(s26)


comp <- merge(
  s25[,c("yr","w.Yst")],
  s26[,c("yr","w.Yst")],
  by="yr",
  suffixes=c(".25",".26")
)

head(comp)

comp$ratio <- comp$w.Yst.26 / comp$w.Yst.25

plot(comp$yr, comp$ratio)
abline(h=median(comp$ratio, na.rm=TRUE), col="red")

subset(comp, yr == 2010)
subset(comp, yr == 2020)
subset(comp, yr == 2025)



f1 <- readLines("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/R/nefsc.analysis.R")

f2 <- readLines("C:/Users/HowseVJ/Documents/GitHub/bio.lobster/R/nefsc.analysis_vh.R")

diffs <- which(f1 != f2)

diffs

install.packages("diffobj")
library(diffobj)

diffChr(f1, f2)
