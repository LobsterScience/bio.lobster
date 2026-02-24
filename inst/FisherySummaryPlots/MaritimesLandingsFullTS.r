
require(bio.lobster)
require(PBSmapping)
require(tidyr)
require(dplyr)
la()
a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')
d = lobster.db('historic.landings.county')
d = aggregate(LANDINGS_MT~SYEAR,data=d,FUN=sum)
names(d)[c(1,2)]=c('YR','LAND')


b$YR = as.numeric(substr(b$SYEAR,6,9))
aa = subset(a,YR<1976,select=c(YR,LFA27,LFA29,LFA30, LFA31,LFA32,LFA33,LFA34,LFA35,LFA36,LFA38))
b = subset(b,YR>1975 & YR<=2025,select=c(YR,LFA33,LFA34,LFA35,LFA36,LFA38))

aa <- aa %>%
  pivot_longer(
    cols = -YR,                          # everything except the id column
    names_to = "LFA",                    # new column for the LFA identifier
    names_prefix = "LFA",                # drop the "LFA" text from names
    values_to = "LAND"                  # new column holding the values
  ) %>%
  mutate(LFA = as.integer(LFA)) %>%
  group_by(YR) %>%
  summarise(LAND = sum(LAND, na.rm = TRUE), .groups = "drop")

  # optional: make LFA numeric

bb = b %>%
  pivot_longer(
    cols = -YR,                          # everything except the id column
    names_to = "LFA",                    # new column for the LFA identifier
    names_prefix = "LFA",                # drop the "LFA" text from names
    values_to = "LAND"                  # new column holding the values
  ) %>%
  mutate(LFA = as.integer(LFA)) %>%
  group_by(YR) %>%
  summarise(LAND = sum(LAND, na.rm = TRUE), .groups = "drop")


da  =bind_rows(d,aa,bb,)

daa = aggregate(LAND~YR,data=da,FUN=sum)
plot(daa)
