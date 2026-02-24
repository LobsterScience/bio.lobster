require(bio.lobster)
require(devtools)
require(bio.utilities)
require(dplyr)
require(tidyr)
la()
v =read.csv('C:/Users/cooka/Downloads/all.vent.data.csv')

v$id = paste(v$date,v$license,v$config.name)
vt = aggregate(trap.id~config,data=v,FUN=length)
v = subset(v,config %ni% c('46x2','44x2','49x1'))

va = aggregate(cbind(s14,s15,s16,s17a,s17b,s18,s19,s20)~trap.id+date+lfa+config,data=v,FUN=sum)
va$lfa = toupper(va$lfa)
va$year = lubridate::year(va$date)
dates = va
seasons = lobster.db('season.dates')
seasons$year = seasons$SYEAR
seasons$lfa = seasons$LFA

seasons$start_date <- as.Date(seasons$START_DATE)
seasons$end_date   <- as.Date(seasons$END_DATE)
dates$date         <- as.Date(dates$date)

cand <- merge(
  dates, seasons,
  by = c("lfa", "year"),
  all.x = TRUE, all.y = FALSE
)

in_season <- with(cand, date >= start_date & date <= end_date)

cand$day_of_season <- ifelse(
  in_season, as.integer(cand$date - cand$start_date) + 1L, NA_integer_
)
cand$sy = cand$year
cand$fconfig = as.factor(cand$config)
sublegal_cols <- c("s14", "s15", "s16", "s17a")
legal_cols    <- c("s17b", "s18", "s19", "s20")

cand <- cand %>%
  mutate(
    sublegal = rowSums(across(all_of(sublegal_cols), ~ tidyr::replace_na(., 0))),
    legal    = rowSums(across(all_of(legal_cols),    ~ tidyr::replace_na(., 0)))
  )

vaa  =aggregate(cbind(s14,s15,s16,s17a,s17b,s18,s19,s20)~day_of_season+lfa+sy,data=cand,FUN=sum)

vaa_noyr  =aggregate(cbind(s14,s15,s16,s17a,s17b,s18,s19,s20)~day_of_season+config,data=cand,FUN=sum)


library(VGAM)
library(splines)

vaa_noyr$total <- with(vaa_noyr, s14+s15+s16+s17a+s17b+s18+s19+s20)

fit_vgam <- vgam(
  cbind(s14,s15,s16,s17a,s17b,s18,s19,s20) ~ s(day_of_season)+config,
  family = multinomial(refLevel = 1),
  data = subset(vaa_noyr)
)

summary(fit_vgam)

newdat <- data.frame(day_of_season = seq(min(vaa_noyr$day_of_season), max(vaa_noyr$day_of_season), length.out = 60))
pred_p <- predict(fit_vgam, newdata = newdat, type = "response")

require(tidyr)
plot_df <- cbind(newdat, as.data.frame(pred_p)) %>%
  pivot_longer(
    cols = -day_of_season,
    names_to = "size_class",
    values_to = "prob"
  )

ggplot(plot_df, aes(x = day_of_season, y = prob, color = size_class)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Day of Season (DOS)",
    y = "Probability",
    color = "Size class",
    title = "Fitted size-class probabilities over day of season"
  ) +
  theme_minimal(base_size = 13)


################
##rather than sizes do it as classes

cand$day_of_season = as.numeric(cand$day_of_season)

vaa_noyr  =aggregate(cbind(sublegal,legal)~day_of_season+config,data=cand,FUN=sum)

vaa_noyr$total <- with(vaa_noyr, sublegal+legal)

cand = subset(cand,!is.na(day_of_season))
cand$total = cand$sublegal+cand$legal
require(mgcv)
cand$fconfig = as.factor(cand$config)
fit_gam <- gam(
  cbind(sublegal,legal) ~ fconfig+ (day_of_season*fconfig),
  family = binomial(link='logit'),   # 1 = 'small' is reference
  data = subset(cand)
)

summary(fit_gam)

newdat <- data.frame(expand.grid(day_of_season = seq(min(vaa_noyr$day_of_season), max(vaa_noyr$day_of_season), length.out = 60)
                                 ,fconfig=unique(vaa_noyr$config)))
pred_p <- predict(fit_gam, newdata = newdat, type = "response")

require(tidyr)
plot_df <- cbind(newdat, as.data.frame(pred_p)) %>%
  pivot_longer(
    cols = c(-day_of_season,-fconfig),
    names_to = "size_class",
    values_to = "prob"
  )
require(ggplot2)
ggplot(subset(plot_df,size_class=='pred_p'), aes(x = day_of_season, y = prob, color = fconfig)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Day of Season (DOS)",
    y = "Probability of Shorts",
    color = "Configuration of Vents",
    title = "Fitted size-class probabilities over day of season"
  ) +
  theme_minimal(base_size = 13)


cand = subset(cand,!is.na(day_of_season))
cand$total = cand$sublegal+cand$legal
require(mgcv)
fit_gam <- gam(
  total ~ (day_of_season*fconfig)+fconfig,
  family = poisson(link='log'),   
  data = subset(cand,total>0)
)

summary(fit_gam)

newdat <- data.frame(expand.grid(day_of_season = seq(min(vaa_noyr$day_of_season), max(vaa_noyr$day_of_season), length.out = 60),fconfig=unique(cand$fconfig)))
pred_p <- predict(fit_gam, newdata = newdat, type = "response")

require(tidyr)
plot_df <- cbind(newdat, as.data.frame(pred_p))

ggplot(subset(plot_df), aes(x = day_of_season, y = pred_p, color = fconfig)) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "Day of Season (DOS)",
    y = "Total Lobster Catch",
    color = "Configuration of Vents"
  ) +
  theme_minimal(base_size = 13)

####
###subslegal

cand = subset(cand,!is.na(day_of_season))
cand$total = cand$sublegal+cand$legal
require(mgcv)
fit_gam <- gam(
  sublegal ~ (day_of_season*fconfig)+fconfig,
  family = poisson(link='log'), 
  data = subset(cand,total>0)
)

summary(fit_gam)

newdat <- data.frame(expand.grid(day_of_season = seq(min(vaa_noyr$day_of_season), max(vaa_noyr$day_of_season), length.out = 60),fconfig=unique(cand$fconfig)))
pred_p <- predict(fit_gam, newdata = newdat, type = "response")

require(tidyr)
plot_df <- cbind(newdat, as.data.frame(pred_p))

ggplot(subset(plot_df), aes(x = day_of_season, y = pred_p, color = fconfig)) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "Day of Season (DOS)",
    y = "Sublegal Lobster Catch",
    color = "Configuration of Vents"
  ) +
  theme_minimal(base_size = 13)

cand = subset(cand,!is.na(day_of_season))
cand$total = cand$sublegal+cand$legal
fit_gam <- gam(
  sublegal ~ (day_of_season*fconfig)+fconfig+legal,
  family = poisson(link='log'), 
  data = subset(cand,total>0)
)

summary(fit_gam)

newdat <- data.frame(expand.grid(day_of_season = seq(min(vaa_noyr$day_of_season), max(vaa_noyr$day_of_season), length.out = 60),fconfig=unique(cand$fconfig),legal=mean(cand$legal)))
pred_p <- predict(fit_gam, newdata = newdat, type = "response")
#predp is for the first column of the cbind.... ie short

require(tidyr)
plot_df <- cbind(newdat, as.data.frame(pred_p))

ggplot(subset(plot_df), aes(x = day_of_season, y = pred_p, color = fconfig)) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "Day of Season (DOS)",
    y = "Sublegal Lobster Catch",
    color = "Configuration of Vents"
  ) +
  theme_minimal(base_size = 13)

