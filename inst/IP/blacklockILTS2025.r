#blacklock Survey data

lobster.db('survey')
g = subset(surveyMeasurements, LFA %in% c('L35','L36','L37') & SPECCD_ID==2550)
g$YEAR= lubridate::year(g$BOARD_DATE)
g = subset(g,YEAR>2018)


ggplot(g,aes(x=FISH_LENGTH))+geom_histogram() +facet_wrap(~YEAR)


gs = write.csv(subset(g,select=c(BOARD_DATE,FISHSET_ID,LFA,SPECCD_ID,FISH_LENGTH,SEX)),'ILTS_Lobster_LengthFreqs.csv')
