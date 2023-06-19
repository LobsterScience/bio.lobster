#' @export

lengthOfSeason = function(year=2009,LFA=36) {
		if(LFA %in% c(36,38)) {
			first_day_of_november <- make_date(year, 11, 1)
			weekday <- wday(first_day_of_november, label = TRUE,abbr=F)
			 doW = weekdays(as.Date(first_day_of_november,tz="GMT")+0:6)
			days_to_add <- match("Tuesday", doW) + 7 - 1
			d1 <- first_day_of_november + days(days_to_add)
			if(LFA==36){
				d2 = make_date(year+1, 1, 14)
				d3 = make_date(year+1, 3, 31)
				d4 = make_date(year+1, 6, 29)
			len = (d4-d3) + (d2-d1)
			}
			if(LFA==38){
						d4 = make_date(year+1, 6, 29)
					len = (d4-second_tuesday)
			}
			}
		if(LFA ==35){
			d1 <- make_date(year, 10, 14)
			d2 <- make_date(year, 12, 31)
			d3 <- make_date(year+1, 3, 1) - days(1)
			d4 <- make_date(year+1,7,31)
			len = (d4-d3) + (d2-d1)
			}
		if(LFA %in% c(33,34)){
			d1 <- make_date(year, 12, 1)
			weekday <- wday(d1, label = TRUE,abbr=F)
			d1 = ceiling_date(d1,'week',week_start=1)-days(7)
			d4 = d2 <- make_date(year+1, 5, 31)
			len =  (d2-d1)
			}
			


		return(list(ndays = len,start=d1,end=d4))
		}


