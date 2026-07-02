addSYEAR<- function(aS) {
  
  ## Force base data.frame
  aS <- as.data.frame(aS)
  
  ## Ensure STARTDATE exists
  if (!"STARTDATE" %in% names(aS)) {
    stop("STARTDATE column is missing. Cannot create SDATE.")
  }
  
  ## Create SDATE as Date
  aS$SDATE <- as.Date(aS$STARTDATE)
  
  ## Load season dates
  season.dates <- backFillSeasonDates(
    lobster.db('season.dates'),
    eyr = year(Sys.time())
  )
  
  ## Ensure season dates are Date objects
  season.dates$START_DATE <- as.Date(season.dates$START_DATE)
  season.dates$END_DATE   <- as.Date(season.dates$END_DATE)
  
  ## Prepare SYEAR
  aS$SYEAR <- NA_integer_
  
  ## Loop through LFAs
  lfas <- na.omit(unique(aS$LFA))
  
  for (l in lfas) {
    
    ## Season rows for this LFA
    h <- season.dates[season.dates$LFA == l, ]
    
    ## Rows in atSea for this LFA
    idx_lfa <- which(aS$LFA == l)
    
    ## Loop through each season year for this LFA
    for (i in seq_len(nrow(h))) {
      
      sy <- h$SYEAR[i]
      s1 <- h$START_DATE[i]
      s2 <- h$END_DATE[i]
      
      ## Find rows whose SDATE falls inside this season window
      idx <- idx_lfa[aS$SDATE[idx_lfa] >= s1 & aS$SDATE[idx_lfa] <= s2]
      
      ## Assign SYEAR
      if (length(idx) > 0) {
        aS$SYEAR[idx] <- sy
      }
    }
  }
  
  ## Handle future years (e.g., 2026) with no season dates yet
  missing <- which(is.na(aS$SYEAR))
  if (length(missing) > 0) {
    aS$SYEAR[missing] <- year(aS$SDATE[missing])
  }
  
  return(aS)
}
