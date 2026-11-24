#' @export
eggProduction <- function(cl, lfa,year ) {
  
  lfa_map <- c(
    "27N" = "LFA27-30",
    "27S" = "LFA27-30",
    "27"  = "LFA27-30",
    "29"  = "LFA29",
    "28"  = "LFA28,30",
    "30"  = "LFA28,30",
    "31A" = "LFA31A",
    "31B" = "LFA32",
    "311" = "LFA31A",
    "312" = "LFA32",
    "32"  = "LFA32",
    "33E" = "LFA33",
    "33W" = "LFA33",
    "33"  = "LFA33",
    "34"  = "LFA34",
    "35"  = "LFA35",
    "36"  = "LFA36",
    "37"  = "LFA38",
    "38"  = "LFA38",
    "40"  = "LFA41",
    "41"  = "LFA41"
    )
  
  # maturity parameters
  mat <- read.csv(file.path(git.repo,'bio.lobster.data','HCR','mat_pars.csv'))
  
  # fecundity parameters
  fec <- data.frame(
    LFA = c("LFA27-30","LFA33","all areas"),
    a = c(0.000057,0.003135,0.00256),
    b = c(4.28,3.354,3.409)
  )
  
  lfa = ifelse( startsWith(lfa, "L"),sub("^L", "", lfa),lfa  )
  
  # "34" "LFA34" "27" "27"
  # map survey LFAs to maturity LFAs
  lfa2 <- ifelse(  startsWith(lfa, "LFA"),lfa,unname(lfa_map[as.character(lfa)]))
  
  ## maturity and years
  ii <- mapply(  function(l, y) {
      x <- which(mat$LFA == l & y >= mat$minYear & y <= mat$maxYear )
      if(length(x) != 1)
        stop(paste("No unique maturity parameters for", l, "year", y))
      x},
    lfa2, year)
  
  pMat <- 1 / (1 + exp(mat$a[ii] + mat$b[ii] * cl))
  
  ## fecundity
  jj <- match(lfa2, fec$LFA)
  
  # use all-areas relationship where no specific one exists
  jj[is.na(jj)] <- match("all areas", fec$LFA)
  
  Eggs <- fec$a[jj] * cl^fec$b[jj]
  
  ## spawning frequency
  Pspawn <- ifelse(cl < 120, 0.5, 2/3)
  
  pMat * Eggs * Pspawn
}
