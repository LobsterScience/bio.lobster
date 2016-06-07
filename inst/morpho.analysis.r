
  lob.data.dir = file.path( project.datadirectory("lobster"), "data")
  ld = read.table(file.path(lob.data.dir, "morphometrics.lobster.ess.2005.csv"), sep=";", as.is=T, header=T)
  colnames(ld) = tolower(colnames(ld))
  
  ld$id = seq(along=ld[,1])
 
  male=1
  female=2

  # initial data corrections

  mv1 = c("carlength","carwidth","adbwidth","clawlength","clawwidth","clawthickness")
  mv2 = c("carlength", "carwidth","adbwidth","clawlength","clawwidth","clawthickness")
  
  nvars= length(mv1)


  l = ld
  threshold = 5 # number of standard deviations to use as an upper bound
  plotdata = F

  for (sex in c(male, female)) {
  for ( iv1 in 1:nvars ) {
    v1 = mv1[iv1]
    
    for (iv2 in iv1:nvars) {
    v2 = mv2[iv2]

    if (v1==v2) next
    
    i = which( is.finite( l[,v1] ) &  is.finite( l[,v2] ) & l$sex==sex)
     
    lo = l[i,]

    if (plotdata) {
      x11()
      plot( l[,v1],  l[,v2])
      title( paste(v1,v2) ) 
    }

    llm = lm( log(lo[,v2]) ~ log(lo[,v1]) )
    strange = as.vector( which( abs( rstandard( llm ) ) > threshold ) )
    if (length( strange) > 0 ) {
      todrop = lo$id [ strange ]
      l[ todrop, v2 ] = NA
    }

  }}}

  # for (i in 1:20) dev.off()

  save(l, file=file.path(lob.data.dir, "morphometrics.lobster.ess.2005.cleaned.rdata"), compress=T)

  
  # Discriminant functions analysis for females
  require(MASS)
  
  f = l[ which(l$sex==female), ]
  mature = which(f$pleopod >=2)
  immature = which(f$pleopod <2)
  f$mat = NA
  f$mat[mature] = "mat"
  f$mat[immature] = "imm"
  f$mat = as.factor(f$mat)

  # log transform data
  morpho.vars = c("carlength","carwidth","adbwidth","clawlength","clawwidth","clawthickness")
  for (v in morpho.vars) f[,v] = log(f[,v])
  

  dfa.result = lda(formula = mat ~ carlength + carwidth + adbwidth + clawlength + clawwidth + clawthickness, data=f, na.action=na.omit 	)

  dfa.result = lda(formula = mat ~ carlength + carwidth + adbwidth +   clawthickness, data=f, na.action=na.omit 	)



