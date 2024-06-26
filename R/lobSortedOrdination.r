#' @export
  
  lobSortedOrdination = function( b, colscheme='redgreen', addscores=F, sortdecreasing=F, title=NULL, outfileroot=NULL ,outfilenames=NULL,groupings=NULL,addAttributes=F,attributetype='ecosystemIndicators',ht=c(12,12),wd=c(15,15),labs=T,margins=c(1, 3.2, 0.3, 1)) {

    yvals=rownames(b)
    vars =colnames(b)

    years= as.numeric(yvals)
    yrange = range(years)

    if (is.null(title)) title = "Years"
    if (is.null(outfileroot)) outfileroot="sorted.anomalies"
if(is.null(outfilenames)) {
    of1 = file.path(outfileroot, "PC1.png")
    of2 = file.path(outfileroot, "PC2.png")
    of3 = file.path(outfileroot, "anomalies.png")
    } else {
    of1 = file.path(outfileroot,paste(outfilenames, "PC1.png", sep="."))
    of2 = file.path(outfileroot,paste(outfilenames, "PC2.png", sep="."))
    of3 = file.path(outfileroot,paste(outfilenames, "anomalies.png", sep="."))
        
    }
    
    corel = cor( b, use="pairwise.complete.obs" ) # set up a correlation matrix ignoring NAs
    corel[ is.na(corel) ] = 0
    
    s = svd(corel)  # eigenanalysis via singular value decomposition

    ndat = dim(b)[1]
    scores = matrix(0, nrow = ndat, ncol = 2)
    for (j in 1:2) {
        for (i in 1:ndat) {
            scores[i, j] = sum(b[i, ] * t(s$v[, j]), na.rm = T)
          }
      }


    evec = s$v
    eval = s$d
    
    x = cbind( scores[,1] / sqrt(eval[1] ), scores[,2] / sqrt( eval[2]) )
    y = cbind( evec[,1] * sqrt(eval[1] ) , evec[,2] * sqrt( eval[2]) )

        outscores = data.frame(x)
        outscores$yr = as.numeric(yvals)

    png(file=of1,units='in',width=wd[1],height=ht[1],pointsize=18, res=300,type='cairo')
#    par(cex=2, lty=2)
    plot( yvals, x[,1], xlab="Years", ylab=paste("PCA1 (", round(eval[1]/sum(eval)*100, 1), "%)", sep="") )
    lines( lowess(yvals, x[,1], f=1/5 ) )
    dev.off()
    print(of2)
    png(file=of2,units='in',width=wd[1],height=ht[1],pointsize=18, res=300,type='cairo')
    plot( yvals, x[,2], xlab="Years", ylab=paste("PCA2 (", round(eval[2]/sum(eval)*100, 1), "%)", sep="" ) )
    lines( lowess(yvals, x[,2], f=1/5) )
    dev.off()
    
    # form residual variations figure based upon a sorting of ordination scores
    varloadings = NULL
    varloadings = as.data.frame( cbind( y, vars ) )

    q = as.data.frame( t( as.matrix( b ) ) )
    q$vars = rownames( q )
    q = merge( x=q, y=varloadings, by.x="vars", by.y="vars", all=T )
    ordered = sort( as.numeric( as.character( q$V1 ) ), index.return=T, decreasing=F )
    qq = q[ ordered$ix, ]

    sc1 = round(as.numeric(as.character(qq$V1)),2)
    sc2 =   round(as.numeric(as.character(qq$V2)),2)
    if (addscores) {
      varnames = paste(qq$vars, " {", sc1, ", ",sc2, "}", sep="")
    } else { 
      varnames = qq$vars 
    }
    qa = q     
    qq$vars = qq$V1 = qq$V2 = NULL
    qq = as.data.frame( t( qq ) )
    colnames( qq ) = varnames
    Z = as.matrix( qq )

    a = max( abs( min(Z, na.rm=T) ), abs( max( Z, na.rm=T) ) )
    br = seq( -a, a, .1)

    png(file=of3,units='in',width=wd[2],height=ht[2],pointsize=25, res=300,type='cairo')
    if(!is.function(colscheme)){ 
    if (colscheme=="rainbow")  {
      cols=rainbow(length(br)-1, start=0, end=1/3)
    } else if (colscheme=="redgreen") {
      cols=rev(red_green(length(br)))    
    } else if (colscheme=="bluered") {
      cols=rev(blue.red(length(br)))    
    } else if (colscheme=="heat") { 
      cols=rev(heat.colors(length(br)-1))
    }
    } else {
      cols=colscheme(length(br)-1)
    }

    par( mai=margins, cex=1 )
    
    image(z=Z, x=years, breaks=br, col=cols, ylab="", axes=F )
    
    for (i in seq(range(yrange)[1], range(yrange)[2], by=10)) abline( v=i-0.5, col="slategray")
    for (i in seq(range(yrange)[1], range(yrange)[2], by=1)) abline( v=i-0.5, col="slategray1")

    z = 1/(dim(Z)[2] - 1)
    for (i in seq(0, 1, by=z*10)) abline( h=i-(z/2), col="slategray")
    for (i in seq(0, 1, by=z)) abline( h=i-(z/2), col="slategray1")

    par(las=1)
    axis( 1 )

    vars = colnames(Z)
    nums = length(vars):1

    #browser()

    var.table = data.frame(Rank=rev(nums),Variable=rev(vars),PCA1=rev(sc1),PCA2=rev(sc2))

    par(cex=0.6)
    if(labs){
        axis( 2, at=seq(0,1,length=length(vars)), labels=vars)
    } else{
         axis( 2, at=seq(0,1,length=length(vars)), labels=nums)
     }
    
    if(addAttributes){
            g = indicatorAttributesMatrixPlot(x=vars,groupings='default',attribute=attributetype)
            axis( 4, at=seq(0,1,length=length(vars)), labels=g,cex.lab=4)
        }
     if(!is.null(groupings)){
        for(i in 1:length(groupings)){
             CurlyBraces(x0=max(years)+1,x1=max(years)+1,y0=groupings[[i]][1]/dim(Z)[2],y1=groupings[[i]][2]/dim(Z)[2],pos=1,direction=1,depth=2)
        }
        text(rep(max(years)+3.5,length(groupings)), unlist(lapply(groupings,mean))/dim(Z)[2], 1:length(groupings),xpd=NA,cex=2)
    }
    write.table(Z, file="anomalies.dat", quote=F, sep=";")
    box()
    dev.off()
    out = list( id=yvals, vars=vars, correlation.matrix=corel, 
               eigenvectors=evec, eigenvalues=eval, projections.id=x, projections.vars=y,pcaa=q)
    print( str( out)) 

    return(out)
  }


