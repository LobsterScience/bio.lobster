#' @export

	meansize.direct = function( ) {
	
		k = groundfish.db( DS="det.base" )
 	  m = which( is.finite( k$len))
		l = as.data.frame( unlist( ( tapply( log(k$len[m]), k$spec[m], mean ) ) ))
		names(l) = "meanlength"
		l$spec= as.numeric( as.character( rownames(l) ) )
		l$meanlength = exp( l$meanlength )
	
		n = which( is.finite( k$mass))
		o = as.data.frame( unlist( ( tapply( log(k$mass[n]), k$spec[n], mean ) ) ))
		names(o) = "meanweight"
		o$spec= as.numeric( as.character( rownames(o) ) )
		o$meanweight = exp( o$meanweight )
	
		r = merge( l, o, by="spec", all=T, sort=T )
		return(r)

	}


