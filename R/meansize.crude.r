#' @export

	meansize.crude = function( Sp, Tn, Tw ) { 
		# fix missing numbers and mass estimates:
		# zeros for one while nonzeros for correpsonding records
		meanwgt = Tw / Tn
		good = which( is.finite( meanwgt) & is.finite( 1/meanwgt ) )
		mw = as.data.frame( unlist( (tapply( log( meanwgt[good]), Sp[good], mean )) ))
		names(mw) = "meanweight"
		mw$spec= as.numeric( as.character( rownames(mw) ) )
		mw$meanweight = exp( mw$meanweight )
	
		return(mw)
	}


