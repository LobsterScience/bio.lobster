
  array_map = function( method, coords, dims=NULL, origin=NULL, res=NULL, gridparams=NULL ) {
    #// array indexing from 1d to nd and nd to 1d 
    # .. for higher dimensions, just follow the patterns
    # coords are input coords 
    # dims are dimension sizes 
    # origin min coord values
    # res resolution (dx, dy)

    if ( !is.null(gridparams) ) {
      dims = gridparams$dims
      origin = gridparams$origin
      res = gridparams$res
    }

    if (method=="xy->2") {
      return( round( cbind( (coords[,1]-origin[1])/res[1], (coords[,2]-origin[2])/res[2] ) ) + 1L )
    }

    if (method=="xy->1") {
      ij = round( cbind( (coords[,1]-origin[1])/res[1], (coords[,2]-origin[2])/res[2] ) ) + 1L  # same as "xy->2"
      return( ij[,1] + (ij[,2]-1L)*dims[1] ) # same as 2->1
    }

    if (method=="2->1") { 
      return( coords[,1] + (coords[,2]-1L)*dims[1] ) 
    }
    
    if (method=="3->1") { 
      return( coords[,1] + (coords[,2]-1L)*dims[1] + (coords[,3]-1L)*dims[1]*dims[2] ) 
    }
    
    if (method=="4->1") { 
      return( coords[,1] + (coords[,2]-1L)*dims[1] + (coords[,3]-1L)*dims[1]*dims[2] + (coords[,4]-1L)*dims[1]*dims[2]*dims[3] ) 
    }


    if (method=="3->2") { 
      ii = array_map( "3->1" , coords, dims ) 
      jj = array_map( "1->2" , ii, dims )
      return( jj ) 
    }
    
    if ( method=="1->2" ) {
      j = coords-1 # -1 converts to C-indexing
      x = j %%  dims[1]
      j = j %/% dims[1]
      y = j
      return( cbind(x,y)+1L )  # +1 returns to R-indexing
    }

    if ( method=="1->3" ) {
      j = coords-1L # -1 converts to C-indexing
      x = j %%  dims[1]
      j = j %/% dims[1]
      y = j %%  dims[2]
      j = j %/% dims[2]
      z = j
      return( cbind(x,y,z) + 1L ) # +1 returns to R-indexing
    }

    if ( method=="1->4" ) {
      j = coords-1L # -1 converts to C-indexing
      x = j %%  dims[1]
      j = j %/% dims[1]
      y = j %%  dims[2]
      j = j %/% dims[2]
      z = j %%  dims[3]
      j = j %/% dims[3]
      a = j
      return( cbind(x,y,z,a) + 1L ) # +1 returns to R-indexing
    }
  }

