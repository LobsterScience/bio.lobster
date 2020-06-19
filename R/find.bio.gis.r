#//MMM - Sept 2014
#// Modified code to allow specification of filename with
#// extension.  Cases exist where 2 files have the same name, but
#// different extensions (e.g.StAnnsMPA.csv vs StAnnsMPA.dat) - the
#// original code could never get an exact match since it ignored the
#// extension.
#
#//Also changed so that in the case of a single closest match, the data
#//for the lone match is simply returned

find.bio.gis = function(
    polyname,
    loc = project.datadirectory("bio.lobster","data","polygons", "data"),
    ignorelist=c("archive", "retired"),
    acceptable.extensions = c( "dat", "csv", "xy", "shp","ll" ),
    returndata=FALSE ,
    return.one.match=T) {

    fs = .Platform$file.sep

    out = NULL
    rem  = NULL

    # find all filenames and match
    flist = list.files( path=loc, pattern="*", recursive=TRUE, ignore.case=TRUE, include.dirs=TRUE, full.names=TRUE )

#MMM - Sept 2014 - this isn't ideal.  Cases exist where 2 files have the same name, but different extensions
# and this prevents a full path from correctly identifying the file (e.g.StAnnsMPA.csv vs StAnnsMPA.dat)
#fl = basename(flist)
fl<-flist
    # keep only acceptable file types
    keep = NULL
    acceptable.extensions = paste( "[.]*.", acceptable.extensions, "$", sep="")
    for (ik in acceptable.extensions) {
      keep0 = grep ( ik, fl, ignore.case=T )
      if (length( keep0)>0 ) keep = c( keep, keep0 )
    }
    if ( length(keep)>0 ) {
      keep = unique(keep)
      flist = flist[keep]
    }

    # remove data flagged to be "ignored" or archived etc..
    for (ig in ignorelist) {
      rem0 = grep ( ig, flist,  ignore.case =T ) ## NOTE:: this matches directory names too
      if (length( rem0)>0 ) rem = c( rem, rem0 )
    }
    if ( length(rem)>0 ) {
      rem = unique(rem)
      flist = flist[-rem]
    }

#see note above about finding exact matches with extension
    # reset to current list
#fl = basename(flist)
fl<-flist
    for (pn in polyname ) {

      i = grep( paste("\\", fs, pn, sep=""), flist , ignore.case=TRUE )
      if (length(i) == 1) {      ## EXACT match
        return(fl[i])
      } else {
        if(return.one.match) {# added by amc hope it does not make too much of a mess..used if one file name
              ff = sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(flist)) #recursive search to last dot and the remove everything after last dot
              pn1 = paste("^",pn,"$",sep="")
              fil = grep(pn1,ff)
              return(flist[fil])
          }
        j = !is.na(pmatch( pn, fl ))
        k = agrep( pn, fl, ignore.case=TRUE )
        l = unique( c(j, k) )
        if ( length(l) > 2 ){             #return the list of candidates
          print( paste( "No exact match found for", pn, ", here are the closest matches:" ) )
          print( flist[l]  )
        } else if ( length(l) > 1 ) {     #treat a lone result like an exact match
          return(fl[l])
          #out = c( out, l)
        } else {
          print( "No similarly named files found")
        }
      }
    }
#MMM Sep 2014
#don't think we need to load the data - just want to find the file
#also, this results in an extra null in any non-exact match search
#    res = NULL
#    if (length(out) > 0 ) {
#      if (returndata) {
#        for ( o in out ) {
#          res[[ fl[o] ]] = read.table( flist[o], header=F)
#        }
#      } else {
#        res = flist[out]
#      }
#    }
#    return( res )
  }
