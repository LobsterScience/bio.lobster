#' @export

importShapefile <- function (fn, readDBF=TRUE, projection=NULL, zone=NULL, 
                             minverts=3, placeholes=FALSE, show.progress=FALSE)
{
  #extracted from pbsmapping need to update with SF or sp asap
  
  fn <- normalizePath(fn, mustWork=FALSE)
  fn <- .getBasename(fn, "shp")
  
  ## test for the required '.shx' file
  shxFile <- paste(fn, ".shx", sep="")
  if (!file.exists(shxFile))
    stop(paste(
      "Cannot find the index file (\"", shxFile, "\") required to import\n",
      "the shapefile.\n", sep=""))
  
  ## read shapefile
  eval(parse(text="shapeList <- .Call(\"Rshapeget\",as.character(fn),as.logical(FALSE),PACKAGE=\"maptools\")"))
  if (length(shapeList) < 1)
    stop("The shapefile is empty or an error occurred while importing.\n")
  shpType=unique(sapply(shapeList,function(x){x$shp.type}))
  if (length(shpType) != 1)
    stop ("Supports only a single shape type per shapefile.\n")
  nVerts=sapply(shapeList,function(x){x$nVerts})
  v0=is.element(nVerts,0) # any shapefiles with 0 vertices?
  if (any(v0==TRUE)) {
    nVerts=nVerts[!v0]; shapeList=shapeList[!v0] }
  shpID=sapply(shapeList,function(x){x$shpID})
  nParts=sapply(shapeList,function(x){x$nParts})
  pStarts=sapply(shapeList,function(x){x$Pstart},simplify=FALSE)
  if (length(pStarts)!=length(nParts) && all((nParts==sapply(pStarts,length))!=TRUE))
    stop ("Mismatch in 'nParts' and 'pStarts'.\n")
  pStarts=unlist(pStarts)
  v1=unlist(sapply(shapeList,function(x){x$verts[,1]},simplify=FALSE))
  v2=unlist(sapply(shapeList,function(x){x$verts[,2]},simplify=FALSE))
  verts=cbind(v1,v2)
  
  ## Keep track of parents and children
  PC=pStarts
  zP=is.element(PC,0); PC[zP]=1; PC[!zP]=0
  
  ## reformat results
  #if (shpType == 3 || shpType == 5) {	## PolySet
  if (shpType %in% c(3,13,23, 5,15,25)) {	## PolyLine, PolyLineZ, PolyLineM, Polygon, PolygonZ, PolygonM
    ## create preliminary PID/SID columns
    PID <- rep(1:(length(unique(shpID))), times=nParts)
    SID <- unlist(lapply(split(nParts, 1:(length(nParts))), "seq"))
    
    ## to determine the number of vertices in each part, we divide the problem
    ## into two cases:
    ## 1) last component/hole of each polygon: the total vertices in the polygon
    ##		less the starting POS of that last component/hole
    ## 2) otherwise: use a "diff" on the starting POS's of each part
    lastComp <- rev(!duplicated(rev(PID)))
    nv <- vector()
    nv[lastComp] <- rep(nVerts, times=nParts)[lastComp] - pStarts[lastComp]
    nv[!lastComp] <- diff(pStarts)[diff(pStarts) > 0]
    
    ## create PID/SID columns
    PID <- rep(PID, times=nv)
    SID <- rep(SID, times=nv)
    ## create POS column; we'll fix the ordering for holes later
    POS <- unlist(lapply(split(nv, 1:(length(nv))), "seq"))
    ## build the data frame
    df <- data.frame(PID=PID, SID=SID, POS=POS, X=verts[, 1], Y=verts[, 2])
    
    #if (shpType == 5) {
    if (shpType %in% c(5,15,25)) {
      ## PolySet: polygons: reorder vertices for holes
      or <- .calcOrientation (df)
      ## where "orientation" == -1, we need to reverse the POS
      or$solid <- is.element(or$orientation,1); or$hole <- !or$solid
      if (any(or$hole)) {
        or$nv <- nv
        toFix <- rep(or$hole, times=or$nv)
        o <- or$nv[or$hole]
        newPOS <- unlist(lapply(lapply(split(o, 1:length(o)), "seq"), "rev"))
        df[toFix, "POS"] <- newPOS	}
      
      if (placeholes) {
        ## Fix to the problem where ArcPew does not put solid shapes before holes
        class(df) <- c("PolySet", setdiff(class(df),"PolySet"))
        df=placeHoles(df, minVerts=minverts, orient=TRUE, show.progress=show.progress)
      }
    }
    class(df) <- c("PolySet", class(df))
    #} else if (shpType == 1) {	## EventData
  } else if (shpType %in% c(1,11,21)) {	## Point, PointZ, PointM
    EID <- 1:(length(unique(shpID)))
    df <- data.frame(EID=EID, X=verts[, 1], Y=verts[, 2])
    class(df) <- c("EventData", class(df))
  } else {
    stop ("Shape type not supported.\n");
  }
  
  ## "cbind" the DBF for EventData or attach as an attribute for PolySets:
  ## According to the "ESRI Shapefile Technical Description", any set of fields
  ## may be present in the DBF file.
  ## The (relevant) requirements are:
  ##	 (1) one record per shape feature (i.e., per PID or EID), and
  ##	 (2) same order as in shape (*.shp) file.
  dbfFile <- paste(fn, ".dbf", sep="")
  if (readDBF && !file.exists(dbfFile)) {
    warning(paste(
      "The argument 'readDBF' is true but the attribute database\n",
      "(\"", dbfFile, "\") does not exist.\n", sep=""))
  } else if (readDBF) {
    dbf <- foreign::read.dbf (dbfFile)
    if (shpType == 1) {	## EventData
      if (nrow(df) != nrow(dbf)) {
        warning(paste(
          "The shapefile and its associated DBF do not contain the",
          "same number of records. DBF ignored.\n", sep="\n"))
        return (df)
      }
      df.class=class(df)
      df <- cbind(df, dbf)
      class(df) <- df.class
    } else if (shpType == 3 || shpType == 5) {
      ## add index to result
      dbf <- cbind(1:nrow(dbf), dbf)
      names(dbf)[1] <- "PID"
      class(dbf) <- c("PolyData", class(dbf))
      attr(df, "PolyData") <- dbf
    }
    ## At this point, shpTypes != 1, 3, 5 caused the "stop" above; we do not
    ## need an "else" to check here
  }
  attr(df,"parent.child")=PC
  attr(df,"shpType")=shpType
  prjFile <- paste(fn, ".prj", sep="")
  if (file.exists(prjFile)) {
    prj=scan(prjFile, what="character", quiet=TRUE, skipNul=TRUE)
    #browser();return()
    prj=prj[!is.element(prj,"")][1]
    if (length(prj)==0 || is.na(prj) || is.null(prj)) prj="Unknown" }
  else prj="Unknown"
  attr(df,"prj")=prj
  xmlFile <- paste(fn, ".shp.xml", sep="")
  if (file.exists(xmlFile)) {
    xml=readLines(xmlFile); attr(df,"xml")=xml }
  
  if (regexpr("GEO",prj)>0 | regexpr("Degree",prj)>0) proj="LL"
  else if (regexpr("PROJ",prj)>0 && regexpr("UTM",prj)>0) proj="UTM"
  else proj=1
  attr(df,"projection")=proj
  
  if (proj=="UTM" && any(df$X>500))
  {df$X=df$X/1000; df$Y=df$Y/1000}
  if (!is.null(zone))
    attr(df, "zone") <- zone
  if (!is.null(projection))
    attr(df,"projection")=projection
  #browser();return()
  return (df) }

.getBasename <- function (fn, ext)
  # If appropriate, remove the extension from 'fn' to obtain the shapefile
  # name without the extension.  When testing for file existance, use the
  # extension 'ext'.
{
  # if appending .shp does not give a valid file...
  if (!file.exists (paste(fn, ".", ext, sep=""))) {
    # attempt to remove extension and try again
    fn <- sub ("\\..{3}$", "", fn);
    
    if (!file.exists (paste(fn, ".", ext, sep=""))) {
      stop (paste ("Cannot find the file \"", fn, ".", ext, "\".", sep=""));
    }
  }
  
  return (fn);
}

.calcOrientation <- function(polys)
  # Assumes 'polys' contains a valid PolySet.
  #
  # Returns:
  #   data frame (invisible) with 'orientation' column (-1 when
  #     counter-clockwise; 0 when N/A; +1 when clockwise)
  #   OR: NULL (invisible) (if no rows in output)
{
  inRows <- nrow(polys);
  # Memory requirement for output are lower than nrow(polys); in fact, they
  # are length(unique(paste(polys$PID, polys$SID))).
  # The extra time to compute the real memory requirements makes doing so
  # not worth it.
  outCapacity <- nrow(polys);
  
  # create the data structure that the C functions expect
  # Using $<col> notation seems faster than [, "col"] notation.
  if (!is.element("SID", names(polys))) {
    inID <- c(polys$PID, integer(length = inRows), polys$POS);
  } else {
    inID <- c(polys$PID, polys$SID, polys$POS);
  }
  inXY <- c(polys$X, polys$Y);
  
  # call the C function
  results <- .C("calcOrientation",
                inID = as.integer(inID),
                inXY = as.double(inXY),
                inVerts = as.integer(inRows),
                outID = integer(2 * outCapacity),
                outOrientation = double(outCapacity),
                outRows = as.integer(outCapacity),
                outStatus = integer(1),
                PACKAGE = "PBSmapping");
  # note: outRows is set to how much space is allocated -- the C function
  #       should consider this
  
  if (results$outStatus == 1) {
    stop(
      "Insufficient physical memory for processing.\n");
  }
  if (results$outStatus == 2) {
    stop(paste(
      "Insufficient memory allocated for output.  Please upgrade to the latest",
      "version of the software, and if that does not fix this problem, please",
      "file a bug report.\n",
      sep = "\n"));
  }
  
  # determine the number of rows in the result
  outRows <- as.vector(results$outRows);
  
  # extract the data from the C function results
  if (outRows > 0) {
    d <- data.frame(PID = results$outID[1:outRows],
                    SID = results$outID[(outCapacity+1):(outCapacity+outRows)],
                    orientation = results$outOrientation[1:outRows]);
    
    if (!is.element("SID", names(polys)))
      d$SID <- NULL;
    
    invisible(d);
  } else {
    invisible(NULL);
  }
}

