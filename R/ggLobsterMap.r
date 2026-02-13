#' Title: Map LFAs
#' Description: Plot lobster fishing areas with labels, grids, bathymetry, etc.
#' @export
ggLobsterMap <- function(
        area='custom',
        fill.colours='grey',
        ylim=c(40,52),
        xlim=c(-74,-47),
        LFAfill='white',
        attrData=NULL,
        attrColumn='Z',
        addGrids=FALSE,
        addNAFO=FALSE,
        nafo='4X',
        bathy=TRUE,
        fw=NULL,
        legLab="",
        addLFAlines=TRUE,
        addLFALabels=FALSE,
        addGridLabels=FALSE,
        addPoints=FALSE,
        pts=NULL,
        addNAFOLabels=FALSE,
        scaleTrans='identity',
        brks=NULL,
        return.object=FALSE,
        layerDir=file.path(bio.directory,'bio.lobster.data','mapping_data'),
        LFA_label_size=4,
        grid_label_size=3,
        #colourLFA=FALSE,
        show37label=FALSE,
        colourLFA=NULL,
        ...
){
    lfa_colours <- c(
        "skyblue",
        "gold",
        "red1",
        "green3",
        "orchid",
        "orange",
        "dodgerblue3"
    )
    
    
    # ----------------------
    # Area presets (FULL)
    # ----------------------
    presets <- list(
        all      = list(ylim=c(41.1,48),   xlim=c(-67.8,-57.8)),
        inshore  = list(ylim=c(42.1,48),   xlim=c(-67.8,-57.8)),
        `27-38`  = list(ylim=c(42.5,48),   xlim=c(-67.4,-57.8)),
        `27-33`  = list(ylim=c(42.5,48),   xlim=c(-66.6,-57.8)),
        `27-32`  = list(ylim=c(43.5,48),   xlim=c(-63.6,-57.8)),
        ENS      = list(ylim=c(44.0,45.7), xlim=c(-62.2,-59.8)),
        `34-38`  = list(ylim=c(42.5,46),   xlim=c(-67.8,-63.5)),
        `35-36`  = list(ylim=c(44.5,46),   xlim=c(-67.2,-63.2)),
        west     = list(ylim=c(41.1,46),   xlim=c(-67.8,-62.2)),
        `27`     = list(ylim=c(44.9,47.9), xlim=c(-61,-57.8)),
        `27.Crop`= list(ylim=c(45.4,47.6), xlim=c(-61.1,-58.8)),
        `28`     = list(ylim=c(45.3,46.4), xlim=c(-61.6,-60.2)),
        `29`     = list(ylim=c(45.3,46),   xlim=c(-61.6,-60.3)),
        `30`     = list(ylim=c(44.6,45.9), xlim=c(-60.8,-59.6)),
        `31A`    = list(ylim=c(44.4,45.7), xlim=c(-61.8,-60)),
        `31B`    = list(ylim=c(44.1,45.3), xlim=c(-62.2,-60.5)),
        `32`     = list(ylim=c(43.8,45),   xlim=c(-63.5,-61.5)),
        `33`     = list(ylim=c(42.5,44.8), xlim=c(-65.8,-62.2)),
        `34`     = list(ylim=c(42.5,45),   xlim=c(-67.8,-65)),
        `35`     = list(ylim=c(44.5,46),   xlim=c(-66,-63.2)),
        `36`     = list(ylim=c(44.5,45.7), xlim=c(-67.2,-65)),
        `37`     = list(ylim=c(44.5,45.2), xlim=c(-67.2,-66)),
        `38`     = list(ylim=c(43.8,45),   xlim=c(-67.5,-66.2)),
        `40`     = list(ylim=c(42.25,43),  xlim=c(-66.5,-65.25)),
        `41`     = list(ylim=c(41.1,44),   xlim=c(-68,-63.5)),
        `41_full`= list(ylim=c(40,46.5),   xlim=c(-68,-55)),
        SWN      = list(ylim=c(42.5,45),   xlim=c(-67.8,-62.2)),
        BoF      = list(ylim=c(43.75,46),  xlim=c(-67.8,-63.2)),
        `33-35`  = list(ylim=c(42.5,46),   xlim=c(-67.8,-63.2)),
        `33-34`  = list(ylim=c(42.5,45),   xlim=c(-67.5,-62.2))
    )
    
    if(area %in% names(presets)){
        ylim <- presets[[area]]$ylim
        xlim <- presets[[area]]$xlim
    }
    
    # ----------------------
    # Libraries & sf setup
    # ----------------------
    library(ggplot2)
    library(sf)
    library(dplyr)
    library(scales)
    library(shadowtext)
    
    sf_use_s2(FALSE)
    theme_set(theme_bw())
    
    # ----------------------
    # Load layers
    # ----------------------
    ns_coast <- readRDS(file.path(layerDir,"CoastSF.rds"))
    grids    <- st_make_valid(readRDS(file.path(layerDir,"GridPolysLand_2023LFA37Split.rds")))
    lfa      <- readRDS(file.path(layerDir,"LFAPolys37Split.rds"))
    bath     <- readRDS(file.path(layerDir,"bathy10-300SF.rds"))
    nafo_sf  <- readRDS(file.path(layerDir,"NAFO_sf.rds"))
    cents    <- readRDS(file.path(layerDir,"LFALabelsSF.rds"))
    
    st_crs(ns_coast) <- st_crs(grids) <- st_crs(lfa) <-
        st_crs(bath) <- st_crs(cents) <- 4326
    
    gridCent <- st_centroid(grids)
    
    
    
    if(addNAFOLabels){
        nafo_sel <- subset(nafo_sf, NAFO_1 %in% nafo)
        nafo_u   <- st_union(nafo_sel)
        labs     <- st_centroid(nafo_u)
        labs$lab <- nafo
    }
    
    # ----------------------
    # Crop layers early
    # ----------------------
    crop_box <- st_bbox(c(xmin=xlim[1], ymin=ylim[1],
                          xmax=xlim[2], ymax=ylim[2]), crs=4326)
    
    ns_coast <- suppressWarnings(st_crop(ns_coast, crop_box))
    grids    <- suppressWarnings(st_crop(grids, crop_box))
    lfa      <- suppressWarnings(st_crop(lfa, crop_box))
    bath     <- suppressWarnings(st_crop(bath, crop_box))
    cents    <- suppressWarnings(st_crop(cents, crop_box))
    
    # ----------------------
    # Base plot (empty)
    # ----------------------
    p <- ggplot()
    
    # ----------------------
    # Bathymetry
    # ----------------------
    if(bathy){
        p <- p + geom_sf(
            data=subset(bath, Z %in% c(60,100)),
            colour=alpha("#2C77BF",0.3)
        )
    }
    
    # ----------------------
    # Grids
    # ----------------------
    if(addGrids){
        p <- p + geom_sf(data=grids, fill=NA)
    }
    
    # ----------------------
    # LFA outlines (offshore first)
    # ----------------------
    if(addLFAlines){
        p <- p + geom_sf(
            data=lfa,
            fill=NA,
            colour="black",
            linewidth=0.6
        )
    }
    
    # ----------------------
    # Highlight LFAs (automatic colours)
    # ----------------------
    if(!is.null(colourLFA)){
        
        user_to_pid <- c(
            "27"="27","28"="28","29"="29","30"="30",
            "31A"="311","31a"="311",
            "31B"="312","31b"="312",
            "32"="32","33"="33","34"="34",
            "35"="35","36"="36","37"="37","38"="38"
        )
        
        # Normalize colourLFA to a list of LFA groups
        if(is.character(colourLFA)){
            groups <- as.list(colourLFA)
        } else if(is.list(colourLFA)){
            groups <- colourLFA
        } else {
            stop("colourLFA must be a character vector or a list")
        }
        
        if(length(groups) > length(lfa_colours)){
            stop("Not enough default colours for number of LFA groups")
        }
        
        for(i in seq_along(groups)){
            
            lfas <- groups[[i]]
            pids <- user_to_pid[as.character(lfas)]
            
            hl <- subset(lfa, PID %in% pids)
            
            p <- p + geom_sf(
                data = hl,
                fill = scales::alpha(lfa_colours[i], 0.5),
                colour = alpha("black", 0.8),
                linewidth = 0.4
            )
        }
    }
    
    
    # ----------------------
    # Attribute data
    # ----------------------
    if(!is.null(attrData)){
        g <- attrData
        if(!"Z" %in% names(g)) g$Z <- g[[attrColumn]]
        if(is.null(brks)) brks <- range(g$Z, na.rm=TRUE)
        
        if(any(grepl("GRID",toupper(names(g))))){
            names(g)[grep("GRID",toupper(names(g)))] <- "GRID_NO"
            dat <- st_as_sf(merge(g, grids, by=c("LFA","GRID_NO")))
        } else {
            dat <- st_as_sf(merge(g, lfa, by="LFA"))
        }
        st_crs(dat) <- 4326
        
        p <- p +
            geom_sf(data=dat, aes(fill=Z)) +
            scale_fill_distiller(
                trans=scaleTrans,
                limits=brks,
                palette="Spectral",
                name=legLab
            )
        if(!is.null(fw)) p <- p + facet_wrap(fw)
    }
    
    
    
    # ----------------------
    # COASTLINE 
    # ----------------------
    p <- p + geom_sf(
        data=ns_coast,
        fill=fill.colours,
        colour="black",
        linewidth=0.5
    )
    
    
    # ----------------------
    # LFA labels
    # ----------------------
    if(addLFALabels){
        if(!show37label) cents <- subset(cents, label!="37")
        xy <- cbind(st_coordinates(cents), st_drop_geometry(cents))
        p <- p + shadowtext::geom_shadowtext(
            data=xy,
            aes(X,Y,label=label),
            size=LFA_label_size,
            colour="darkblue",
            bg.colour="white",
            bg.r=0.12
        )
    }
    
    # ----------------------
    # Grid labels
    # ----------------------
    if(addGridLabels){
        p <- p + geom_sf_text(
            data = gridCent,
            aes(label = GRID_NO),
            family = "sans",
            colour="gray",
            size   = grid_label_size
        )
    }
    
    # ----------------------
    # NAFO labels
    # ----------------------
    if(addNAFOLabels){
        p <- p + geom_sf_text(
            data = labs,
            aes(label = lab),
            family = "sans"
        )
    }
    
    # ----------------------
    # Points
    # ----------------------
    if(addPoints && !is.null(pts)){
        
        # If pts has a 'group' column, plot as colored points
        if("group" %in% names(pts)){
            xy <- as.data.frame(st_coordinates(pts))
            xy$group <- pts$group
            p <- p + geom_point(data=xy, aes(x=X, y=Y, colour=group)) +
                coord_sf(xlim=xlim, ylim=ylim)
            
        } else {
            # Just plain points
            p <- p + geom_sf(data=pts, colour='#d44842', size=1.25) +
                coord_sf(xlim=xlim, ylim=ylim)
        }
        
        # Apply faceting if fw is provided
        if(!is.null(fw)){
            p <- p + facet_wrap(fw) +
                coord_sf(xlim=xlim, ylim=ylim)
        }
    }
    
    
    
    
    # ----------------------
    # Final map settings
    # ----------------------
    p <- p +
        coord_sf(xlim=xlim, ylim=ylim, expand=FALSE) +
        labs(x="Longitude", y="Latitude")
    
    if(return.object) return(p)
    p
}