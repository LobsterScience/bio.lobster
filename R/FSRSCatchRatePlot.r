#' @export
			
FSRSCatchRatePlot <- function(
        recruits = NULL,
        legals = NULL,
        usr = NULL,
        lrp = NULL,
        save = TRUE,
        name.labels = FALSE,
        lfa = NULL,
        rm = FALSE,
        French = FALSE,
        fd = file.path(project.figuredirectory('bio.lobster','ReferencePoints')),
        title = paste('LFA', lfa),
        fn = paste0('FSRSRecruitCatchRate', lfa),
        ...
) {
    dir.create(fd, recursive = TRUE, showWarnings = FALSE)
    par(las = 1)
    lab <- NULL
    
    # Layout for two panels
    if (!is.null(recruits) && !is.null(legals)) {
        par(mfrow = c(2, 1), mar = c(0, 4, 0, 2), omi = c(1, 0.25, 0.1, 0.25))
        lab <- c('(a)', '(b)')
    }
    
    if (name.labels) {
        lab <- c('Recruits', 'Legals')
    }
    
    if (French) {
        lobtext <- 'Homards/casier'
        yrtext  <- 'Année'
    } else {
        lobtext <- 'Lobsters / Trap'
        yrtext  <- 'Year'
    }
    
    ymaxr <- ifelse(!is.null(recruits), max(recruits[,-1], na.rm = TRUE), 0)
    ymaxl <- ifelse(!is.null(legals), max(legals[,-1], na.rm = TRUE), 0)
    ymax  <- max(c(ymaxr, ymaxl, lrp, usr), na.rm = TRUE)
    
    # --- RECRUITS panel ---
    if (!is.null(recruits)) {
        plot(recruits[,1], recruits[,2],
             xlab = '', ylab = '', type = 'n', main = '',
             ylim = c(0, 1.15 * ymax), axes = FALSE, ...)
        title(title, line = -1.1, cex.main = 1.5)
        
        polygon(c(recruits[,1], rev(recruits[,1])),
                c(recruits[,3], rev(recruits[,4])),
                col = 'grey', border = NA)
        lines(recruits[,1], recruits[,2], lwd = 2, pch = 16, type = 'b')
        
        if (rm) {
            recruits$running.median <- rmed(recruits[,1], recruits[,2])$x
            lines(recruits[,1], recruits$running.median, col = 'blue', lty = 2, lwd = 3)
        }
        
        axis(2)
        axis(4, lab = FALSE)
        box()
        
        if (!is.null(usr)) abline(h = usr, col = 'green', lwd = 2, lty = 2)
        if (!is.null(lrp)) abline(h = lrp, col = 'red', lwd = 2, lty = 3)
        
        # --- Label inside top-left corner ---
        usr.coords <- par("usr")
        text(x = usr.coords[1] + 0.02 * diff(usr.coords[1:2]),
             y = usr.coords[4] - 0.03 * diff(usr.coords[3:4]),
             labels = lab[1],
             adj = c(0, 1),
             cex = 1.25)
    }
    
    # --- LEGALS panel ---
    if (!is.null(legals)) {
        plot(legals[,1], legals[,2],
             xlab = '', ylab = '', type = 'n', main = '',
             ylim = c(0, 1.15 * ymax), ...)
        polygon(c(legals[,1], rev(legals[,1])),
                c(legals[,3], rev(legals[,4])),
                col = 'grey', border = NA)
        lines(legals[,1], legals[,2], lwd = 2, pch = 16, type = 'b')
        
        if (rm) {
            legals$running.median <- rmed(legals[,1], legals[,2])$x
            lines(legals[,1], legals$running.median, col = 'blue', lty = 2, lwd = 3)
        }
        
        axis(1)
        axis(2)
        axis(4, lab = FALSE)
        box()
        
        if (!is.null(usr)) abline(h = usr, col = 'green', lwd = 2, lty = 2)
        if (!is.null(lrp)) abline(h = lrp, col = 'red', lwd = 2, lty = 3)
        
        # --- Label inside top-left corner ---
        usr.coords <- par("usr")
        text(x = usr.coords[1] + 0.02 * diff(usr.coords[1:2]),
             y = usr.coords[4] - 0.03 * diff(usr.coords[3:4]),
             labels = lab[2],
             adj = c(0, 1),
             cex = 1.25)
    }
    
    # --- Outer axis labels ---
    mtext(lobtext, side = 2, line = -1, outer = TRUE, las = 0)
    mtext(yrtext,  side = 1, line = 2,  outer = TRUE)
    
    # --- Save outputs ---
    if (save) {
        savePlot(file.path(fd, paste(fn, 'png', sep='.')), type='png')
    }
    if (!is.null(recruits))
        write.csv(recruits, file.path(fd, paste(fn, 'recruits.csv', sep='.')))
    if (!is.null(legals))
        write.csv(legals, file.path(fd, paste(fn, 'legals.csv', sep='.')))
    
    print(lfa)
}

