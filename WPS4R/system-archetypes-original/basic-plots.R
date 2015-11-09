plot.kohcodes.my <-
    function (x, main, palette.name, bgcol=NULL, whatmap, codeRendering, 
              keepMargins, maxlegendcols, ...) 
    {
        if (!keepMargins) {
            opar <- par(c("mar", "ask"))
            on.exit(par(opar))
        }
        if (is.null(palette.name)) 
            palette.name <- terrain.colors
        whatmap <- check.whatmap(x, whatmap)
        nmaps <- length(whatmap)
        if (is.list(x$codes)) {
            if (prod(par("mfrow")) < nmaps) 
                par(ask = TRUE)
            for (i in 1:nmaps) {
                huhn <- x
                huhn$codes <- huhn$codes[[whatmap[i]]]
                if (length(main) == length(x$codes)) {
                    main.title <- main[whatmap[i]]
                }
                else {
                    if (length(main) == nmaps) {
                        main.title <- main[i]
                    }
                    else {
                        if (length(main) == 1) {
                            main.title <- main
                        }
                        else {
                            if (is.null(main)) {
                                if (!is.null(names(x$codes))) {
                                    main.title <- names(x$codes)[whatmap[i]]
                                }
                                else {
                                    main.title <- "Codes plot"
                                }
                            }
                        }
                    }
                }
                if (length(codeRendering) == length(x$codes)) {
                    cR <- codeRendering[whatmap[i]]
                }
                else {
                    if (length(codeRendering) == nmaps) {
                        cR <- codeRendering[i]
                    }
                    else {
                        cR <- codeRendering
                    }
                }
                plot.kohcodes(huhn, main = main.title, palette.name = palette.name, 
                              bgcol = bgcol, whatmap = NULL, codeRendering = cR, 
                              keepMargins = TRUE, ...)
            }
        }
        else {
            codes <- x$codes
            nvars <- ncol(codes)
            if (is.null(codeRendering)) {
                if (nvars < 15) {
                    codeRendering <- "segments"
                    maxlegendcols <- 3
                }
                else {
                    codeRendering <- "lines"
                }
            }
            margins <- rep(0.6, 4)
            if (!is.null(main)) 
                margins[3] <- margins[3] + 2
            par(mar = margins)
            if (codeRendering == "segments" & nvars < 40 & !is.null(colnames(codes))) {
                kohonen:::plot.somgrid(x$grid, ylim = c(max(x$grid$pts[, 2]) + min(x$grid$pts[, 
                                                                                              2]), -2))
                
                current.plot <- par("mfg")
                plot.width <- diff(par("usr")[1:2])
                cex <- 1
                leg.result <- legend(x = mean(x$grid$pts[, 1]), xjust = 0.5, 
                                     y = 0, yjust = 1, legend = colnames(codes), cex = cex, 
                                     plot = FALSE, ncol = min(maxlegendcols, nvars), 
                                     fill = palette.name(nvars))
                while (leg.result$rect$w > plot.width) {
                    cex <- cex * 0.9
                    leg.result <- legend(x = mean(x$grid$pts[, 1]), 
                                         xjust = 0.5, y = 0, yjust = 1, legend = colnames(codes), 
                                         cex = cex, plot = FALSE, ncol = min(maxlegendcols, 
                                                                             nvars), fill = palette.name(nvars))
                }
                leg.result <- legend(x = mean(x$grid$pts[, 1]), xjust = 0.5, 
                                     y = 0, yjust = 1, cex = cex, legend = colnames(codes), 
                                     plot = FALSE, ncol = min(maxlegendcols, nvars), 
                                     fill = palette.name(nvars), ...)
                par(mfg = current.plot)
                kohonen:::plot.somgrid(x$grid, ylim = c(max(x$grid$pts[, 2]) + min(x$grid$pts[, 
                                                                                              2]), -leg.result$rect$h))
                legend(x = mean(x$grid$pts[, 1]), xjust = 0.5, y = 0, 
                       yjust = 1, cex = cex, plot = TRUE, legend = colnames(codes), 
                       ncol = min(maxlegendcols, nvars), fill = palette.name(nvars), 
                       ...)
            }
            else {
                kohonen:::plot.somgrid(x$grid, ...)
            }
            title.y <- max(x$grid$pts[, 2]) + 1.2
            if (title.y > par("usr")[4] - 0.2) {
                title(main)
            }
            else {
                text(mean(range(x$grid$pts[, 1])), title.y, main, 
                     adj = 0.5, cex = par("cex.main"), font = par("font.main"))
            }
            if (is.null(bgcol)) 
                bgcol <- "transparent"
            symbols(x$grid$pts[, 1], x$grid$pts[, 2], circles = rep(0.5, 
                                                                    nrow(x$grid$pts)), inches = FALSE, add = TRUE, bg = bgcol)
            if (codeRendering == "lines") {
                yrange <- range(codes)
                codes <- codes - mean(yrange)
            }
            else {
                codemins <- apply(codes, 2, min)
                codes <- sweep(codes, 2, codemins)
            }
            switch(codeRendering, segments = {
                stars(codes, locations = x$grid$pts, labels = NULL, 
                      len = 0.4, add = TRUE, col.segments = palette.name(nvars), 
                      draw.segments = TRUE)
            }, lines = {
                for (i in 1:nrow(x$grid$pts)) {
                    if (yrange[1] < 0 & yrange[2] > 0) {
                        lines(seq(x$grid$pts[i, 1] - 0.4, x$grid$pts[i, 
                                                                     1] + 0.4, length = 2), rep(x$grid$pts[i, 
                                                                                                           2], 2), col = "gray")
                    }
                    lines(seq(x$grid$pts[i, 1] - 0.4, x$grid$pts[i, 
                                                                 1] + 0.4, length = ncol(codes)), x$grid$pts[i, 
                                                                                                             2] + codes[i, ] * 0.8/diff(yrange), col = "red")
                }
            }, stars = stars(codes, locations = x$grid$pts, labels = NULL, 
                             len = 0.4, add = TRUE))
        }
        invisible()
    }

################################################################################
# color palettes, suggested by Leo Lopes
coolBlueHotRed <- function(n, alpha = 1) {
    rainbow(n, end=4/6, alpha=alpha)[n:1]
}
repRainbow <- function(n, length.col=6) {
    n.compl <- n %/% length.col
    n.rest <-  n %% length.col
    col.vec <- numeric(0)
    if (n.compl > 0)
        col.vec <- rep(rainbow(length.col), n.compl)
    if (n.rest > 0)
        col.vec <- c(col.vec, rainbow(n.compl, end= n.rest/ length.col ))
    
    return(col.vec)
}

