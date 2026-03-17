#' @keywords internal
.plotSamples <- function(ctx) {
    cov <- split(ctx$data$cov, ctx$data$cov$sample)
    juncs <- split(ctx$data$juncs, ctx$data$juncs$sample)
    ctx$plot$plist <- lapply(ctx$args$bam, \(i){ggplot2::ggplot()})
    out <- lapply(names(ctx$plot$plist), \(i){
        p <- ctx$plot$plist[[i]]
        p <- .plotCoverage(p, cov[[i]], ctx$args$colours[[i]])
        p <- .plotJunctions(
            p, juncs[[i]], cov[[i]], ctx$args$lsv, ctx$args$arc_height,
            ctx$args$colours[[i]], ctx$args$junc_text_size, ctx$args$scale_arcs
        )
        p <- .plotHighlight(p, ctx$args$highlight, ctx$args$highlight_colour)
        p <- p + ggplot2::scale_y_continuous(
            breaks = \(x) { # Don't show y < 0
                b <- scales::breaks_extended()(x)
                b[b >= 0]
            },
            expand = c(0.1, 0.1) # Gives labels more room
        )
        p + ggplot2::labs(x = "", y = i)
    })
    st <- BiocGenerics::start(ctx$args$region)
    en <- BiocGenerics::end(ctx$args$region)
    ylim <- NULL
    if (ctx$args$common_y) {
        ## TODO: check this is the best way to access ggplot obj data
        ys <- unlist(lapply(out, \(x){
            pdat <- ggplot2::ggplot_build(x)@data
            unlist(lapply(pdat, `[[`, "y"))
        }))
        if (length(ys))
            ylim <- c(min(ys), max(ys))
    }
    out <- lapply(out, \(p){
        p + ggplot2::coord_cartesian(
            xlim = c(st, en), ylim = ylim, clip = "off"
        )
    })
    names(out) <- names(ctx$plot$plist)
    ctx$plot$plist <- out
    ctx
}
