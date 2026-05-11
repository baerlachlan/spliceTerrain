#' @importFrom ggplot2 %+replace%
#' @keywords internal
.plotTerrain <- function(ctx) {
    st <- BiocGenerics::start(ctx$plot$region)
    en <- BiocGenerics::end(ctx$plot$region)
    w <- BiocGenerics::width(ctx$plot$region)
    n_break <- ifelse(w < 5, w, 5)
    breaks <- seq(st, en, length.out = n_break)
    if (is.null(ctx$plot$map)) {
        labels <- scales::comma(round(breaks))
    } else {
        labels <- scales::comma(round(.mapPlotToGenome(breaks, ctx$plot$map)))
    }
    ctx$plot$plist <- lapply(ctx$plot$plist, \(p){
        p <- p + ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
        p + .theme_spliceTerrain(
            axis_title_size = ctx$input$axis_title_size,
            axis_text_size = ctx$input$axis_text_size
        )
    })
    p <- patchwork::wrap_plots(
        ctx$plot$plist, ncol = 1, heights = ctx$input$panel_heights
    )
    p <- p + patchwork::plot_layout(axes = "collect_x")
    p
}

#' @keywords internal
.mapPlotToGenome <- function(pos, map) {
    ## Preallocate output to keep input order
    out <- rep(NA_real_, length(pos))
    ## Positions that fall within mapped regions
    hits <- IRanges::findOverlaps(
        IRanges::IRanges(pos, pos),
        IRanges::IRanges(map$p_start, map$p_end)
    )
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    if (length(qh)) out[qh] <- map$g_start[sh] + (pos[qh] - map$p_start[sh])
    ## For positions that fall in gaps, approximate by proportional distance
    nohit <- setdiff(seq_along(pos), qh)
    px <- pos[nohit]
    ## Find the last block whose p_end is <= px
    left <- findInterval(px, map$p_end)
    right <- left + 1
    ## Plot-level gap between blocks
    gap_p_start <- map$p_end[left]
    gap_p_end <- map$p_start[right]
    gap_p_w <- gap_p_end - gap_p_start
    ## Fraction through the plot gap
    frac <- (px - gap_p_start) / gap_p_w
    ## Genomic-level gap between blocks
    gap_g_start <- map$g_end[left]
    gap_g_end <- map$g_start[right]
    gap_g_w <- gap_g_end - gap_g_start
    out[nohit] <- gap_g_start + frac * gap_g_w
    out
}

#' @keywords internal
.theme_spliceTerrain <- function(axis_title_size, axis_text_size, ...) {
    ggplot2::theme_bw(...) %+replace%
        ggplot2::theme(
            panel.background = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(colour = "grey90",),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line(colour = "grey70"),
            axis.line.x = ggplot2::element_line(colour = "grey70"),
            axis.title.y = ggplot2::element_text(
                angle = 90, size = axis_title_size
            ),
            axis.text = ggplot2::element_text(size = axis_text_size),
            plot.margin = ggplot2::margin(5, 5, 5, 5)
        )
}
