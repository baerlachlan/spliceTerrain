#' @keywords internal
.plotTerrain <- function(ctx) {
    st <- BiocGenerics::start(ctx$args$region)
    en <- BiocGenerics::end(ctx$args$region)
    w <- BiocGenerics::width(ctx$args$region)
    p <- patchwork::wrap_plots(
        ctx$plot$plist, ncol = 1, heights = ctx$args$panel_heights
    )
    p <- p + patchwork::plot_layout(axes = "collect_x")
    n_break <- ifelse(w < 5, w, 5)
    breaks <- seq(st, en, length.out = n_break)
    if (is.null(ctx$plot$map)) {
        labels <- scales::comma(round(breaks))
    } else {
        labels <- scales::comma(round(.mapPlotToGenome(breaks, ctx$plot$map)))
    }
    p <- p & ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
    p <- p & .theme_spliceTerrain(
        axis_title_size = ctx$args$axis_title_size,
        axis_text_size = ctx$args$axis_text_size
    )
    p
}
