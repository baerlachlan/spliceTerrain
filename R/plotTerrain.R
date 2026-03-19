#' @keywords internal
.plotTerrain <- function(ctx) {
    st <- BiocGenerics::start(ctx$args$region)
    en <- BiocGenerics::end(ctx$args$region)
    w <- BiocGenerics::width(ctx$args$region)
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
            axis_title_size = ctx$args$axis_title_size,
            axis_text_size = ctx$args$axis_text_size
        )
    })
    if (ctx$args$return_data) return(ctx)
    p <- patchwork::wrap_plots(
        ctx$plot$plist, ncol = 1, heights = ctx$args$panel_heights
    )
    p <- p + patchwork::plot_layout(axes = "collect_x")
    p
}
