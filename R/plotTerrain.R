#' @keywords internal
.plotTerrain <- function(
        plist, region, map, panel_heights, axis_title_size, axis_text_size,
        default_theme
) {

    st <- BiocGenerics::start(region)
    en <- BiocGenerics::end(region)
    w <- BiocGenerics::width(region)

    p <- patchwork::wrap_plots(plist, ncol = 1, heights = panel_heights)
    p <- p + patchwork::plot_layout(axes = "collect_x")
    n_break <- ifelse(w < 5, w, 5)
    breaks <- seq(st, en, length.out = n_break)
    if (is.null(map)) {
        labels <- scales::comma(round(breaks))
    } else {
        labels <- scales::comma(round(.mapPlotToGenome(breaks, map)))
    }
    p <- p & ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
    if (!default_theme) {
        p <- p & .theme_spliceTerrain(
            axis_title_size = axis_title_size, axis_text_size = axis_text_size
        )
    }
    p

}
