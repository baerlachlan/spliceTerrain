#' @keywords internal
.plotTerrain <- function(
        plist, region, map, panel_heights, axis_title_size, axis_text_size
) {

    st <- BiocGenerics::start(region)
    en <- BiocGenerics::end(region)
    w <- BiocGenerics::width(region)

    p <- patchwork::wrap_plots(plist, ncol = 1, heights = panel_heights)
    p <- p + patchwork::plot_layout(
        axes = "collect_x"
    )
    p <- p & ggplot2::coord_cartesian(xlim = c(st, en), clip = "off")
    n_break <- ifelse(w < 5, w, 5)
    breaks <- seq(st, en, length.out = n_break)
    if (is.null(map)) {
        labels <- scales::comma(breaks)
    } else {
        labels <- scales::comma(round(.mapPlotToGenome(breaks, map)))
    }
    p <- p & ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
    p <- p & ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "grey90",),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(colour = "grey70"),
        axis.line.x = ggplot2::element_line(colour = "grey70"),
        axis.title.y = ggplot2::element_text(
            angle = 0, vjust = 0.5, size = axis_title_size
            ),
        axis.text = ggplot2::element_text(size = axis_text_size),
        plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
    p

}
