#' @keywords internal
.plotTerrain <- function(
        plist, region, map, panel_heights
) {

    p <- patchwork::wrap_plots(plist, ncol = 1, heights = panel_heights)
    p <- p + patchwork::plot_layout(
        axes = "collect_x"
    )
    p <- p & ggplot2::coord_cartesian(
        xlim = c(
            min(BiocGenerics::start(region)),
            max(BiocGenerics::end(region))
        )
    )
    breaks <- seq(
        BiocGenerics::start(region), BiocGenerics::end(region), length.out = 5
    )
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
        axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
        plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
    p

}
