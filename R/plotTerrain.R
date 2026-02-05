#' @keywords internal
.plotTerrain <- function(
        plot_list, region
) {

    # plot_list <- c(p_bam, list(p_ann))
    p <- patchwork::wrap_plots(plot_list, ncol = 1)
    p <- p + patchwork::plot_layout(
        axes = "collect_x"
    )
    p <- p & ggplot2::coord_cartesian(
        xlim = c(
            min(BiocGenerics::start(region)),
            max(BiocGenerics::end(region))
        )
    )
    p <- p & ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "grey90",),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(colour = "grey70"),
        axis.line.x = ggplot2::element_line(colour = "grey70"),
        axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
        plot.margin = ggplot2::margin(5,5,5,5)
        # plot.margin = ggplot2::margin(0,0,0,0)
    )
    p

}
