#' @keywords internal
.plotTerrain <- function(
        region, p_bam, p_ann
) {

    plot_list <- c(p_bam, list(p_ann))
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
        plot.margin = ggplot2::margin(5,5,5,5)
    )
    p

}
