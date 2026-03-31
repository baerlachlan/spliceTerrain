#' @importFrom ggplot2 %+replace%
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
                angle = 0, vjust = 0.5, size = axis_title_size
            ),
            axis.text = ggplot2::element_text(size = axis_text_size),
            plot.margin = ggplot2::margin(5, 5, 5, 5)
        )
}
