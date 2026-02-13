#' @keywords internal
.plotJunctions <- function(p, junctions, coverage, lsv, arc_height) {

        layout <- .junctionArcLayout(junctions, coverage, arc_height)
        arcs <- .junctionArcPoints(layout)
        labels <- .junctionArcLabels(layout, junctions, lsv)

        p +
            ggplot2::geom_line(
                data = arcs,
                ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
                colour = "black", lineend = "round"
            ) +
            ggplot2::geom_label(
                data = labels,
                ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                label.padding = grid::unit(0, "pt"), size = 3, linewidth = 0
            )

}
