#' @keywords internal
.plotJunctions <- function(
        p, junctions, coverage, lsv, arc_height, colour, j_text_size,
        scale_arc_size
) {

    if (is.null(junctions)) return(p)
    if (is.null(coverage)) return(p)

    layout <- .junctionArcLayout(junctions, coverage, arc_height)
    arcs <- .junctionArcPoints(layout)
    labels <- .junctionArcLabels(layout, junctions, lsv)

    size_col <- ifelse(scale_arc_size, "size_on", "size_off")

    p +
        ggplot2::geom_line(
            data = arcs,
            ggplot2::aes(
                x = .data$x, y = .data$y, group = .data$id,
                size = .data[[size_col]]
            ),
            colour = colour, lineend = "round", show.legend = FALSE
        ) +
        ggplot2::scale_size(range = c(0.2,0.8)) +
        ggplot2::geom_label(
            data = labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            label.padding = grid::unit(0, "pt"),
            label.r = grid::unit(j_text_size, "pt"), linewidth = 0,
            size = j_text_size, text.colour = colour
        )
}
