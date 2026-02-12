#' @importFrom rlang .data
#' @keywords internal
.junctionGeoms <- function(p, arcs, labels) {

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
