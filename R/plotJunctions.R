#' @keywords internal
.plotJunctions <- function(
        p, juncs, cov, psi, arc_height, colour, junc_text_size, scale_arcs
) {
    if (is.null(juncs)) return(p)
    if (is.null(cov)) return(p)
    layout <- .junctionArcLayout(juncs, cov, arc_height)
    arcs <- .junctionArcPoints(layout)
    labels <- .junctionArcLabels(layout, juncs, psi)
    size_col <- ifelse(scale_arcs, "size_on", "size_off")
    p + ggplot2::geom_line(
        data = arcs,
        ggplot2::aes(
            x = .data$x, y = .data$y, group = .data$id,
            linewidth = .data[[size_col]]
        ),
        colour = colour, lineend = "round", show.legend = FALSE
    ) +
        ggplot2::scale_linewidth(range = c(0.2, 0.8)) +
        ggplot2::geom_label(
            data = labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            label.padding = grid::unit(0, "pt"),
            label.r = grid::unit(junc_text_size, "pt"), linewidth = 0,
            size = junc_text_size, text.colour = colour
        )
}
