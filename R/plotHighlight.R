#' @importFrom rlang .data
#' @keywords internal
.plotHighlight <- function(
        p, highlight, highlight_colour
) {

    highlight <- as.data.frame(highlight)

    p + ggplot2::geom_rect(
        data = highlight,
        ggplot2::aes(
            xmin = .data$start, xmax = .data$end,
            ymin = -Inf, ymax = Inf
        ),
        colour = highlight_colour, fill = highlight_colour,
        lineend = "square"
    )

}
