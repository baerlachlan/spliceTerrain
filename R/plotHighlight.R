#' @importFrom rlang .data
#' @keywords internal
.plotHighlight <- function(
        p, highlight
) {

    highlight <- as.data.frame(highlight)

    p + ggplot2::geom_rect(
        data = highlight,
        ggplot2::aes(
            xmin = .data$start, xmax = .data$end,
            ymin = -Inf, ymax = Inf
        ),
        colour = scales::alpha("red", 0.2), fill = "red",
        lineend = "square", alpha = 0.2
    )

}
