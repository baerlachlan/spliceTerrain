#' @importFrom rlang .data
#' @keywords internal
.plotHighlight <- function(p, highlight, highlight_colour, region) {
    if (is.null(highlight)) return(p)
    highlight <- IRanges::restrict(
        highlight, start = BiocGenerics::start(region),
        end = BiocGenerics::end(region), keep.all.ranges = FALSE
    )
    highlight <- highlight[BiocGenerics::width(highlight) > 0L]
    if (!length(highlight)) return(p)
    highlight <- as.data.frame(highlight)
    p + ggplot2::geom_rect(
        data = highlight,
        ggplot2::aes(
            xmin = .data$start, xmax = .data$end, ymin = -Inf, ymax = Inf
        ),
        colour = highlight_colour, fill = highlight_colour, lineend = "square"
    )
}
