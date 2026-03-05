#' @importFrom rlang .data
#' @keywords internal
.plotCoverage <- function(
        p, coverage, colour
) {

    if (is.null(coverage)) return(p)

    coverage <- as.data.frame(coverage)

    p +
        ggplot2::geom_bar(
            data = coverage,
            ggplot2::aes(
                .data$start, .data$coverage
            ),
            stat = "identity", width = 1, colour = colour, fill = colour
        )

}
