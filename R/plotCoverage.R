#' @importFrom rlang .data
#' @keywords internal
.plotCoverage <- function(
        p, coverage, colour
) {

    if (is.null(coverage)) return(p)

    coverage <- as.data.frame(coverage)

    p +
        ggplot2::geom_rect(
            data = coverage,
            ggplot2::aes(
                ## Add 1bp to either side to avoid visual artefact
                xmin = .data$start - 1, xmax = .data$end + 1,
                ymin = 0, ymax = .data$coverage
            ),
            colour = colour, fill = colour, lineend = "square"
        )

}
