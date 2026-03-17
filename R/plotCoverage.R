#' @importFrom rlang .data
#' @keywords internal
.plotCoverage <- function(p, cov, colour) {
    if (is.null(cov)) return(p)
    cov <- as.data.frame(cov)
    p + ggplot2::geom_bar(
        data = cov,
        ggplot2::aes(.data$start, .data$coverage),
        stat = "identity", width = 1, colour = colour, fill = colour
    )
}
