#' @importFrom rlang .data
#' @keywords internal
.plotCoverage <- function(
        plot_list, coverage
) {

    coverage <- as.data.frame(coverage)
    coverage <- split(coverage, coverage$sample)

    pl <- lapply(names(plot_list), \(i){
        p <- plot_list[[i]] + ggplot2::geom_rect(
            data = coverage[[i]],
            ggplot2::aes(
                xmin = .data$start, xmax = .data$end,
                ymin = 0, ymax = .data$coverage
            ),
            colour = "black", fill = "black", lineend = "square", alpha = 1
        )
        p <- p + ggplot2::scale_y_continuous(
            labels = \(x) ifelse(x < 0, "", x),
            breaks = \(x) {
                b <- scales::breaks_extended()(x)
                b[b >= 0]
            }
        )
        p <- p + ggplot2::labs(
            x = "", y = unique(coverage[[i]]$sample)
        )
        p
    })
    names(pl) <- names(plot_list)
    pl

}
