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
                ## Add 1bp to either side to avoid visual glitch
                xmin = .data$start - 1, xmax = .data$end + 1,
                ymin = 0, ymax = .data$coverage
            ),
            colour = "black", fill = "black", lineend = "square"
        )
        p <- p + ggplot2::scale_y_continuous(
            ## Don't show y < 0
            breaks = \(x) {
                b <- scales::breaks_extended()(x)
                b[b >= 0]
            },
            expand = c(0.1, 0.1) # helps lsv labels being cut
        )
        p <- p + ggplot2::labs(
            x = "", y = unique(coverage[[i]]$sample)
        )
        p
    })
    names(pl) <- names(plot_list)
    pl

}
