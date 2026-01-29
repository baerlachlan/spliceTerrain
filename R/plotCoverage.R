#' @keywords internal
.plotCoverage <- function(
        p, coverage
) {

    coverage <- as.data.frame(coverage)
    coverage <- split(coverage, coverage$sample)

    lapply(seq_along(p), \(i){
        # p[[i]] + ggplot2::geom_bar(
        #     data = coverage[[i]],
        #     ggplot2::aes(x = start, width = width, y = coverage),
        #     stat = "identity",
        #     colour = "black", fill = "black"
        # )
        p[[i]] <- p[[i]] + ggplot2::geom_rect(
            data = coverage[[i]],
            ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = coverage),
            colour = "black", fill = "black", lineend = "round", alpha = 1
        )
        p[[i]] <- p[[i]] + ggplot2::scale_y_continuous(
            labels = \(x) ifelse(x < 0, "", x),
            breaks = \(x) {
                b <- scales::breaks_extended()(x)
                b[b >= 0]
            }
        )
        p[[i]] <- p[[i]] + ggplot2::labs(
            x = "", y = unique(coverage[[i]]$sample)
        )
        p[[i]] <- p[[i]] + ggplot2::theme(
            axis.line.y = ggplot2::element_line(colour = "black")
        )
        p[[i]]
    })

}
