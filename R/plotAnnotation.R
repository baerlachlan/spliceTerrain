#' @importFrom rlang .data
#' @keywords internal
.plotAnnotation <- function(
        plot_list, annotation, min_arrow
) {
    # browser()
    ## TODO: Let user input column to group annotations by
    df <- as.data.frame(annotation)
    exons <- data.frame(
        start = df$start, end = df$end, strand = df$strand, group = df$group
    )
    introns <- split(exons, exons[["group"]])
    introns <- lapply(introns, \(x){
        # browser()
        n <- nrow(x)
        if (n > 1) {
            df <- data.frame(
                start = vapply(1:(n-1), \(i){x$end[i] + 1L}, integer(1)),
                end = vapply(2:n, \(i){x$start[i] - 1L}, integer(1)),
                strand = unique(x$strand),
                group = unique(x[["group"]])
            )
            df$width <- df$end - df$start
            df$midpoint <- df$start + (df$width / 2)
            df$arrow <- ifelse(
                # df$width >= min_arrow & df$strand == "+", "\u276F",
                df$width >= min_arrow & df$strand == "+", "\u2B9E",
                ifelse(
                    # df$width >= min_arrow & df$strand == "-", "\u276E",
                    df$width >= min_arrow & df$strand == "-", "\u2B9C",
                    ""
                )
            )
            df
        }
    })
    introns <- do.call(rbind, introns)

    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_rect(
        data = exons,
        ## TODO: height
        ggplot2::aes(xmin = .data$start, xmax = .data$end, y = .data$group),
        height = 0.3, colour = "black", fill = "black"
    )
    p <- p + ggplot2::geom_segment(
        data = introns,
        ggplot2::aes(x = .data$start, xend = .data$end, y = .data$group),
        linewidth = 0.4, colour = "black"
    )
    p <- p + ggplot2::geom_text(
        data = introns,
        ggplot2::aes(x = .data$midpoint, y = .data$group, label = .data$arrow),
        vjust = 0.37, colour = "black"
    )
    p <- p + ggplot2::labs(x = "", y = "")
    p <- p + ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank(),
        # axis.line.x = ggplot2::element_blank()
    )
    c(plot_list, list(p))

}
