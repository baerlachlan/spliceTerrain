#' @keywords internal
.plotAnnotation <- function(
        annotation
) {

    ## TODO: Let user input column to group annotations by
    df <- as.data.frame(annotation)
    exons <- data.frame(
        start = df$start, end = df$end, strand = df$strand, group = df$group
    )
    introns <- split(exons, exons[["group"]])
    ## TODO: Allow user to control min_arrow
    min_arrow <- 100L
    introns <- lapply(introns, \(x){
        n <- nrow(x)
        df <- data.frame(
            start = vapply(1:(n-1), \(i){x$end[i] + 1L}, integer(1)),
            end = vapply(2:n, \(i){x$start[i] - 1L}, integer(1)),
            strand = unique(x$strand),
            group = unique(x[["group"]])
        )
        df$width <- df$end - df$start
        df$midpoint <- df$start + (df$width / 2)
        df$arrow <- ifelse(
            df$width >= min_arrow & df$strand == "+", "\u276F",
            ifelse(
                df$width >= min_arrow & df$strand == "-", "\u276E",
                ""
            )
        )
        df
    })
    introns <- do.call(rbind, introns)

    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_rect(
        data = exons,
        ## TODO: height
        ggplot2::aes(xmin = start, xmax = end, y = group, height = 0.2),
        colour = "black", fill = "black"
    )
    p <- p + ggplot2::geom_segment(
        data = introns,
        ggplot2::aes(x = start, xend = end, y = group),
        linewidth = 0.4, colour = "black"
    )
    p <- p + ggplot2::geom_text(
        data = introns,
        ggplot2::aes(x = midpoint, y = group, label = arrow),
        vjust = 0.35, colour = "black"
    )
    p <- p + ggplot2::labs(x = "", y = "")
    p <- p + ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(colour = "black"),
        # axis.line.x = ggplot2::element_line(colour = "black")
        axis.line.x = ggplot2::element_blank()
        )
    p

}
