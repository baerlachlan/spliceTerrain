#' @importFrom rlang .data
#' @keywords internal
.plotAnnotation <- function(
        plist, annotation, min_arrow, highlight, highlight_colour,
        ann_text_col, ann_text_size
) {

    if (is.null(annotation) || !length(annotation)) return(plist)

    df <- as.data.frame(annotation)
    introns <- split(df, df[["group"]])
    introns <- lapply(introns, \(x){
        n <- nrow(x)
        if (n > 1) {
            df <- data.frame(
                start = vapply(seq_len(n-1), \(i){x$end[i] + 1L}, integer(1)),
                end = vapply(2:n, \(i){x$start[i] - 1L}, integer(1)),
                strand = unique(x$strand),
                group = unique(x[["group"]])
            )
            df$width <- df$end - df$start - 1 # 1-based
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
    if (!is.null(highlight)) {
        p <- .plotHighlight(p, highlight, highlight_colour)
    }
    p <- p + ggplot2::geom_rect(
        data = df,
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
    p <- p + ggplot2::geom_text(
        data = introns,
        ggplot2::aes(x = .data$midpoint, y = .data$group, label = .data$arrow),
        vjust = 0.37, colour = "black"
    )
    if (!is.null(ann_text_col))
        p <- p + ggplot2::geom_text(
            data = df,
            ggplot2::aes(
                x = .data$start + (.data$width / 2),
                y = .data$group, label = .data[[ann_text_col]]
            ),
            colour = "white", size = ann_text_size
        )
    p <- p + ggplot2::labs(x = "", y = "")
    p <- p + ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank()
    )
    c(plist, list(p))

}
