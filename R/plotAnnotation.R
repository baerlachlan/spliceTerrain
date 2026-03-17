#' @importFrom rlang .data
#' @keywords internal
.plotAnnotation <- function(ctx) {
    if (is.null(ctx$args$annotation) || !length(ctx$args$annotation))
        return(ctx)
    df <- as.data.frame(ctx$args$annotation)
    exons <- split(df, df[["group"]])
    introns <- .getIntrons(exons, ctx$args$min_arrow)
    p <- ggplot2::ggplot()
    p <- .plotHighlight(p, ctx$args$highlight, ctx$args$highlight_colour)
    p <- p + ggplot2::geom_rect(
        data = df,
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
        vjust = 0.35, colour = "black"
    )
    if (!is.null(ctx$args$anno_text_col))
        if (exists(ctx$args$annotation[[ctx$args$anno_text_col]]))
            p <- p + ggplot2::geom_text(
                data = df,
                ggplot2::aes(
                    x = .data$start + (.data$width / 2),
                    y = .data$group, label = .data[[ctx$args$anno_text_col]]
                ),
                colour = "white", size = ctx$args$anno_text_size
            )
    p <- p + ggplot2::labs(x = "", y = "")
    p <- p + ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank()
    )
    ctx$plot$plist <- c(ctx$plot$plist, list(p))
    ctx
}


#' @keywords internal
.getIntrons <- function(exons, min_arrow) {
    introns <- lapply(exons, \(x){
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
                # df$width >= min_arrow & df$strand == "+", "\u2B9E",
                df$width >= min_arrow & df$strand == "+", "\u25B8",
                ifelse(
                    # df$width >= min_arrow & df$strand == "-", "\u276E",
                    # df$width >= min_arrow & df$strand == "-", "\u2B9C",
                    df$width >= min_arrow & df$strand == "-", "\u25C2",
                    ""
                )
            )
            df
        }
    })
    do.call(rbind, introns)
}
