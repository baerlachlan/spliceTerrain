#' @importFrom rlang .data
#' @keywords internal
.plotAnnotation <- function(ctx) {
    if (is.null(ctx$plot$annotation) || !length(ctx$plot$annotation))
        return(ctx)
    df <- as.data.frame(ctx$plot$annotation)
    group_y <- stats::setNames(seq_along(unique(df$group)), unique(df$group))
    df$y <- unname(group_y[df$group])
    exons <- split(df, df[["group"]])
    introns <- .getIntrons(exons, ctx$input$min_arrow)
    arrowheads <- .annotationArrowheads(introns)
    p <- ggplot2::ggplot()
    p <- .plotHighlight(p, ctx$plot$highlight, ctx$input$highlight_colour)
    p <- .plotAnnotationExons(p, df)
    p <- .plotAnnotationIntrons(p, introns, arrowheads)
    p <- .plotAnnotationLabels(
        p, df, ctx$input$anno_text_col, ctx$input$anno_text_size
    )
    p <- p + ggplot2::coord_cartesian(
        xlim = c(
            BiocGenerics::start(ctx$plot$region),
            BiocGenerics::end(ctx$plot$region)
        )
    )
    p <- p + ggplot2::scale_y_continuous(
        breaks = unname(group_y), labels = names(group_y)
    )
    p <- p + ggplot2::labs(x = "", y = "")
    p <- p + ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank()
    )
    ctx$plot$plist <- c(ctx$plot$plist, list(annotation = p))
    ctx
}

#' @keywords internal
.plotAnnotationExons <- function(p, df) {
    p + ggplot2::geom_tile(
        data = df,
        ggplot2::aes(
            x = .data$start + (.data$width / 2), y = .data$y,
            width = .data$width
        ),
        height = 0.3, colour = "black", fill = "black"
    )
}

#' @keywords internal
.plotAnnotationIntrons <- function(p, introns, arrowheads) {
    if (!is.null(introns) && nrow(introns)) {
        p <- p + ggplot2::geom_segment(
            data = introns,
            ggplot2::aes(x = .data$start, xend = .data$end, y = .data$y),
            linewidth = 0.4, colour = "black"
        )
    }
    if (!is.null(arrowheads) && nrow(arrowheads)) {
        p <- p + ggplot2::geom_polygon(
            data = arrowheads,
            ggplot2::aes(
                x = .data$x, y = .data$y, group = .data$arrow_id
            ),
            fill = "black", colour = NA
        )
    }
    p
}

#' @keywords internal
.plotAnnotationLabels <- function(p, df, text_col, text_size) {
    if (is.null(text_col) || is.null(df[[text_col]])) return(p)
    p + ggplot2::geom_text(
        data = df,
        ggplot2::aes(
            x = .data$start + (.data$width / 2),
            y = .data$y, label = .data[[text_col]]
        ),
        colour = "white", size = text_size
    )
}

#' @keywords internal
.getIntrons <- function(exons, min_arrow) {
    introns <- lapply(exons, \(x) {
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
            df$y <- unique(x$y)
            df$draw_arrow <- df$width >= min_arrow &
                df$strand %in% c("+", "-")
            df
        }
    })
    do.call(rbind, introns)
}

#' @keywords internal
.annotationArrowheads <- function(introns) {
    if (is.null(introns) || !nrow(introns)) return(NULL)
    introns <- introns[introns$draw_arrow, ]
    if (!nrow(introns)) return(NULL)

    arrow_width <- min(introns$width * 0.15, 150)
    ## Centre on geometric centroid
    tip_x <- ifelse(
        introns$strand == "+",
        introns$midpoint + (2 * arrow_width / 3),
        introns$midpoint - (2 * arrow_width / 3)
    )
    base_x <- ifelse(
        introns$strand == "+",
        introns$midpoint - (arrow_width / 3),
        introns$midpoint + (arrow_width / 3)
    )

    data.frame(
        arrow_id = rep(seq_len(nrow(introns)), each = 3),
        x = c(rbind(tip_x, base_x, base_x)),
        y = c(rbind(introns$y, introns$y + 0.08, introns$y - 0.08))
    )
}
