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
    introns <- .annotationArrowRange(introns)
    p <- ggplot2::ggplot()
    p <- .plotAnnotationIntrons(p, introns)
    p <- .plotAnnotationExons(
        p, df, ctx$input$anno_fill_by, ctx$input$anno_fill_colours
    )
    p <- .plotAnnotationLabels(
        p, df, ctx$input$anno_label_by, ctx$input$anno_label_colour,
        ctx$input$anno_label_size
    )
    p <- .plotHighlight(p, ctx$plot$highlight, ctx$input$highlight_colour)
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
.plotAnnotationExons <- function(p, df, fill_by, fill_colours) {
    if (is.null(fill_by)) {
        return(
            p + ggplot2::geom_tile(
                data = df,
                ggplot2::aes(
                    x = .data$start + (.data$width / 2), y = .data$y,
                    width = .data$width
                ),
                height = 0.3, colour = "black", fill = "black"
            )
        )
    }
    ## Ensure column acts as discrete
    df[[fill_by]] <- as.character(df[[fill_by]])
    p <- p + ggplot2::geom_tile(
        data = df,
        ggplot2::aes(
            x = .data$start + (.data$width / 2), y = .data$y,
            width = .data$width, fill = .data[[fill_by]]
        ),
        height = 0.3, colour = "black", show.legend = FALSE
    )
    if (!is.null(fill_by) && !is.null(fill_colours)) {
        p <- p + ggplot2::scale_fill_manual(values = fill_colours)
    }
    p
}

#' @keywords internal
.plotAnnotationIntrons <- function(p, introns) {
    if (!is.null(introns) && nrow(introns)) {
        plain_introns <- introns
        arrow_introns <- introns[introns$draw_arrow, ]
        if (nrow(plain_introns)) {
            p <- p + ggplot2::geom_segment(
                data = plain_introns,
                ggplot2::aes(x = .data$start, xend = .data$end, y = .data$y),
                linewidth = 0.4, colour = "black"
            )
        }
        if (nrow(arrow_introns)) {
            p <- p + ggplot2::geom_segment(
                data = arrow_introns,
                ggplot2::aes(
                    x = .data$arrow_start, xend = .data$arrow_end, y = .data$y,
                    yend = .data$y
                ),
                linewidth = 0.4, colour = "black",
                arrow = grid::arrow(
                    type = "closed", length = grid::unit(2.2, "mm")
                )
            )
        }
    }
    p
}

#' @keywords internal
.annotationArrowRange <- function(introns) {
    if (is.null(introns) || !nrow(introns)) return(introns)
    ## Data-space shaft is deliberately short; min_arrow controls visibility.
    arrow_width <- rep(1, nrow(introns))
    introns$arrow_start <- introns$midpoint - (arrow_width / 2)
    introns$arrow_end <- introns$midpoint + (arrow_width / 2)
    reverse <- introns$strand == "-"
    introns$arrow_start[reverse] <- introns$midpoint[reverse] +
        (arrow_width[reverse] / 2)
    introns$arrow_end[reverse] <- introns$midpoint[reverse] -
        (arrow_width[reverse] / 2)
    introns
}

#' @keywords internal
.plotAnnotationLabels <- function(
        p, df, label_by, label_colour, label_size
) {
    if (is.null(label_by)) return(p)
    p + ggplot2::geom_text(
        data = df,
        ggplot2::aes(
            x = .data$start + (.data$width / 2),
            y = .data$y, label = .data[[label_by]]
        ),
        colour = label_colour, size = label_size
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
            df$width <- df$end - df$start
            df$midpoint <- df$start + (df$width / 2)
            df$y <- unique(x$y)
            df$draw_arrow <- df$width >= min_arrow &
                df$strand %in% c("+", "-")
            df
        }
    })
    do.call(rbind, introns)
}
