#' @importFrom rlang .data
#' @keywords internal
.plotJunctions <- function(
        plot_list, junctions
) {

    junctions <- split(junctions, junctions$sample)

    pl <- lapply(names(plot_list), \(i){
        if (!length(junctions[[i]])) return(plot_list[[i]])
        ## Levels control height of arc to avoid overlapping
        levels <- IRanges::disjointBins(junctions[[i]])
        df <- as.data.frame(junctions[[i]])
        max_cov <- max(df$coverage)
        ## TODO: maybe allow user to control h_step with e.g. j_height arg
        ## (default 0.5)
        h_step <- -0.5 * max_cov
        heights <- h_step + (levels - 1L) * h_step

        ## midpoint + half-width
        mid <- (df$start + df$end) / 2
        hw <- (df$end - df$start) / 2

        ## Generate points along each arc
        t <- seq(-1, 1, length.out = 100L)
        x <- lapply(seq_along(mid), \(i){mid[i] + hw[i] * t})
        x <- unlist(x)
        y <- lapply(heights, \(x){x * (1 - t^8)})
        y <- unlist(y)

        id <- rep(seq_len(nrow(df)), each = 100L)

        arcs <- data.frame(x = x, y = y, id = id)
        labels <- data.frame(
            x = mid, y = heights, label = as.character(df$coverage)
        )

        p <- plot_list[[i]] + ggplot2::geom_line(
            data = arcs,
            ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
            colour = "black", lineend = "round"
        )
        p <- p + ggplot2::geom_label(
            data = labels,
            ggplot2:: aes(x = .data$x, y = .data$y, label = .data$label),
            label.padding = grid::unit(0, "lines"),
            border.colour = "white", size = 3
        )
        p
    })
    names(pl) <- names(plot_list)
    pl

}
