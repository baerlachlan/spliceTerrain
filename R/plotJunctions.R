#' @keywords internal
.plotJunctions <- function(
        p, junctions
) {

    ## Levels control height of arc to avoid overlapping
    junctions <- split(junctions, junctions$sample)

    lapply(seq_along(junctions), \(i){
        levels <- IRanges::disjointBins(junctions[[i]])
        df <- as.data.frame(junctions[[i]])
        max_cov <- max(df$coverage)
        h_step <- -0.5 * max_cov
        heights <- h_step + (levels - 1L) * h_step

        ## midpoint + half-width
        mid <- (df$start + df$end) / 2
        hw <- (df$end - df$start) / 2

        # Generate points along each arc
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

        p <- p[[i]] + ggplot2::geom_line(
            data = arcs,
            ggplot2::aes(x = x, y = y, group = id),
            colour = "black", lineend = "round"
        )
        p <- p + ggplot2::geom_label(
            data = labels,
            ggplot2:: aes(x = x, y = y, label = label),
            label.padding = grid::unit(0, "lines"),
            border.colour = "white", size = 3
        )
        p
    })

}
