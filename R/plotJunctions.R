#' @importFrom rlang .data
#' @keywords internal
.plotJunctions <- function(plot_list, junctions, coverage) {

    junctions <- split(junctions, junctions$sample)
    coverage <- split(coverage, coverage$sample)

    out_plots <- lapply(names(plot_list), function(sample_id) {

        junc <- junctions[[sample_id]]
        cov  <- coverage[[sample_id]]

        ## Arc stacking
        levels <- IRanges::disjointBins(junc)
        ## Alternating above/below pattern for arcs
        sign <- rep(c(1, -1), length.out = length(junc))

        max_cov <- max(cov$coverage)
        y_step <- 0.15 * max_cov
        heights <- (y_step + (ceiling(levels / 2) - 1L) * y_step) * sign

        ## Junction midpoints and half-widths
        start_j <- BiocGenerics::start(junc)
        end_j <- BiocGenerics::end(junc)
        mid <- (start_j + end_j) / 2
        hw <- (end_j - start_j) / 2

        ## Coverage at junction anchors
        anc <- .rangesToAnchors(junc)
        left <- GenomicRanges::shift(anc[anc$anchor == "start"], -1L)
        right <- GenomicRanges::shift(anc[anc$anchor == "end"], 1L)
        cov_l <- .coverageAtPos(cov, left)
        cov_r <- .coverageAtPos(cov, right)
        cov_lr <- pmax(cov_l, cov_r)

        ## “above” arcs start at anchor coverage and peak at coverage + height
        above <- sign == 1L
        heights[above] <- heights[above] + cov_lr[above]

        ## Amount the arc must rise above left/right anchors
        diff_l <- heights - cov_l
        diff_r <- heights - cov_r

        ## "below" arcs start at 0 and peak at height
        diff_l[!above] <- heights[!above]
        diff_r[!above] <- heights[!above]

        ## Build arc points
        t <- seq(0, 1, length.out = 50L)
        tt <- c(-rev(t), t) # Symmetric for both halves combined
        bump_l <- (1 - rev(t)^8) # left half
        bump_r <- (1 - t^8) # right half

        ## Matrix of dims number of juncs x 100
        x_mat <- mid + hw * rep(tt, each = length(mid))
        dim(x_mat) <- c(length(mid), length(tt))

        ## Build left and right halves
        y_l <- diff_l * rep(bump_l, each = length(diff_l))
        dim(y_l) <- c(length(diff_l), length(bump_l))
        y_r <- diff_r * rep(bump_r, each = length(diff_r))
        dim(y_r) <- c(length(diff_r), length(bump_r))
        ## Add anchor baselines for above arcs
        if (any(above)) {
            y_l[above,] <- y_l[above,] + cov_l[above]
            y_r[above,] <- y_r[above,] + cov_r[above]
        }
        ## Bind halves
        y_mat <- cbind(y_l, y_r)

        arcs <- data.frame(
            x = c(t(x_mat)),
            y = c(t(y_mat)),
            id = rep(seq_len(length(mid)), each = ncol(x_mat))
        )

        labels <- data.frame(
            x = mid,
            y = heights,
            label = as.character(junc$coverage)
        )

        plot_list[[sample_id]] +
            ggplot2::geom_line(
                data = arcs,
                ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
                colour = "black", lineend = "round"
            ) +
            ggplot2::geom_label(
                data = labels,
                ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                label.padding = grid::unit(0, "lines"),
                border.colour = "white",
                size = 3
            )
    })

    names(out_plots) <- names(plot_list)
    out_plots
}

#' @keywords internal
.coverageAtPos <- function(coverage, pos) {
    hits <- IRanges::findOverlaps(pos, coverage)
    out <- integer(length(pos))
    if (length(hits)) {
        qh <- S4Vectors::queryHits(hits)
        sh <- S4Vectors::subjectHits(hits)
        out[qh] <- coverage$coverage[sh]
    }
    out
}
