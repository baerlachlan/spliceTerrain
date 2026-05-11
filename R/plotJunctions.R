#' @keywords internal
.plotJunctions <- function(
        p, juncs, cov, psi, arc_height, colour, junc_text_size, scale_arcs,
        max_cov
) {
    if (is.null(juncs)) return(p)
    layout <- .junctionArcLayout(juncs, cov, arc_height, max_cov)
    arcs <- .junctionArcPoints(layout)
    labels <- .junctionArcLabels(layout, juncs, psi)
    size_col <- ifelse(scale_arcs, "size_on", "size_off")
    p + ggplot2::geom_line(
        data = arcs,
        ggplot2::aes(
            x = .data$x, y = .data$y, group = .data$id,
            linewidth = .data[[size_col]]
        ),
        colour = colour, lineend = "round", show.legend = FALSE
    ) +
        ggplot2::scale_linewidth(range = c(0.2, 0.8)) +
        ggplot2::geom_label(
            data = labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            label.padding = grid::unit(0, "pt"),
            label.r = grid::unit(1.5, "pt"), linewidth = 0,
            size = junc_text_size, text.colour = colour, fill = "white",
            lineheight = 0.9
        )
}

#' @keywords internal
.junctionArcLayout <- function(junc, cov, arc_height, max_cov) {
    ## Arc stacking
    levels <- IRanges::disjointBins(sort(junc))
    ## Alternating above/below pattern for arcs
    sign <- rep(c(1, -1), length.out = length(junc))
    ## Junction midpoints and half-widths
    start_j <- BiocGenerics::start(junc)
    end_j <- BiocGenerics::end(junc)
    mid <- (start_j + end_j) / 2
    hw <- (end_j - start_j) / 2
    ## Coverage at junction anchors
    anc <- .rangesToAnchors(junc)
    left <- GenomicRanges::shift(anc[anc$anchor == "start"], -1)
    right <- GenomicRanges::shift(anc[anc$anchor == "end"], 1)
    cov_l <- .coverageAtPos(cov, left)
    cov_r <- .coverageAtPos(cov, right)
    cov_lr <- pmax(cov_l, cov_r)
    ## Overlap clusters and per-cluster max anchor coverage
    overlap <- GenomicRanges::reduce(junc, with.revmap = TRUE)
    revmap <- S4Vectors::mcols(overlap)$revmap
    cluster_id <- rep(seq_along(revmap), lengths(revmap))
    cluster_max_cov <- vapply(revmap, function(i) {
        max(cov_lr[i], na.rm = TRUE)
    }, numeric(1))
    cluster_max_cov_per_junc <- cluster_max_cov[cluster_id]
    ## Determine incremental heights from max coverage
    ## 1 to allow arcs if coverage is filtered
    if (is.null(max_cov)) {
        max_cov <- if (is.null(cov$coverage)) 1 else max(cov$coverage)
    }
    y_step <- arc_height * max_cov
    heights <- (y_step + levels * y_step) * sign
    ## “above” arcs start at anchor coverage and peak at coverage + height
    above <- sign == 1
    heights[above] <- heights[above] + cluster_max_cov_per_junc[above]
    ## Amount the arc must rise above left/right anchors
    diff_l <- heights - cov_l
    diff_r <- heights - cov_r
    ## "below" arcs start at 0 and peak at height
    diff_l[!above] <- heights[!above]
    diff_r[!above] <- heights[!above]
    list(
        mid = mid, hw = hw, heights = heights, above = above,
        diff_l = diff_l, diff_r = diff_r,
        cov_l = cov_l, cov_r = cov_r, cov_j = junc$coverage
    )
}

#' @keywords internal
.coverageAtPos <- function(coverage, pos) {
    out <- numeric(length(pos))
    if (is.null(coverage)) return(out)  ## cov may be filtered so avoid error
    hits <- IRanges::findOverlaps(pos, coverage)
    if (length(hits)) {
        qh <- S4Vectors::queryHits(hits)
        sh <- S4Vectors::subjectHits(hits)
        out[qh] <- coverage$coverage[sh]
    }
    out
}

#' @keywords internal
.junctionArcPoints <- function(layout) {
    mid <- layout$mid
    hw <- layout$hw
    above <- layout$above
    diff_l <- layout$diff_l
    diff_r <- layout$diff_r
    cov_l <- layout$cov_l
    cov_r <- layout$cov_r
    ## Build arc points
    t <- seq(0, 1, length.out = 50)
    tt <- c(-rev(t), t) # Both halves combined
    ## The exponent controls how round or square the arcs become
    bump_l <- 1 - rev(t)^32 # left half
    bump_r <- 1 - t^32 # right half
    ## Matrix of dims: number of juncs x 100
    x_mat <- mid + hw * rep(tt, each = length(mid))
    dim(x_mat) <- c(length(mid), length(tt))
    ## Build left and right halves
    y_l <- diff_l * rep(bump_l, each = length(diff_l))
    dim(y_l) <- c(length(diff_l), length(bump_l))
    y_r <- diff_r * rep(bump_r, each = length(diff_r))
    dim(y_r) <- c(length(diff_r), length(bump_r))
    ## Add anchor baselines for "above" arcs
    if (any(above)) {
        y_l[above,] <- y_l[above,] + cov_l[above]
        y_r[above,] <- y_r[above,] + cov_r[above]
    }
    ## Bind halves
    y_mat <- cbind(y_l, y_r)
    id <- rep(seq_len(length(mid)), each = ncol(x_mat))
    data.frame(
        x = c(t(x_mat)),
        y = c(t(y_mat)),
        id = id,
        size_on = layout$cov_j[id],
        size_off = 1
    )
}

.junctionArcLabels <- function(layout, juncs, psi) {
    labels <- juncs$coverage
    if (!is.null(psi)) {
        anchors <- .rangesToAnchors(juncs)
        st <- anchors[anchors$anchor == "start"]
        hits_st <- IRanges::findOverlaps(psi, st)
        sh_st <- S4Vectors::subjectHits(hits_st)
        en <- anchors[anchors$anchor == "end"]
        hits_en <- IRanges::findOverlaps(psi, en)
        sh_en <- S4Vectors::subjectHits(hits_en)
        sh <- unique(c(sh_st, sh_en))
        labels[sh] <- paste0(
            labels[sh], "\n",
            "(", scales::percent(round(labels[sh] / sum(labels[sh]), 3)), ")"
        )
    }
    data.frame(
        x = layout$mid,
        y = layout$heights,
        label = labels
    )
}
