#' @keywords internal
.junctionArcLayout <- function(junc, cov, arc_height) {
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
    left <- GenomicRanges::shift(anc[anc$anchor == "start"], -1L)
    right <- GenomicRanges::shift(anc[anc$anchor == "end"], 1L)
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
    max_cov <- max(cov$coverage)
    y_step <- arc_height * max_cov
    heights <- (y_step + levels * y_step) * sign
    ## “above” arcs start at anchor coverage and peak at coverage + height
    above <- sign == 1L
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
    hits <- IRanges::findOverlaps(pos, coverage)
    out <- numeric(length(pos))
    if (length(hits)) {
        qh <- S4Vectors::queryHits(hits)
        sh <- S4Vectors::subjectHits(hits)
        out[qh] <- coverage$coverage[sh]
    }
    out
}
