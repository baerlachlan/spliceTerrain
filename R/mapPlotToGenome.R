#' @keywords internal
.mapPlotToGenome <- function(pos, map) {
    # browser()
    hits <- IRanges::findOverlaps(
        IRanges::IRanges(pos, pos),
        IRanges::IRanges(map$p_start, map$p_end)
    )
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    mapped <- map$g_start[sh] + (pos[qh] - map$p_start[sh]) * map$scale[sh]
    mapped

    nohit_idx <- setdiff(seq_along(pos), qh)

    ## Preallocate output (keeps input order)
    out <- rep(NA_real_, length(pos))

    ## Positions that fall within mapped regions
    hits <- IRanges::findOverlaps(
        IRanges::IRanges(pos, pos),
        IRanges::IRanges(map$p_start, map$p_end),
        type = "within"
    )
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    if (length(qh)) {
        out[qh] <- map$g_start[sh] + (pos[qh] - map$p_start[sh]) / map$scale[sh]
    }

    ## Positions that fall in gaps
    ## Approximate by proportional distance
    nohit <- setdiff(seq_along(pos), qh)
    px <- pos[nohit]
    ## Find the last block whose p_end is <= px
    ## i == 0 -> px is left of first block
    ## i == m -> px is right of last block
    m <- length(map)
    i <- findInterval(px, map$p_end)
    ## A gap is between block i and i+1, so we need 1 <= i <= m-1
    in_gap <- which(i >= 1L & i <= (m - 1L))
    if (!length(in_gap)) return(out)

    left <- i[in_gap]
    right <- left + 1L

    gap_p_start <- map$p_end[left]
    gap_p_end <- map$p_start[right]
    gap_p_w <- gap_p_end - gap_p_start

    ok_gap <- gap_p_w > 0
    if (!any(ok_gap)) return(out)

    left <- left[ok_gap]
    right <- right[ok_gap]
    px_ok <- px[in_gap][ok_gap]

    ## Fraction through the plot gap
    frac <- (px_ok - gap_p_start[ok_gap]) / gap_p_w[ok_gap]

    # Genomic gap between blocks (note: inclusive/exclusive convention doesn’t matter for “approx”)
    gap_g_start <- map$g_end[left]
    gap_g_end <- map$g_start[right]
    gap_g_w <- gap_g_end - gap_g_start

    out[nohit[in_gap][ok_gap]] <- gap_g_start + frac * gap_g_w

    out

}
