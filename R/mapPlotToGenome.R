#' @keywords internal
.mapPlotToGenome <- function(pos, map) {
    ## Preallocate output to keep input order
    out <- rep(NA_real_, length(pos))
    ## Positions that fall within mapped regions
    hits <- IRanges::findOverlaps(
        IRanges::IRanges(pos, pos),
        IRanges::IRanges(map$p_start, map$p_end)
    )
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    if (length(qh)) out[qh] <- map$g_start[sh] + (pos[qh] - map$p_start[sh])
    ## For positions that fall in gaps, approximate by proportional distance
    nohit <- setdiff(seq_along(pos), qh)
    px <- pos[nohit]
    ## Find the last block whose p_end is <= px
    left <- findInterval(px, map$p_end)
    right <- left + 1L
    ## Plot-level gap between blocks
    gap_p_start <- map$p_end[left]
    gap_p_end <- map$p_start[right]
    gap_p_w <- gap_p_end - gap_p_start
    ## Fraction through the plot gap
    frac <- (px - gap_p_start) / gap_p_w
    ## Genomic-level gap between blocks
    gap_g_start <- map$g_end[left]
    gap_g_end <- map$g_start[right]
    gap_g_w <- gap_g_end - gap_g_start
    out[nohit] <- gap_g_start + frac * gap_g_w
    out
}
