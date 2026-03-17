#' @keywords internal
.mapGenomeToPlot <- function(gr, map) {
    ## Always returns a list as gr may be NULL (see comment in .applyMap())
    if (is.null(gr)) return(list(gr))
    anchors <- .rangesToAnchors(gr)
    hits <- IRanges::findOverlaps(anchors, map)
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    pos <- BiocGenerics::start(anchors)
    mapped <- map$p_start[sh] + (pos[qh] - map$g_start[sh])
    out <- anchors
    IRanges::ranges(out) <- IRanges::IRanges(start = mapped, end = mapped)
    list(.anchorsToRanges(out, gr))
}
