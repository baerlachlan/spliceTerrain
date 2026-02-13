#' @keywords internal
.mapGenomeToPlot <- function(gr, map) {

    anchors <- .rangesToAnchors(gr)
    hits <- IRanges::findOverlaps(anchors, map)
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    pos <- BiocGenerics::start(anchors)
    mapped <- map$p_start[sh] + (pos[qh] - map$g_start[sh])
    out <- anchors
    IRanges::ranges(out) <- IRanges::IRanges(start = mapped, end = mapped)
    .anchorsToRanges(out, gr)

}
