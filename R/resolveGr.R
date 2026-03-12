#' @keywords internal
.resolveGr <- function(gr, region, type) {

    if (is.null(gr)) return(gr)
    if (is.character(gr)) gr <- GenomicRanges::GRanges(gr)
    if (inherits(gr, "GRangesList"))
        gr <- unlist(gr, use.names = FALSE)
    if (!length(GenomicRanges::intersect(
        Seqinfo::seqlevels(gr), Seqinfo::seqlevels(region)
    ))) stop("`", type, "` does not overlap `region`")

    hits <- GenomicRanges::findOverlaps(gr, region)
    gr <- gr[S4Vectors::from(hits)]
    gr <- BiocGenerics::sort(gr)
    gr

}
