#' @keywords internal
.anchorsToRanges <- function(anchors, gr) {
    anc <- S4Vectors::mcols(anchors)$anchor
    start <- anchors[anc == "start"]
    end <- anchors[anc == "end"]
    x <- GenomicRanges::GRanges(
        seqnames = Seqinfo::seqnames(start),
        ranges = IRanges::IRanges(
            BiocGenerics::start(start), BiocGenerics::start(end)
        ),
        strand = BiocGenerics::strand(start)
    )
    S4Vectors::mcols(x) <- S4Vectors::mcols(gr)
    x
}
