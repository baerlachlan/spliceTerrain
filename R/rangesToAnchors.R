#' @keywords internal
.rangesToAnchors <- function(gr) {
    if (is.null(gr)) return(gr)
    start <- GenomicRanges::GRanges(
        seqnames = Seqinfo::seqnames(gr),
        ranges = IRanges::IRanges(BiocGenerics::start(gr), width = 1),
        strand = BiocGenerics::strand(gr),
        anchor = "start"
    )
    end <- GenomicRanges::GRanges(
        seqnames = Seqinfo::seqnames(gr),
        ranges = IRanges::IRanges(BiocGenerics::end(gr), width = 1),
        strand = BiocGenerics::strand(gr),
        anchor = "end"
    )
    c(start, end)
}
