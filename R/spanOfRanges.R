.spanOfRanges <- function(gr) {

    s <- min(S4Vectors::start(gr))
    e <- max(S4Vectors::end(gr))

    GenomicRanges::GRanges(
        seqnames = GenomicRanges::seqnames(gr)[1],
        ranges   = IRanges::IRanges(start = s, end = e),
        strand   = GenomicRanges::strand(gr)[1]
    )

}
