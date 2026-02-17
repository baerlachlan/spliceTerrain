.spanOfRanges <- function(gr) {

    if (inherits(gr, "GRangesList")) {
        gr <- unlist(gr, use.names = FALSE)
    }
    if (!inherits(gr, "GRanges")) {
        stop("`region` must be a GRanges or GRangesList.")
    }
    if (length(gr) == 0L) stop("`region` has length 0.")

    s <- min(S4Vectors::start(gr))
    e <- max(S4Vectors::end(gr))

    GenomicRanges::GRanges(
        seqnames = GenomicRanges::seqnames(gr)[1],
        ranges   = IRanges::IRanges(start = s, end = e),
        strand   = GenomicRanges::strand(gr)[1]
    )

}
