#' @keywords internal
.resolveGr <- function(ctx, type) {
    gr <- ctx$args[[type]]
    region <- ctx$args$region
    if (is.null(gr)) return(ctx)
    if (is.character(gr)) gr <- GenomicRanges::GRanges(gr)
    if (inherits(gr, "GRangesList"))
        gr <- unlist(gr, use.names = FALSE)
    if (!length(GenomicRanges::intersect(
        Seqinfo::seqlevels(gr), Seqinfo::seqlevels(region)
    ))) stop("`", type, "` does not overlap `region`")
    hits <- GenomicRanges::findOverlaps(gr, region)
    gr <- gr[S4Vectors::from(hits)]
    gr <- BiocGenerics::sort(gr)
    ctx$args[[type]] <- gr
    ctx
}

#' @keywords internal
.spanOfRanges <- function(gr) {
    s <- min(S4Vectors::start(gr))
    e <- max(S4Vectors::end(gr))
    GenomicRanges::GRanges(
        seqnames = GenomicRanges::seqnames(gr)[1],
        ranges   = IRanges::IRanges(start = s, end = e),
        strand   = GenomicRanges::strand(gr)[1]
    )
}
