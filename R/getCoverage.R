#' @keywords internal
.getCoverage <- function(ctx) {
    cov <- lapply(ctx$data$gal, GenomicAlignments::coverage)
    cov <- lapply(cov, unlist)
    cov <- lapply(names(cov), \(x){
        lens <- S4Vectors::runLength(cov[[x]])
        vals <- as.integer(S4Vectors::runValue(cov[[x]]))
        if (!length(vals)) {
            return(GenomicRanges::GRanges(sample = x, coverage = 0))
        }
        ends <- cumsum(lens)
        starts <- ends - lens + 1L
        gr <- GenomicRanges::GRanges(
            seqnames = unique(Seqinfo::seqnames(ctx$args$region)),
            ranges = IRanges::IRanges(start = starts, end = ends),
            strand = unique(BiocGenerics::strand(ctx$args$region)),
            sample = x,
            coverage = vals
        )
        gr <- gr[vals != 0]
        gr
        ## Convert to single bp ranges for nicer plotting with geom_bar
        ## May cause a performance hit, revert to geom_area if needed
        w <- BiocGenerics::width(gr)
        idx <- rep(seq_along(gr), times = w)
        pos <- BiocGenerics::start(gr)[idx] + sequence(w) - 1L
        out <- GenomicRanges::GRanges(
            seqnames = Seqinfo::seqnames(gr)[idx],
            ranges = IRanges::IRanges(start = pos, width = 1L),
            strand = BiocGenerics::strand(gr)[idx],
            sample = gr$sample[idx],
            coverage = gr$coverage[idx],
            seqinfo = Seqinfo::seqinfo(gr)
        )
        out <- out[out$coverage >= ctx$args$min_coverage[x]]
        out
    })
    cov <- do.call(c, cov)
    cov <- IRanges::subsetByOverlaps(cov, ctx$args$region, type = "within")
    ctx$data$cov <- cov
    ctx
}
