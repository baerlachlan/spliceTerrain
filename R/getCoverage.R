#' @keywords internal
.getCoverage <- function(ctx) {
    cov <- lapply(ctx$input$gal, GenomicAlignments::coverage)
    cov <- lapply(cov, unlist)
    cov <- lapply(names(cov), \(x){
        lens <- S4Vectors::runLength(cov[[x]])
        vals <- as.integer(S4Vectors::runValue(cov[[x]]))
        if (!length(vals)) {
            return(GenomicRanges::GRanges(
                sample = x, coverage_raw = 0, coverage = 0
            ))
        }
        ends <- cumsum(lens)
        starts <- ends - lens + 1
        gr <- GenomicRanges::GRanges(
            seqnames = unique(Seqinfo::seqnames(ctx$input$region)),
            ranges = IRanges::IRanges(start = starts, end = ends),
            strand = unique(BiocGenerics::strand(ctx$input$region)),
            sample = x,
            coverage_raw = vals,
            coverage = .normaliseCounts(vals, x, ctx)
        )
        gr <- gr[vals != 0]
        gr <- IRanges::restrict(
            gr,
            start = BiocGenerics::start(ctx$input$region),
            end = BiocGenerics::end(ctx$input$region),
            keep.all.ranges = FALSE
        )
        gr <- gr[BiocGenerics::width(gr) > 0]
        threshold <- .resolveMinThreshold(
            ctx$input$min_coverage[x], gr$coverage_raw
        )
        gr[gr$coverage_raw >= threshold]
    })
    cov <- do.call(c, cov)
    cov <- IRanges::subsetByOverlaps(cov, ctx$input$region, type = "within")
    ctx$input$cov <- cov
    ctx$plot$cov <- cov
    ctx
}
