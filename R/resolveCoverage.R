#' @keywords internal
.resolveCoverage <- function(
        gal, region, min_coverage
) {
# browser()
    cov <- lapply(gal, GenomicAlignments::coverage)
    cov <- lapply(cov, unlist)
    cov <- lapply(names(cov), \(x){
        lens <- S4Vectors::runLength(cov[[x]])
        vals <- as.integer(S4Vectors::runValue(cov[[x]]))
        ends <- cumsum(lens)
        starts <- ends - lens + 1L
        gr <- GenomicRanges::GRanges(
            seqnames = unique(Seqinfo::seqnames(region)),
            ranges = IRanges::IRanges(start = starts, end = ends),
            strand = unique(BiocGenerics::strand(region)),
            sample = x,
            coverage = vals
        )
        gr <- gr[vals != 0]
        gr
    })
    cov <- do.call(c, cov)
    ## Drop ranges outside region and trim partial overlaps
    cov <- GenomicRanges::pintersect(cov, region, drop.nohit.ranges = TRUE)
    cov[cov$coverage > min_coverage]

}
