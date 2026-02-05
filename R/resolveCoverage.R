#' @keywords internal
.resolveCoverage <- function(
        gal, region, min_coverage
) {

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
    # browser()
    # hits <- IRanges::findOverlaps(cov, region)
    # cov <- GenomicRanges::pintersect(
    #     cov[S4Vectors::from(hits)], region[S4Vectors::to(hits)],
    #     drop.nohit.ranges = TRUE
    # )
    # cov <- cov[BiocGenerics::width(cov) > 0]
    cov[cov$coverage > min_coverage]

}
