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
        if (!length(vals)) {
            return(GenomicRanges::GRanges(
                # GenomicRanges::granges(region),
                sample = x, coverage = 0
                ))
            # gr <- region
            # S4Vectors::mcols(gr) <- S4Vectors::DataFrame(
            #     sample = x, coverage = 0
            # )
            # return(gr)
        }
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
    cov <- IRanges::subsetByOverlaps(cov, region, type = "within")
    cov[cov$coverage >= min_coverage]

}
