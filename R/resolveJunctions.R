#' @keywords internal
.resolveJunctions <- function(
        gal, region, min_junction_reads
) {
# browser()
    juncs <- lapply(gal, GenomicAlignments::summarizeJunctions)
    strand <- as.character(unique(BiocGenerics::strand(region)))
    juncs <- lapply(names(juncs), \(x){
        if (strand == "+") {
            S4Vectors::mcols(juncs[[x]]) <- S4Vectors::DataFrame(
                sample = x,
                coverage = juncs[[x]]$plus_score
            )
        } else if (strand == "-") {
            S4Vectors::mcols(juncs[[x]]) <- S4Vectors::DataFrame(
                sample = x,
                coverage = juncs[[x]]$minus_score
            )
        } else {
            S4Vectors::mcols(juncs[[x]]) <- S4Vectors::DataFrame(
                sample = x,
                coverage = juncs[[x]]$score
            )
        }
        juncs[[x]]
    })
    juncs <- do.call(c, juncs)
    # hits <- IRanges::findOverlaps(juncs, region)
    # juncs <- GenomicRanges::pintersect(
    #     juncs[S4Vectors::from(hits)], region[S4Vectors::to(hits)],
    #     drop.nohit.ranges = TRUE
    #     )
    # ## If region ends at a splice junction the arc will still show, so remove
    # juncs <- juncs[BiocGenerics::width(juncs) > 0]
    juncs[juncs$coverage > min_junction_reads]

}
