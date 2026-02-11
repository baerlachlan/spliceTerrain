#' @keywords internal
.resolveJunctions <- function(
        gal, region, min_junction_reads, strandedness
) {

    juncs <- lapply(gal, GenomicAlignments::summarizeJunctions)
    strand <- as.character(unique(BiocGenerics::strand(region)))
    juncs <- lapply(names(juncs), \(x){
        if (strand == "+" & strandedness != "unstranded") {
            cov <- juncs[[x]]$plus_score
        } else if (strand == "-" & strandedness != "unstranded") {
            cov <- juncs[[x]]$minus_score
        } else {
            cov <- juncs[[x]]$score
        }
        S4Vectors::mcols(juncs[[x]]) <- S4Vectors::DataFrame(
            sample = x, coverage = cov
        )
        juncs[[x]]
    })
    juncs <- do.call(c, juncs)
    juncs[juncs$coverage > min_junction_reads]

}
