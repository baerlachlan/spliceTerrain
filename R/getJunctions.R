#' @keywords internal
.getJunctions <- function(ctx) {
    juncs <- lapply(ctx$data$gal, GenomicAlignments::summarizeJunctions)
    strand <- as.character(unique(BiocGenerics::strand(ctx$args$region)))
    juncs <- lapply(names(juncs), \(x){
        if (!length(juncs[[x]])) {
            return(GenomicRanges::GRanges(sample = x, coverage = 0))
        }
        if (strand == "+" & ctx$args$strandedness[x] != "unstranded") {
            cov <- juncs[[x]]$plus_score
        } else if (strand == "-" & ctx$args$strandedness[x] != "unstranded") {
            cov <- juncs[[x]]$minus_score
        } else {
            cov <- juncs[[x]]$score
        }
        S4Vectors::mcols(juncs[[x]]) <- S4Vectors::DataFrame(
            sample = x, coverage = cov
        )
        out <- juncs[[x]]
        out <- out[out$coverage >= ctx$args$min_junction_reads[x]]
    })
    juncs <- do.call(c, juncs)
    juncs <- IRanges::subsetByOverlaps(juncs, ctx$args$region, type = "within")
    ctx$data$juncs <- juncs
    ctx
}
