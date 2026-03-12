#' @keywords internal
.resolveJunctions <- function(ctx) {
    juncs <- lapply(ctx$data$gal, GenomicAlignments::summarizeJunctions)
    strand <- as.character(unique(BiocGenerics::strand(ctx$input$region)))
    juncs <- lapply(names(juncs), \(x){
        if (!length(juncs[[x]])) {
            return(GenomicRanges::GRanges(sample = x, coverage = 0))
        }
        if (strand == "+" & ctx$args$strandedness != "unstranded") {
            cov <- juncs[[x]]$plus_score
        } else if (strand == "-" & ctx$args$strandedness != "unstranded") {
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
    juncs <- IRanges::subsetByOverlaps(juncs, ctx$input$region, type = "within")
    juncs <- juncs[juncs$coverage >= ctx$args$min_junction_reads]
    ctx$data$juncs <- juncs
    ctx
}
