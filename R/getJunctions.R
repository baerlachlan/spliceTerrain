#' @keywords internal
.getJunctions <- function(ctx) {
    juncs <- lapply(ctx$input$gal, GenomicAlignments::summarizeJunctions)
    strand <- as.character(unique(BiocGenerics::strand(ctx$input$region)))
    juncs <- lapply(names(juncs), \(x){
        if (!length(juncs[[x]])) {
            return(GenomicRanges::GRanges(
                sample = x, coverage_raw = 0, coverage = 0
            ))
        }
        if (strand == "+" & ctx$input$strandedness[x] != "unstranded") {
            cov <- juncs[[x]]$plus_score
        } else if (strand == "-" & ctx$input$strandedness[x] != "unstranded") {
            cov <- juncs[[x]]$minus_score
        } else {
            cov <- juncs[[x]]$score
        }
        S4Vectors::mcols(juncs[[x]]) <- S4Vectors::DataFrame(
            sample = x,
            coverage_raw = cov,
            coverage = .normaliseCounts(cov, x, ctx)
        )
        out <- juncs[[x]]
        out <- IRanges::subsetByOverlaps(
            out, ctx$input$region, type = "within"
        )
        threshold <- .resolveMinThreshold(
            ctx$input$min_junction_reads[x], out$coverage_raw
        )
        out <- out[out$coverage_raw >= threshold]
    })
    juncs <- do.call(c, juncs)
    juncs <- IRanges::subsetByOverlaps(juncs, ctx$input$region, type = "within")
    juncs <- .matchAnnotatedJunctions(juncs, ctx$input$annotation)
    ctx$input$juncs <- juncs
    ctx$plot$juncs <- juncs
    ctx
}

#' @keywords internal
.matchAnnotatedJunctions <- function(juncs, annotation) {
    annotation_match <- rep(FALSE, length(juncs))
    introns <- .annotationJunctions(annotation)
    if (length(juncs) && length(introns)) {
        hits <- GenomicRanges::findOverlaps(
            juncs, introns, type = "equal", ignore.strand = TRUE
        )
        annotation_match[S4Vectors::queryHits(hits)] <- TRUE
    }
    juncs$annotation_match <- annotation_match
    juncs
}

#' @keywords internal
.annotationJunctions <- function(annotation) {
    if (is.null(annotation) || !length(annotation))
        return(GenomicRanges::GRanges())

    annotation <- split(annotation, annotation$group)
    introns <- lapply(annotation, function(exons) {
        exons <- GenomicRanges::reduce(exons, ignore.strand = TRUE)
        n <- length(exons)
        if (n < 2) return(exons[FALSE])

        start <- BiocGenerics::end(exons)[-n] + 1L
        end <- BiocGenerics::start(exons)[-1] - 1L
        keep <- start <= end
        GenomicRanges::GRanges(
            seqnames = Seqinfo::seqnames(exons)[1],
            ranges = IRanges::IRanges(start[keep], end[keep])
        )
    })
    unique(do.call(c, unname(introns)))
}
