#' @keywords internal
.getJunctions <- function(ctx) {
    strand <- as.character(unique(BiocGenerics::strand(ctx$input$region)))
    juncs <- lapply(names(ctx$input$gal), \(x){
        aln <- ctx$input$gal[[x]]
        if (strand != "*" && ctx$input$strandedness[x] != "unstranded")
            aln <- aln[BiocGenerics::strand(aln) == strand]
        if (inherits(aln, "GAlignmentsList"))
            aln <- aln[lengths(aln) > 0L]
        out <- GenomicAlignments::summarizeJunctions(aln, with.revmap = TRUE)
        if (!length(out)) {
            return(GenomicRanges::GRanges(
                sample = x, coverage_raw = 0, coverage = 0
            ))
        }
        ## revmap contains distinct supporting alignment-group indices.
        cov <- lengths(out$revmap)
        S4Vectors::mcols(out) <- S4Vectors::DataFrame(
            sample = x,
            coverage_raw = cov,
            coverage = .normaliseCounts(cov, x, ctx)
        )
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
