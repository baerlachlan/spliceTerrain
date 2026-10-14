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
        out
    })
    juncs <- do.call(c, juncs)
    seqname <- Seqinfo::seqnames(ctx$input$region)
    juncs <- juncs[Seqinfo::seqnames(juncs) %in% seqname]
    ctx$input$juncs_all <- juncs
    juncs <- IRanges::subsetByOverlaps(juncs, ctx$input$region, type = "within")
    thresholds <- vapply(names(ctx$input$gal), function(x) {
        .resolveMinThreshold(
            ctx$input$min_junction_reads[x],
            juncs$coverage_raw[juncs$sample == x]
        )
    }, numeric(1))
    juncs <- juncs[juncs$coverage_raw >= thresholds[juncs$sample]]
    juncs <- .matchAnnotatedJunctions(juncs, ctx$input$annotation)
    juncs <- .addJunctionUsage(
        juncs, ctx$input$juncs_all, ctx$input$junction_usage
    )
    ctx$input$juncs <- juncs
    ctx$plot$juncs <- juncs
    ctx
}

#' @keywords internal
.addJunctionUsage <- function(juncs, reference, region) {
    juncs$usage_fraction <- rep(NA_real_, length(juncs))
    if (is.null(region) || !length(juncs) || !length(reference)) return(juncs)

    anchors <- .rangesToAnchors(reference)
    hits <- IRanges::overlapsAny(anchors, region)
    n <- length(reference)
    selected <- which(hits[seq_len(n)] | hits[n + seq_len(n)])
    if (!length(selected)) return(juncs)
    samples <- as.character(reference$sample)
    totals <- tapply(reference$coverage_raw[selected], samples[selected], sum)
    denominator <- unname(totals[samples[selected]])
    positive <- denominator > 0
    fractions <- rep(NA_real_, n)
    chosen <- selected[positive]
    fractions[chosen] <- reference$coverage_raw[chosen] / denominator[positive]

    hits <- GenomicRanges::findOverlaps(
        juncs, reference, type = "equal", ignore.strand = TRUE
    )
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    same_sample <- as.character(juncs$sample[qh]) == samples[sh]
    juncs$usage_fraction[qh[same_sample]] <- fractions[sh[same_sample]]
    juncs
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
