#' @keywords internal
.loadAlignments <- function(ctx) {
    flag <- Rsamtools::scanBamFlag(
        isSecondaryAlignment = FALSE, isSupplementaryAlignment = FALSE
    )
    ## `which` doesn't consider strand, so we need to filter for this later
    param <- Rsamtools::ScanBamParam(
        flag = flag, which = ctx$input$region, mapqFilter = ctx$input$min_mapq
    )
    gal <- lapply(names(ctx$input$bam), \(x){
        bam <- ctx$input$bam[x]
        strandedness <- switch(
            ctx$input$strandedness[x],
            unstranded = 0, forward = 1, reverse = 2
        )
        if (.bamIsPaired(bam)) {
            aln <- GenomicAlignments::readGAlignmentPairs(
                bam, param = param, strandMode = strandedness
            )
        } else {
            aln <- GenomicAlignments::readGAlignments(bam, param = param)
            if (identical(ctx$input$strandedness[x], "reverse"))
                BiocGenerics::strand(aln) <- .invertStrand(
                    BiocGenerics::strand(aln)
                )
        }
        ## Only filter for strand if library is stranded
        if (strandedness) {
            aln <- IRanges::subsetByOverlaps(aln, ctx$input$region)
        }
        Seqinfo::seqlevels(aln) <- Seqinfo::seqlevelsInUse(aln)
        aln
    })
    names(gal) <- names(ctx$input$bam)
    ctx$input$gal <- gal
    ctx
}

#' @keywords internal
.invertStrand <- function(strand) {
    strand <- as.character(strand)
    strand <- ifelse(strand == "+", "-", ifelse(strand == "-", "+", strand))
    BiocGenerics::strand(strand)
}

#' @keywords internal
.bamIsPaired <- function(bam) {
    bf <- Rsamtools::BamFile(bam, yieldSize = 1)
    param <- Rsamtools::ScanBamParam(
        flag = Rsamtools::scanBamFlag(isUnmappedQuery = FALSE),
        what = "flag"
    )
    flag <- Rsamtools::scanBam(bf, param = param)[[1]]$flag
    length(flag) > 0 && bitwAnd(flag, 1) == 1
}
