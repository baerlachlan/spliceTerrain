#' @keywords internal
.loadAlignments <- function(ctx) {
    gal <- lapply(names(ctx$input$bam), \(x){
        bam <- ctx$input$bam[x]
        paired <- .bamIsPaired(bam)
        flag <- Rsamtools::scanBamFlag(
            isPaired = if (paired) TRUE else NA, isUnmappedQuery = FALSE,
            isSecondaryAlignment = FALSE, isSupplementaryAlignment = FALSE
        )
        ## `which` ignores strand; filter interpreted alignments below.
        param <- Rsamtools::ScanBamParam(
            flag = flag, which = ctx$input$region,
            mapqFilter = ctx$input$min_mapq
        )
        strandedness <- switch(
            ctx$input$strandedness[x],
            unstranded = 0, forward = 1, reverse = 2
        )
        if (paired) {
            aln <- GenomicAlignments::readGAlignmentsList(
                bam, param = param, strandMode = strandedness
            )
        } else {
            aln <- GenomicAlignments::readGAlignments(bam, param = param)
            if (identical(ctx$input$strandedness[[x]], "reverse"))
                BiocGenerics::strand(aln) <- .invertStrand(
                    BiocGenerics::strand(aln)
                )
        }
        ## Only filter for strand if library is stranded
        if (strandedness) {
            keep <- IRanges::overlapsAny(
                GenomicAlignments::grglist(aln), ctx$input$region
            )
            aln <- aln[keep]
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
