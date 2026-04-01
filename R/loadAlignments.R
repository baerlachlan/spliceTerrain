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
.bamIsPaired <- function(bam) {
    bf <- Rsamtools::BamFile(bam, yieldSize = 1000L)
    flag <- Rsamtools::scanBamFlag(isPaired = TRUE)
    param <- Rsamtools::ScanBamParam(flag = flag, what = "flag")
    any(Rsamtools::scanBam(bf, param = param)[[1]]$flag)
}
