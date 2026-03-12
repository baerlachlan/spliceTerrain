#' @keywords internal
.loadAlignments <- function(ctx) {
    strandedness <- switch(
        ctx$args$strandedness,
        unstranded = 0, forward = 1, reverse = 2
    )
    flag <- Rsamtools::scanBamFlag(
        isSecondaryAlignment = FALSE, isSupplementaryAlignment = FALSE
    )
    ## `which` doesn't consider strand, so we need to filter for this later
    param <- Rsamtools::ScanBamParam(
        flag = flag, which = ctx$input$region, mapqFilter = ctx$args$min_mapq
    )
    gal <- lapply(ctx$input$bam, \(x){
        if (.bamIsPaired(x)) {
            GenomicAlignments::readGAlignmentPairs(
                x, param = param, strandMode = strandedness
            )
        } else {
            GenomicAlignments::readGAlignments(x, param = param)
        }
    })
    gal <- lapply(gal, \(aln){
        ## Only filter for strand if library is stranded
        if (strandedness) {
            aln <- IRanges::subsetByOverlaps(aln, ctx$input$region)
        }
        Seqinfo::seqlevels(aln) <- Seqinfo::seqlevelsInUse(aln)
        aln
    })
    ctx$data$gal <- gal
    ctx
}

#' @keywords internal
.bamIsPaired <- function(bam) {
    bf <- Rsamtools::BamFile(bam, yieldSize = 1000L)
    flag <- Rsamtools::scanBamFlag(isPaired = TRUE)
    param <- Rsamtools::ScanBamParam(flag = flag, what = "flag")
    any(Rsamtools::scanBam(bf, param = param)[[1]]$flag)
}
