#' @keywords internal
.loadAlignments <- function(
        bam, region, strandedness, min_mapq
) {

    strandedness <- switch(
        strandedness,
        unstranded = 0, forward = 1, reverse = 2
    )
    flag <- Rsamtools::scanBamFlag() # Placeholder for future development
    ## `which` doesn't consider strand, so we need to filter for this later
    param <- Rsamtools::ScanBamParam(
        flag = flag, which = region, mapqFilter = min_mapq
    )
    if (.bamIsPaired(bam[1])) {
        gal <- lapply(bam, \(x){
            aln <- GenomicAlignments::readGAlignmentPairs(
                x, param = param, strandMode = strandedness
            )
        })
    } else {
        gal <- lapply(bam, \(x){
            aln <- GenomicAlignments::readGAlignments(
                x, param = param
            )
        })
    }
    gal <- lapply(gal, \(aln){
        aln <- IRanges::subsetByOverlaps(aln, region) # strand filter
        Seqinfo::seqlevels(aln) <- Seqinfo::seqlevelsInUse(aln)
        aln
    })
    gal

}

#' @keywords internal
.bamIsPaired <- function(bam) {
    bf <- Rsamtools::BamFile(bam, yieldSize = 1000L)
    flag <- Rsamtools::scanBamFlag(isPaired = TRUE)
    param <- Rsamtools::ScanBamParam(flag = flag, what = "flag")
    any(Rsamtools::scanBam(bf, param = param)[[1]]$flag)
}
