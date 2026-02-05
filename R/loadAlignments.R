#' @keywords internal
.loadAlignments <- function(
        bam, region, strandedness, min_mapq
) {

    strandedness <- switch(
        strandedness,
        unstranded = 0, forward = 1, reverse = 2
    )
    flag <- Rsamtools::scanBamFlag() # Placeholder for future development
    param <- Rsamtools::ScanBamParam(flag = flag, which = region)
# browser()
    if (.bamIsPaired(bam[1])) {
        gal <- lapply(bam, \(x){
            x <- GenomicAlignments::readGAlignmentPairs(
                x, param = param, strandMode = strandedness
            )
            Seqinfo::seqlevels(x) <- Seqinfo::seqlevelsInUse(x)
            x
        })
    } else {
        gal <- lapply(bam, \(x){
            x <- GenomicAlignments::readGAlignments(
                x, param = param
            )
            Seqinfo::seqlevels(x) <- Seqinfo::seqlevelsInUse(x)
            x
        })
    }

}

#' @keywords internal
.bamIsPaired <- function(bam) {
    bf <- Rsamtools::BamFile(bam, yieldSize = 1000L)
    flag <- Rsamtools::scanBamFlag(isPaired = TRUE)
    param <- Rsamtools::ScanBamParam(flag = flag, what = "flag")
    any(Rsamtools::scanBam(bf, param = param)[[1]]$flag)
}
