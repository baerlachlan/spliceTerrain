#' @keywords internal
.resolveRegion <- function(gr) {

    if (is.character(gr)) {
        r <- strsplit(gr, ":")
        stopifnot(length(r[[1]]) %in% c(2, 3)) ## TODO add message
        seqnames <- vapply(r, \(x){x[1]}, character(1))
        start <- vapply(r, \(x){strsplit(x[2], "-")[[1]][1]}, character(1))
        end <- vapply(r, \(x){strsplit(x[2], "-")[[1]][2]}, character(1))
        strand <- vapply(r, \(x){
            if (length(x) == 3) x[3] else "*"
        }, character(1))
        GenomicRanges::GRanges(
            seqnames = seqnames,
            ranges = IRanges::IRanges(
                start = as.numeric(start), end = as.numeric(end)
            ),
            strand = strand
        )
    } else {
        ## Ensure only a single range (the span) is returned
        ## So we don't load duplicate alignments
        ## See `which` arg of scanBamParam
        range(gr)
    }

}
