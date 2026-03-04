#' @keywords internal
.resolveRegion <- function(region) {

    if (is.null(region)) return(region)
    if (is.character(region)) {
        region <- gsub(",", "", region)
        region <- gsub(" ", "", region)
        region <- gsub("\u2013", "-", region)
        region <- gsub("\u2014", "-", region)
        region <- GenomicRanges::GRanges(region)
    }
    if (inherits(region, "GRangesList"))
        region <- unlist(region, use.names = FALSE)
    if (!inherits(region, "GRanges"))
        stop("`region` must be a GRanges or GRangesList.")

    ## Ensure only a single range (the span) is returned
    ## So we don't load duplicate alignments
    ## See `which` arg of scanBamParam
    ## TODO: add warning if reducing to single range
    .spanOfRanges(region)

}
