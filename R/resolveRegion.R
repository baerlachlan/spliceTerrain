#' @keywords internal
.resolveRegion <- function(region) {

    if (is.null(region)) return(region)
    if (is.character(region)) region <- GenomicRanges::GRanges(region)
    if (inherits(region, "GRangesList"))
        region <- unlist(region, use.names = FALSE)
    if (!inherits(region, "GRanges"))
        stop("`region` must be a GRanges or GRangesList.")

    ## Ensure only a single range (the span) is returned
    ## So we don't load duplicate alignments
    ## See `which` arg of scanBamParam
    .spanOfRanges(region)

}
