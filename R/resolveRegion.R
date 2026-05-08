#' @keywords internal
.resolveRegion <- function(ctx) {
    region <- ctx$input$region
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
    if (length(unique(as.character(Seqinfo::seqnames(region)))) != 1L)
        stop("`region` must resolve to ranges on exactly one seqname.")
    ## Ensure only a single range (the span) is returned
    ## So we don't load duplicate alignments
    ## See `which` arg of scanBamParam
    ## TODO: add warning if reducing to single range
    span <- .spanOfRanges(region)
    ctx$input$region <- span
    ctx$plot$region <- span
    ctx
}
