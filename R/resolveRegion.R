#' @keywords internal
.resolveRegion <- function(ctx) {
    region <- ctx$input$region
    if (is.null(region)) return(region)
    if (is.character(region)) {
        region <- .normaliseRegionString(region)
        region <- GenomicRanges::GRanges(region)
    }
    if (inherits(region, "GRangesList"))
        region <- unlist(region, use.names = FALSE)
    if (!inherits(region, "GRanges"))
        stop("`region` must be a GRanges or GRangesList.")
    if (length(unique(as.character(Seqinfo::seqnames(region)))) != 1)
        stop("`region` must resolve to ranges on exactly one seqname.")
    if (length(unique(as.character(BiocGenerics::strand(region)))) != 1)
        stop("`region` ranges must all have the same strand.")
    ## Use one span so BAM queries do not load duplicate alignments
    span <- .spanOfRanges(region)
    ctx$input$region <- span
    ctx$plot$region <- span
    ctx
}

#' @keywords internal
.normaliseRegionString <- function(x) {
    x <- gsub(",", "", x)
    x <- gsub(" ", "", x)
    x <- gsub("\u2013", "-", x)
    x <- gsub("\u2014", "-", x)
    x
}

#' @keywords internal
.spanOfRanges <- function(gr) {
    s <- min(S4Vectors::start(gr))
    e <- max(S4Vectors::end(gr))
    GenomicRanges::GRanges(
        seqnames = GenomicRanges::seqnames(gr)[1],
        ranges   = IRanges::IRanges(start = s, end = e),
        strand   = unique(GenomicRanges::strand(gr))
    )
}
