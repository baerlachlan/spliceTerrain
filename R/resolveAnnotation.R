#' @keywords internal
.resolveAnnotation <- function(ctx) {
    annotation <- ctx$args$annotation
    region <- ctx$args$region
    if (is.null(annotation)) return(ctx)
    if (!inherits(annotation, "GRangesList"))
        stop("'annotation' must be a GRangesList.")
    if (!length(GenomicRanges::intersect(
        Seqinfo::seqlevels(annotation), Seqinfo::seqlevels(region)
    ))) stop("`annotation` does not overlap `region`")

    hits <- GenomicRanges::findOverlaps(annotation, region)
    annotation <- annotation[S4Vectors::from(hits)]
    annotation <- BiocGenerics::sort(annotation)
    len <- length(annotation)
    lens <- lengths(annotation)
    annotation <- unlist(annotation)
    if (!is.null(names(annotation))) {
        annotation$group <- rep(names(lens), lens)
    } else {
        annotation$group <- rep(paste0("annotation_", seq_len(len)), lens)
    }
    ctx$args$annotation <- annotation
    ctx
}
