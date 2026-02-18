#' @keywords internal
.resolveAnnotation <- function(
        annotation, region
) {

    if (is.null(annotation)) return(annotation)

    if (!inherits(annotation, "GRangesList"))
        stop("'annotation' must be a GRangesList.")
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
    annotation

}
