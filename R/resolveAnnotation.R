#' @keywords internal
.resolveAnnotation <- function(
        annotation, region
) {

    ## When annotation is GRangesList, features are kept that overlap region
    ## When annotation is GRanges, ranges are kept that overlap region
    ## TODO: Might want to restrict this to GRangesList only
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
