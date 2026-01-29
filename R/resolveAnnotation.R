#' @keywords internal
.resolveAnnotation <- function(
        annotation, region
) {

    # annotation <- GenomicRanges::subsetByOverlaps(annotation, region)
    ## When annotation is GRangesList, features are kept that overlap region
    ## When annotation is GRanges, ranges are kept that overlap region
    hits <- GenomicRanges::findOverlaps(annotation, region)
    annotation <- annotation[S4Vectors::from(hits)]
    annotation <- BiocGenerics::sort(annotation)
    lens <- lengths(annotation)
    annotation <- unlist(annotation)
    if (!is.null(names(annotation))) {
        annotation$group <- rep(names(lens), lens)
    } else {
        annotation$group <- rep(paste0("feature_", seq_len(length(annotation))), lens)
    }
    annotation

}
