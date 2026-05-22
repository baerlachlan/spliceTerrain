#' @keywords internal
.resolveAnnotation <- function(ctx) {
    annotation <- ctx$input$annotation
    region <- ctx$input$region
    if (is.null(annotation)) return(ctx)
    if (!inherits(annotation, "GRangesList"))
        stop("'annotation' must be a GRangesList.")
    if (!length(GenomicRanges::intersect(
        Seqinfo::seqlevels(annotation), Seqinfo::seqlevels(region)
    ))) stop("`annotation` does not overlap `region`")
    hits <- GenomicRanges::findOverlaps(annotation, region)
    if (!length(hits)) stop("`annotation` does not overlap `region`")
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
    ctx$input$annotation <- annotation
    ctx <- .checkAnnotationColumn(ctx, "anno_fill_by")
    ctx <- .checkAnnotationColumn(ctx, "anno_label_by")
    ctx$plot$annotation <- annotation
    ctx
}

#' @keywords internal
.checkAnnotationColumn <- function(ctx, field) {
    column <- ctx$input[[field]]
    if (is.null(column)) return(ctx)
    if (!is.character(column) || length(column) != 1 || is.na(column) ||
            !nzchar(column))
        stop("`", field, "` must be a non-empty character scalar.")
    if (!column %in% names(S4Vectors::mcols(ctx$input$annotation)))
        stop("`", field, "` must name a metadata column in `annotation`.")
    ctx
}
