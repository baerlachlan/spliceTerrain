#' @keywords internal
.resolveAnnotation <- function(ctx) {
    annotation <- ctx$input$annotation
    region <- ctx$input$region
    if (is.null(annotation)) return(ctx)
    if (!inherits(annotation, "GRangesList"))
        stop("'annotation' must be a GRangesList.")
    same_seqlevel <- any(
        Seqinfo::seqlevels(annotation) %in% Seqinfo::seqlevels(region)
    )
    if (!same_seqlevel) stop("`annotation` does not overlap `region`")
    hits <- GenomicRanges::findOverlaps(annotation, region)
    if (!length(hits)) stop("`annotation` does not overlap `region`")
    annotation <- annotation[S4Vectors::from(hits)]
    annotation <- BiocGenerics::sort(annotation)

    group <- names(annotation)
    len <- length(annotation)
    if (is.null(group)) {
        group <- paste0("annotation_", seq_len(len))
    } else if (anyNA(group) || any(!nzchar(group)) || anyDuplicated(group)) {
        stop("`annotation` group names must be non-empty and unique.")
    }

    region_seqname <- as.character(Seqinfo::seqnames(region))[1]
    wrong_seqname <- vapply(annotation, function(x) {
        any(as.character(Seqinfo::seqnames(x)) != region_seqname)
    }, logical(1))
    if (any(wrong_seqname)) {
        stop(
            "Each `annotation` group must contain ranges only on the ",
            "plotting seqname."
        )
    }
    mixed_strand <- vapply(annotation, function(x) {
        length(unique(as.character(BiocGenerics::strand(x)))) != 1
    }, logical(1))
    if (any(mixed_strand)) {
        stop("Ranges within each `annotation` group must share one strand.")
    }

    lens <- lengths(annotation)
    annotation <- unlist(annotation, use.names = FALSE)
    annotation$group <- rep(group, lens)
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
