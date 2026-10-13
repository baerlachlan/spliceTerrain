#' @keywords internal
.initPlotContext <- function() {
    list(
        cov = NULL, juncs = NULL, map = NULL, annotation = NULL,
        region = NULL, junction_usage = NULL, highlight = NULL, plist = NULL
    )
}

#' @keywords internal
.asContextList <- function(ctx) {
    out <- ctx$input
    out[c("return_ctx", "ctx")] <- NULL
    out
}

#' @keywords internal
.restoreContext <- function(x) {
    x <- .checkContext(x)
    ctx <- list(input = x, plot = .initPlotContext())
    ctx <- .resolveGr(ctx, "junction_usage")
    ctx <- .checkColours(ctx)
    ctx <- .checkArcSide(ctx)
    ctx <- .checkAnnotatedJunctions(ctx)
    ctx <- .checkPlotOptions(ctx)
    ctx <- .checkPanelHeights(ctx)
    ctx <- .checkAnnotationColumn(ctx, "anno_fill_by")
    .checkAnnotationColumn(ctx, "anno_label_by")
}

#' @keywords internal
.preparePlotContext <- function(ctx) {
    if (length(ctx$input$annotation)) {
        group <- as.character(ctx$input$annotation$group)
        group <- factor(group, levels = unique(group))
        annotation <- split(ctx$input$annotation, group)
        annotation <- lapply(annotation, BiocGenerics::sort)
        ctx$input$annotation <- do.call(c, unname(annotation))
    }
    ctx$input$juncs <- .matchAnnotatedJunctions(
        ctx$input$juncs, ctx$input$annotation
    )
    ctx$input$juncs <- .addJunctionUsage(
        ctx$input$juncs, ctx$input$juncs_all, ctx$input$junction_usage
    )
    ctx$plot <- .initPlotContext()
    fields <- c(
        "cov", "juncs", "annotation", "region", "junction_usage", "highlight"
    )
    ctx$plot[fields] <- ctx$input[fields]
    .applyMap(ctx)
}

#' @keywords internal
.checkContext <- function(ctx) {
    if (!is.list(ctx))
        stop("`ctx` must be a list returned by `spliceTerrain()`.")

    nullable <- c(
        "annotation", "junction_usage", "highlight", "anno_fill_by",
        "anno_fill_colours", "anno_label_by"
    )
    missing_nullable <- setdiff(nullable, names(ctx))
    ctx[missing_nullable] <- rep(list(NULL), length(missing_nullable))

    required <- c(
        "bam", "region", "annotation", "junction_usage",
        "junction_usage_label_sep", "highlight", "cov", "juncs", "juncs_all",
        "compress_introns", "intron_width", "common_y", "arc_height",
        "arc_side", "arc_scale", "colours", "min_arrow", "highlight_colour",
        "anno_fill_by", "anno_fill_colours",
        "anno_label_by", "anno_label_colour", "anno_label_size",
        "junc_text_size", "panel_heights", "axis_title_size",
        "axis_text_size", "annotated_junctions"
    )
    missing_fields <- setdiff(required, names(ctx))
    if (length(missing_fields))
        stop("`ctx` is missing required field(s): ", toString(missing_fields))

    if (!inherits(ctx$region, "GRanges") || length(ctx$region) != 1)
        stop("`ctx$region` must be a single GRanges range.")
    for (field in c("cov", "juncs", "juncs_all")) {
        if (!inherits(ctx[[field]], "GRanges"))
            stop("`ctx$", field, "` must be a GRanges object.")
    }
    for (field in c("annotation", "junction_usage", "highlight")) {
        value <- ctx[[field]]
        if (!is.null(value) && !inherits(value, "GRanges"))
            stop("`ctx$", field, "` must be NULL or a GRanges object.")
    }
    region_seqname <- as.character(Seqinfo::seqnames(ctx$region))
    for (field in c("junction_usage", "highlight")) {
        value <- ctx[[field]]
        if (length(value) &&
                any(as.character(Seqinfo::seqnames(value)) != region_seqname))
            stop("`ctx$", field, "` ranges must use the plotting seqname.")
    }
    .checkContextAnnotation(ctx)

    for (field in c("cov", "juncs", "juncs_all"))
        .checkContextTrack(ctx, field)
    ctx
}

#' @keywords internal
.checkContextTrack <- function(ctx, field) {
    x <- ctx[[field]]
    required <- c("sample", "coverage")
    if (field == "juncs_all") required <- c(required, "coverage_raw")
    missing_fields <- setdiff(required, names(S4Vectors::mcols(x)))
    if (length(missing_fields))
        stop(
            "`ctx$", field, "` is missing required metadata field(s): ",
            toString(missing_fields)
        )
    if (anyNA(x$sample) || any(!x$sample %in% names(ctx$bam)))
        stop("`ctx$", field, "$sample` values must match `ctx$bam` names.")
    for (column in setdiff(required, "sample")) {
        values <- S4Vectors::mcols(x)[[column]]
        if (!is.numeric(values) || anyNA(values) ||
                any(!is.finite(values)) || any(values < 0))
            stop("`ctx$", field, "$", column,
                "` must contain non-negative numbers.")
    }
    region_seqname <- as.character(Seqinfo::seqnames(ctx$region))
    if (length(x) && any(as.character(Seqinfo::seqnames(x)) != region_seqname))
        stop("`ctx$", field, "` ranges must use the plotting seqname.")
    if (field == "juncs_all" && anyDuplicated(S4Vectors::DataFrame(
        sample = as.character(x$sample),
        start = BiocGenerics::start(x), end = BiocGenerics::end(x)
    ))) stop("`ctx$juncs_all` must contain one row per sample and junction.")
}

#' @keywords internal
.checkContextAnnotation <- function(ctx) {
    annotation <- ctx$annotation
    if (!length(annotation)) return(invisible(NULL))
    if (!"group" %in% names(S4Vectors::mcols(annotation)))
        stop("`ctx$annotation` must contain a `group` metadata column.")
    group <- as.character(annotation$group)
    if (anyNA(group) || any(!nzchar(group)))
        stop("`ctx$annotation$group` values must be non-empty and non-missing.")
    region_seqname <- as.character(Seqinfo::seqnames(ctx$region))
    if (any(as.character(Seqinfo::seqnames(annotation)) != region_seqname))
        stop("`ctx$annotation` ranges must use the plotting seqname.")
    annotation <- split(annotation, group)
    mixed_strand <- vapply(annotation, function(x) {
        length(unique(as.character(BiocGenerics::strand(x)))) != 1
    }, logical(1))
    if (any(mixed_strand))
        stop("Ranges within each `ctx$annotation` group must share one strand.")
}
