test_that("spliceTerrain returns a flat genomic context", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        psi = .hnrnpc_psi(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_type(ctx, "list")
    expect_identical(class(ctx), "list")
    expect_false(any(c("input", "plot", "ctx", "return_ctx") %in% names(ctx)))
    expect_s4_class(ctx$cov, "GRanges")
    expect_s4_class(ctx$juncs, "GRanges")
    expect_gt(length(ctx$cov), 0)
    expect_gt(length(ctx$juncs), 0)
    expect_gt(BiocGenerics::start(ctx$region), 1e6)
})

test_that("spliceTerrain remaps modified genomic contexts", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    ctx$juncs <- ctx$juncs[1]
    plotted <- .prepare_plot_context(ctx)

    expect_length(plotted$plot$juncs, 1)
    expect_false(identical(
        BiocGenerics::start(plotted$plot$juncs),
        BiocGenerics::start(ctx$juncs)
    ))
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("modified annotation matching is recalculated", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = .hnrnpc_annotation(),
        annotated_junctions = TRUE,
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_true(any(ctx$juncs$annotation_match))
    ctx$annotation <- rev(ctx$annotation)
    group_order <- unique(ctx$annotation$group)
    plotted <- .prepare_plot_context(ctx)
    expect_identical(unique(plotted$input$annotation$group), group_order)
    starts <- split(
        BiocGenerics::start(plotted$input$annotation),
        plotted$input$annotation$group
    )
    expect_true(all(vapply(starts, function(x) {
        all(diff(x) >= 0)
    }, logical(1))))

    ctx$annotation <- NULL
    ctx$annotated_junctions <- FALSE
    plotted <- .prepare_plot_context(ctx)

    expect_false(any(plotted$plot$juncs$annotation_match))
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("reused contexts are validated", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7], region = .hnrnpc_region(), return_ctx = TRUE
    )

    expect_error(
        spliceTerrain(ctx = 1),
        "`ctx` must be a list returned by `spliceTerrain()`.",
        fixed = TRUE
    )
    bad_ctx <- ctx
    bad_ctx$cov <- NULL
    expect_error(
        spliceTerrain(ctx = bad_ctx),
        "`ctx` is missing required field(s): cov",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(ctx = ctx, colours = "red"),
        "No other arguments may be supplied when using `ctx`.",
        fixed = TRUE
    )

    bad_ctx <- ctx
    bad_ctx$compress_introns <- NA
    expect_error(
        spliceTerrain(ctx = bad_ctx),
        "`compress_introns` must be TRUE or FALSE.",
        fixed = TRUE
    )

    bad_ctx <- ctx
    bad_ctx$annotation <- GenomicRanges::GRanges(
        seqnames = "chr14",
        ranges = IRanges::IRanges(c(70233810L, 70234056L), width = 10L),
        strand = c("+", "-"),
        group = "tx"
    )
    expect_error(
        spliceTerrain(ctx = bad_ctx),
        "Ranges within each `ctx$annotation` group must share one strand.",
        fixed = TRUE
    )

    bad_ctx <- ctx
    bad_ctx$cov$coverage[1] <- -1
    expect_error(
        spliceTerrain(ctx = bad_ctx),
        "`ctx$cov$coverage` must contain non-negative numbers.",
        fixed = TRUE
    )

    zero_ctx <- ctx
    for (field in c(
        "intron_width", "min_arrow", "arc_height", "anno_label_size",
        "junc_text_size", "axis_title_size", "axis_text_size"
    )) zero_ctx[[field]] <- 0
    expect_silent(.prepare_plot_context(zero_ctx))
})
