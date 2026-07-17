test_that("spliceTerrain returns processed context for HNRNPC BAMs", {
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
    expect_named(ctx, c("input", "plot"))
    expect_s4_class(ctx$input$cov, "GRanges")
    expect_s4_class(ctx$input$juncs, "GRanges")
    expect_s4_class(ctx$plot$cov, "GRanges")
    expect_s4_class(ctx$plot$juncs, "GRanges")
    expect_gt(length(ctx$input$cov), 0)
    expect_gt(length(ctx$input$juncs), 0)
})

test_that("spliceTerrain builds plots from returned contexts", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    ctx$input$annotated_junctions <- NULL
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
    ctx$plot$juncs <- ctx$plot$juncs[FALSE]
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})
