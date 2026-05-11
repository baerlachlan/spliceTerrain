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

    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("spliceTerrain validates high-risk user inputs before plotting", {
    bams <- .hnrnpc_bams()

    expect_error(
        spliceTerrain(bam = "missing.bam", region = .hnrnpc_region()),
        "do not exist",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = stats::setNames(bams[c(7, 1)], c("sample", "sample")),
            region = .hnrnpc_region()
        ),
        "sample names must be unique",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            strandedness = c("unstranded", "forward", "reverse")
        ),
        "`strandedness` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            colours = c("black", "red", "blue")
        ),
        "`colours` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
})
