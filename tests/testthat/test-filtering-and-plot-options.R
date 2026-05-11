test_that("coverage and junction thresholds filter processed data", {
    bams <- .hnrnpc_bams()
    low <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )
    high <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1000,
        min_junction_reads = 1000,
        return_ctx = TRUE
    )

    expect_gt(length(low$input$cov), length(high$input$cov))
    expect_gt(length(low$input$juncs), length(high$input$juncs))
    expect_true(all(low$input$cov$coverage >= 1))
    expect_true(all(low$input$juncs$coverage >= 1))
})

test_that("junction-only plots work when coverage is removed", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1000,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_length(ctx$input$cov, 0)
    expect_gt(length(ctx$input$juncs), 0)
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("regions with no alignments return an empty plot", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = "chr14:1-1000",
        return_ctx = TRUE
    )

    expect_s4_class(ctx$input$cov, "GRanges")
    expect_s4_class(ctx$input$juncs, "GRanges")
    expect_length(ctx$input$cov, 0)
    expect_length(ctx$input$juncs, 0)
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("compress_introns controls whether plot-space map is created", {
    bams <- .hnrnpc_bams()
    compressed <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        compress_introns = TRUE,
        return_ctx = TRUE
    )
    genomic <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        compress_introns = FALSE,
        return_ctx = TRUE
    )

    expect_s4_class(compressed$plot$map, "GRanges")
    expect_null(genomic$plot$map)
    expect_true(
        max(BiocGenerics::end(compressed$plot$region)) <
            max(BiocGenerics::end(genomic$plot$region))
    )
})

test_that("highlight and psi overlays are resolved and mapped", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        psi = .hnrnpc_psi(),
        highlight = "chr14:70234056-70234097",
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_s4_class(ctx$input$psi, "GRanges")
    expect_s4_class(ctx$input$highlight, "GRanges")
    expect_s4_class(ctx$plot$psi, "GRanges")
    expect_s4_class(ctx$plot$highlight, "GRanges")
    expect_length(ctx$plot$psi, 1)
    expect_length(ctx$plot$highlight, 1)
})

test_that("plot assembly options work with multiple samples", {
    bams <- .hnrnpc_bams()
    .expect_patchwork_renders(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            min_coverage = 1,
            min_junction_reads = 1,
            common_y = TRUE,
            scale_arcs = TRUE,
            colours = c("black", "red"),
            panel_heights = c(1, 2)
        )
    )
})
