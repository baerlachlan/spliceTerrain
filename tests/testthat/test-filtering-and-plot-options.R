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

test_that("coverage and junctions can be normalised by library size", {
    bams <- stats::setNames(.hnrnpc_bams()[c(7, 1)], c("s1", "s2"))
    raw <- spliceTerrain(
        bam = bams,
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )
    norm <- spliceTerrain(
        bam = bams,
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        lib_size = c(1e6, 2e6),
        normalise_to = 1e6,
        return_ctx = TRUE
    )

    expect_true("coverage_raw" %in% names(S4Vectors::mcols(norm$input$cov)))
    expect_true("coverage_raw" %in% names(S4Vectors::mcols(norm$input$juncs)))
    expect_identical(length(norm$input$cov), length(raw$input$cov))
    expect_identical(length(norm$input$juncs), length(raw$input$juncs))

    cov_1 <- norm$input$cov[norm$input$cov$sample == "s1"]
    cov_2 <- norm$input$cov[norm$input$cov$sample == "s2"]
    junc_2 <- norm$input$juncs[norm$input$juncs$sample == "s2"]
    expect_equal(cov_1$coverage, cov_1$coverage_raw)
    expect_equal(cov_2$coverage, cov_2$coverage_raw / 2)
    expect_equal(junc_2$coverage, junc_2$coverage_raw / 2)
})

test_that("normalisation factors adjust effective library sizes", {
    bams <- stats::setNames(.hnrnpc_bams()[c(7, 1)], c("s1", "s2"))
    norm <- spliceTerrain(
        bam = bams,
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        lib_size = c(1e6, 1e6),
        norm_factors = c(1, 2),
        normalise_to = 1e6,
        return_ctx = TRUE
    )

    cov_2 <- norm$input$cov[norm$input$cov$sample == "s2"]
    junc_2 <- norm$input$juncs[norm$input$juncs$sample == "s2"]
    expect_equal(cov_2$coverage, cov_2$coverage_raw / 2)
    expect_equal(junc_2$coverage, junc_2$coverage_raw / 2)
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

test_that("arc_height scales the default junction arc height", {
    cov <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(1L, 5L),
        coverage = 10
    )
    junc <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(10L, 20L),
        coverage = 1
    )

    default <- spliceTerrain:::.junctionArcLayout(junc, cov, 1, NULL)
    doubled <- spliceTerrain:::.junctionArcLayout(junc, cov, 2, NULL)

    expect_equal(doubled$heights, default$heights * 2)
})

test_that("arc_side controls junction arc placement", {
    cov <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(1L, 60L),
        coverage = 10
    )
    junc <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(c(10L, 25L, 40L), c(20L, 35L, 50L)),
        coverage = 1
    )

    both <- spliceTerrain:::.junctionArcLayout(junc, cov, 1, NULL, "both")
    above <- spliceTerrain:::.junctionArcLayout(junc, cov, 1, NULL, "above")
    below <- spliceTerrain:::.junctionArcLayout(junc, cov, 1, NULL, "below")

    expect_true(any(both$above))
    expect_true(any(!both$above))
    expect_true(all(above$above))
    expect_false(any(below$above))
})
