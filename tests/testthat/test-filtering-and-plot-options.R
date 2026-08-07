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

    expect_gt(length(low$cov), length(high$cov))
    expect_gt(length(low$juncs), length(high$juncs))
    expect_true(all(low$cov$coverage >= 1))
    expect_true(all(low$juncs$coverage >= 1))
})

test_that("coverage runs are clipped to the plotting region", {
    aln <- GenomicAlignments::GAlignments(
        seqnames = rep("chr1", 3), pos = c(90L, 95L, 110L),
        cigar = c("5M", "10M", "5M"), strand = rep("+", 3)
    )
    ctx <- list(
        input = list(
            gal = list(sample = aln),
            region = GenomicRanges::GRanges("chr1:100-102"),
            min_coverage = c(sample = 0),
            lib_size = NULL
        ),
        plot = list()
    )

    resolved <- spliceTerrain:::.getCoverage(ctx)$input$cov

    expect_identical(IRanges::ranges(resolved), IRanges::IRanges(100L, 102L))
    expect_identical(resolved$coverage_raw, 1L)
})

test_that("percentage coverage thresholds use the in-region maximum", {
    aln <- GenomicAlignments::GAlignments(
        seqnames = rep("chr1", 3), pos = rep(90L, 3),
        cigar = c("10M100N5M", "10M100N5M", "10M110N5M"),
        strand = rep("+", 3)
    )
    ctx <- list(
        input = list(
            gal = list(sample = aln),
            region = GenomicRanges::GRanges("chr1:200-214"),
            min_coverage = c(sample = "75%"),
            lib_size = NULL
        ),
        plot = list()
    )

    resolved <- spliceTerrain:::.getCoverage(ctx)$input$cov

    expect_length(resolved, 1)
    expect_identical(resolved$coverage_raw, 2L)
})

test_that("percentage junction thresholds use the in-region maximum", {
    aln <- GenomicAlignments::GAlignments(
        seqnames = rep("chr1", 3), pos = rep(180L, 3),
        cigar = c(
            "5M5N5M100N5M", "5M5N5M100N5M", "5M5N5M90N5M"
        ),
        strand = rep("+", 3)
    )
    ctx <- list(
        input = list(
            gal = list(sample = aln),
            region = GenomicRanges::GRanges("chr1:195-300"),
            min_junction_reads = c(sample = "75%"),
            strandedness = c(sample = "unstranded"),
            lib_size = NULL,
            annotation = NULL
        ),
        plot = list()
    )

    resolved <- spliceTerrain:::.getJunctions(ctx)$input$juncs

    expect_length(resolved, 1)
    expect_identical(resolved$coverage_raw, 2L)
})

test_that("per-BAM thresholds can mix counts and percentages", {
    thresholds <- c("5", "10%")

    expect_silent(
        spliceTerrain:::.checkPercentageThreshold(thresholds, "threshold")
    )
    expect_equal(
        spliceTerrain:::.resolveMinThreshold(thresholds[1], c(5, 20)), 5
    )
    expect_equal(
        spliceTerrain:::.resolveMinThreshold(thresholds[2], c(5, 20)), 2
    )
})

test_that("coverage and junctions can be normalised by library size", {
    bams <- stats::setNames(.hnrnpc_bams()[c(7, 1)], c("s1", "s2"))
    norm <- spliceTerrain(
        bam = bams,
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        lib_size = c(1e6, 2e6),
        normalise_to = 1e6,
        return_ctx = TRUE
    )

    expect_true("coverage_raw" %in% names(S4Vectors::mcols(norm$cov)))
    expect_true("coverage_raw" %in% names(S4Vectors::mcols(norm$juncs)))

    cov_1 <- norm$cov[norm$cov$sample == "s1"]
    cov_2 <- norm$cov[norm$cov$sample == "s2"]
    junc_2 <- norm$juncs[norm$juncs$sample == "s2"]
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

    cov_2 <- norm$cov[norm$cov$sample == "s2"]
    junc_2 <- norm$juncs[norm$juncs$sample == "s2"]
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

    expect_length(ctx$cov, 0)
    expect_gt(length(ctx$juncs), 0)
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
    ctx$common_y <- TRUE
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("regions with no alignments return an empty plot", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = "chr14:1-1000",
        return_ctx = TRUE
    )

    expect_s4_class(ctx$cov, "GRanges")
    expect_s4_class(ctx$juncs, "GRanges")
    expect_length(ctx$cov, 0)
    expect_length(ctx$juncs, 0)
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

    toggled <- compressed
    toggled$compress_introns <- FALSE
    compressed <- .prepare_plot_context(compressed)
    genomic <- .prepare_plot_context(genomic)
    toggled <- .prepare_plot_context(toggled)
    expect_s4_class(compressed$plot$map, "GRanges")
    expect_null(genomic$plot$map)
    expect_null(toggled$plot$map)
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

    expect_s4_class(ctx$psi, "GRanges")
    expect_s4_class(ctx$highlight, "GRanges")
    plotted <- .prepare_plot_context(ctx)
    expect_s4_class(plotted$plot$psi, "GRanges")
    expect_s4_class(plotted$plot$highlight, "GRanges")
    expect_length(plotted$plot$psi, 1)
    expect_length(plotted$plot$highlight, 1)
})

test_that("overlay character regions are normalised before coercion", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        psi = "chr14:70,234,854 - 70,234,854",
        highlight = "chr14:70234056\u201370234097",
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_identical(BiocGenerics::start(ctx$psi), 70234854L)
    expect_identical(BiocGenerics::end(ctx$psi), 70234854L)
    expect_identical(BiocGenerics::start(ctx$highlight), 70234056L)
    expect_identical(BiocGenerics::end(ctx$highlight), 70234097L)
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
            arc_scale = TRUE,
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

test_that("annotation matches control junction arc linetype", {
    junc <- GenomicRanges::GRanges(
        c("chr1:10-20", "chr1:30-40"),
        coverage = c(2, 3),
        annotation_match = c(TRUE, FALSE)
    )
    layout <- spliceTerrain:::.junctionArcLayout(
        junc, GenomicRanges::GRanges(), 1, 1
    )

    arcs <- spliceTerrain:::.junctionArcPoints(layout)

    expect_identical(unique(arcs$linetype_on[arcs$id == 1]), "solid")
    expect_identical(unique(arcs$linetype_on[arcs$id == 2]), "dashed")
    expect_identical(unique(arcs$linetype_off), "solid")
})
