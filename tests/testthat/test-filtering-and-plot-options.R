.with_interchromosomal_bam <- function(code) {
    sam <- tempfile(fileext = ".sam")
    destination <- sub("\\.sam$", "", sam)
    bam <- paste0(destination, ".bam")
    on.exit(unlink(c(sam, bam, paste0(bam, ".bai"))))

    ## One synthetic pair crosses chromosomes; the control pair stays on c.
    ## Different mate positions expose chromosome concatenation/relabeling.
    records <- data.frame(
        qname = c("cross", "cross", "same", "same"),
        flag = c(97L, 145L, 99L, 147L),
        rname = c("synthetic_a", "synthetic_b", rep("synthetic_c", 2)),
        pos = c(100L, 300L, 100L, 300L), mapq = 60L,
        cigar = rep(c("10M90N10M", "10M190N10M"), 2),
        rnext = c("synthetic_b", "synthetic_a", "=", "="),
        pnext = c(300L, 100L, 300L, 100L),
        tlen = c(0L, 0L, 410L, -410L),
        seq = strrep("A", 20), qual = strrep("I", 20)
    )
    writeLines(c(
        "@HD\tVN:1.6\tSO:coordinate",
        paste0("@SQ\tSN:synthetic_", c("a", "b", "c"), "\tLN:1000"),
        do.call(paste, c(records, sep = "\t"))
    ), sam)
    Rsamtools::asBam(sam, destination = destination, indexDestination = TRUE)
    code(bam)
}

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

test_that("paired BAM coverage stays on the requested chromosome", {
    .with_interchromosomal_bam(function(bam) {
        cases <- data.frame(
            chromosome = paste0(
                "synthetic_", c("a", "b", "b", "b", "c", "b")
            ),
            window = c(
                "90-700", "90-700", "250-700", "305-505", "90-700", "800-900"
            ),
            pairs = c(1L, 1L, 1L, 1L, 1L, 0L)
        )
        starts <- list(
            c(100L, 200L), c(300L, 500L), c(300L, 500L),
            c(305L, 500L), c(100L, 200L, 300L, 500L), integer()
        )
        ends <- list(
            c(109L, 209L), c(309L, 509L), c(309L, 509L),
            c(309L, 505L), c(109L, 209L, 309L, 509L), integer()
        )

        for (i in seq_len(nrow(cases))) {
            region <- paste(cases$chromosome[i], cases$window[i], sep = ":")
            ctx <- spliceTerrain(
                bam = c(sample = bam), region = region,
                min_junction_reads = 1, return_ctx = TRUE
            )

            expect_s4_class(ctx$gal[[1]], "GAlignmentsList")
            expect_length(ctx$gal[[1]], cases$pairs[i])
            expect_identical(
                as.character(Seqinfo::seqnames(ctx$cov)),
                rep(cases$chromosome[i], length(starts[[i]])), info = region
            )
            expect_identical(
                IRanges::ranges(ctx$cov),
                IRanges::IRanges(starts[[i]], ends[[i]]), info = region
            )
            expect_equal(ctx$cov$coverage_raw, rep(1L, length(starts[[i]])),
                         info = region)
            expect_equal(ctx$cov$coverage, rep(1L, length(starts[[i]])),
                         info = region)
            .expect_patchwork_renders(spliceTerrain(ctx = ctx))
        }
    })
})

test_that("paired import retains singletons while applying BAM filters", {
    .with_paired_bam(function(bam) {
        ctx <- spliceTerrain(
            bam = bam, region = "synthetic:90-550", min_mapq = 10,
            min_junction_reads = 1, return_ctx = TRUE
        )

        expect_identical(BiocGenerics::end(ctx$juncs), c(199L, 299L))
        expect_identical(ctx$juncs$coverage_raw, c(3L, 3L))
        expect_identical(
            IRanges::ranges(ctx$cov),
            IRanges::IRanges(
                c(100L, 200L, 300L, 310L, 500L),
                c(109L, 209L, 309L, 319L, 519L)
            )
        )
        expect_identical(ctx$cov$coverage_raw, c(6L, 3L, 4L, 1L, 1L))
        expect_s4_class(ctx$gal[[1]], "GAlignmentsList")
        expect_length(ctx$gal[[1]], 6)
        .expect_patchwork_renders(spliceTerrain(ctx = ctx))
    })
})

test_that("paired import works when only mapped singletons are present", {
    .with_paired_bam(function(bam) {
        ctx <- spliceTerrain(
            bam = bam, region = "synthetic:90-550:+", strandedness = "forward",
            min_junction_reads = 1, return_ctx = TRUE
        )

        expect_identical(BiocGenerics::end(ctx$juncs), 199L)
        expect_identical(ctx$juncs$coverage_raw, 2L)
        expect_identical(ctx$cov$coverage_raw, c(2L, 2L))
        expect_length(ctx$gal[[1]], 2)
        .expect_patchwork_renders(spliceTerrain(ctx = ctx))
    }, keep = c("first_plus", "second_minus"))
})

test_that("complete paired import agrees with the pair-only reader", {
    .with_paired_bam(function(bam) {
        protocols <- c("unstranded", "forward", "reverse")
        for (i in seq_along(protocols)) {
            ctx <- spliceTerrain(
                bam = bam, region = "synthetic:90-850",
                strandedness = protocols[i], min_junction_reads = 1,
                return_ctx = TRUE
            )
            pairs <- GenomicAlignments::readGAlignmentPairs(
                bam, strandMode = i - 1L
            )

            expect_equal(
                GenomicAlignments::coverage(ctx$gal[[1]]),
                GenomicAlignments::coverage(pairs), info = protocols[i]
            )
            expect_equal(
                GenomicAlignments::summarizeJunctions(ctx$gal[[1]]),
                GenomicAlignments::summarizeJunctions(pairs),
                info = protocols[i]
            )
            ## Imported reads are unchanged; summaries count each fragment once.
            expect_identical(ctx$juncs$coverage_raw, c(1L, 1L, 1L))
            expect_length(ctx$gal[[1]], 3)
        }
    }, keep = c("pair_plus", "pair_minus", "overlap"))
})

test_that("overlapping mates count once across library and region strands", {
    .with_paired_bam(function(bam) {
        for (protocol in c("unstranded", "forward", "reverse")) {
            for (target in c("*", "+", "-")) {
                keep <- protocol == "unstranded" || target == "*" ||
                    (protocol == "forward" && target == "+") ||
                    (protocol == "reverse" && target == "-")
                ctx <- spliceTerrain(
                    bam = bam, region = paste0("synthetic:650-850:", target),
                    strandedness = protocol, min_junction_reads = 1,
                    return_ctx = TRUE
                )
                info <- paste(protocol, target)
                expect_identical(
                    IRanges::ranges(ctx$cov),
                    IRanges::IRanges(c(700L, 800L), c(709L, 809L))[keep],
                    info = info
                )
                expect_equal(ctx$cov$coverage_raw, rep(1L, 2L * keep),
                             info = info)
                expect_identical(
                    IRanges::ranges(ctx$juncs),
                    IRanges::IRanges(710L, 799L)[keep], info = info
                )
                expect_equal(ctx$juncs$coverage_raw, rep(1L, as.integer(keep)),
                             info = info)
            }
        }
    }, keep = "overlap")
})

test_that("partial overlaps preserve gaps and independent fragments", {
    reads <- GenomicAlignments::GAlignments(
        seqnames = rep("synthetic", 3), pos = c(100L, 205L, 100L),
        cigar = c("10M90N10M90N10M", "5M90N15M", "10M90N10M90N10M"),
        strand = rep("+", 3)
    )
    aln <- GenomicAlignments::GAlignmentsList(
        pair = reads[1:2], singleton = reads[3]
    )
    ctx <- list(
        input = list(
            gal = list(sample = aln), region = GenomicRanges::GRanges(
                "synthetic:90-350"
            ),
            min_coverage = c(sample = 0), min_junction_reads = c(sample = 1),
            lib_size = NULL, strandedness = c(sample = "unstranded"),
            annotation = NULL
        ),
        plot = list()
    )
    ctx <- spliceTerrain:::.getJunctions(spliceTerrain:::.getCoverage(ctx))

    expect_identical(
        IRanges::ranges(ctx$input$cov),
        IRanges::IRanges(c(100L, 200L, 300L, 310L), c(109L, 209L, 309L, 314L))
    )
    expect_identical(ctx$input$cov$coverage_raw, c(2L, 2L, 2L, 1L))
    expect_identical(
        IRanges::ranges(ctx$input$juncs),
        IRanges::IRanges(c(110L, 210L), c(199L, 299L))
    )
    expect_identical(ctx$input$juncs$coverage_raw, c(2L, 2L))
})

test_that("fragment thresholds are applied before normalisation", {
    .with_paired_bam(function(bam) {
        retained <- spliceTerrain(
            bam = bam, region = "synthetic:650-850",
            min_coverage = 1, min_junction_reads = 1,
            lib_size = 2, normalise_to = 1, return_ctx = TRUE
        )
        filtered <- spliceTerrain(
            bam = bam, region = "synthetic:650-850",
            min_coverage = 2, min_junction_reads = 2, return_ctx = TRUE
        )

        expect_identical(retained$cov$coverage_raw, c(1L, 1L))
        expect_equal(retained$cov$coverage, c(0.5, 0.5))
        expect_identical(retained$juncs$coverage_raw, 1L)
        expect_equal(unname(retained$juncs$coverage), 0.5)
        expect_length(filtered$cov, 0)
        expect_length(filtered$juncs, 0)
        .expect_patchwork_renders(spliceTerrain(ctx = retained))
    }, keep = "overlap")
})

test_that("percentage thresholds use maximum fragment support", {
    .with_paired_bam(function(bam) {
        ctx <- spliceTerrain(
            bam = bam, region = "synthetic:90-850",
            min_coverage = "75%", min_junction_reads = "75%", return_ctx = TRUE
        )

        expect_identical(ctx$cov$coverage_raw, rep(1L, 5))
        expect_identical(ctx$juncs$coverage_raw, c(1L, 1L))
        expect_identical(BiocGenerics::start(ctx$juncs), c(110L, 710L))
    }, keep = c("pair_plus", "overlap"))
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
