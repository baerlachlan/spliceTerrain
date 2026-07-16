test_that("BAM names must be complete and unique when supplied", {
    bams <- .placeholder_bams(2)

    expect_error(
        spliceTerrain(bam = "missing.bam", region = .hnrnpc_region()),
        "do not exist",
        fixed = TRUE
    )

    expect_error(
        spliceTerrain(
            bam = stats::setNames(bams, c("sample", "")),
            region = .hnrnpc_region()
        ),
        "`bam` names must not be NA or empty.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = stats::setNames(bams, c("sample", NA_character_)),
            region = .hnrnpc_region()
        ),
        "`bam` names must not be NA or empty.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = stats::setNames(bams, c("sample", "sample")),
            region = .hnrnpc_region()
        ),
        "sample names must be unique",
        fixed = TRUE
    )
})

test_that("per-BAM numeric arguments must be scalar or one value per BAM", {
    bams <- .placeholder_bams(2)

    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            min_coverage = c(1, 2, 3)
        ),
        "`min_coverage` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            min_junction_reads = c(1, 2, 3)
        ),
        "`min_junction_reads` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[1], region = .hnrnpc_region(), min_mapq = NA_real_
        ),
        "`min_mapq` must be a non-negative whole number.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[1], region = .hnrnpc_region(), min_coverage = -1
        ),
        "`min_coverage` values must be non-negative whole numbers.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[1], region = .hnrnpc_region(),
            min_junction_reads = 1.5
        ),
        "`min_junction_reads` values must be non-negative whole numbers.",
        fixed = TRUE
    )
})

test_that("normalisation inputs must match BAMs", {
    bams <- .placeholder_bams(2)

    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            lib_size = 1e6
        ),
        "`lib_size` must be NULL or the number of BAMs",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            norm_factors = c(1, 1)
        ),
        "`norm_factors` requires `lib_size`.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            lib_size = c(1e6, -1)
        ),
        "`lib_size` values must be positive finite numbers.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            lib_size = c(1e6, 1e6),
            norm_factors = c(1, NA)
        ),
        "`norm_factors` values must be positive finite numbers.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            lib_size = c(1e6, 1e6),
            normalise_to = 0
        ),
        "`normalise_to` must be a positive finite number.",
        fixed = TRUE
    )
})

test_that("panel_heights must match plot panels", {
    bams <- .placeholder_bams(2)
    bam_msg <- paste0(
        "`panel_heights` must be length 1 or the number of plot panels ",
        "(2: 2 BAM panel(s))."
    )
    anno_msg <- paste0(
        "`panel_heights` must be length 1 or the number of plot panels ",
        "(2: 1 BAM panel(s) plus 1 annotation panel)."
    )
    annotation <- GenomicRanges::GRangesList(
        tx = GenomicRanges::GRanges(
            seqnames = "chr14",
            ranges = IRanges::IRanges(70222436L, 70222446L)
        )
    )

    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            panel_heights = c(1, 1, 1)
        ),
        bam_msg,
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[1],
            region = .hnrnpc_region(),
            annotation = annotation,
            panel_heights = c(1, 1, 1)
        ),
        anno_msg,
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            panel_heights = c(1, NA)
        ),
        "`panel_heights` values must be positive finite numbers.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            panel_heights = c(1, 0)
        ),
        "`panel_heights` values must be positive finite numbers.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams, region = .hnrnpc_region(),
            colours = c("black", "red", "blue")
        ),
        "`colours` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
})

test_that("strandedness values must be supported", {
    bams <- .placeholder_bams(2)

    expect_error(
        spliceTerrain(
            bam = bams[1],
            region = .hnrnpc_region(),
            strandedness = "antisense"
        ),
        "`strandedness` must be one of: unstranded, forward, reverse",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams, region = .hnrnpc_region(),
            strandedness = c("unstranded", "forward", "reverse")
        ),
        "`strandedness` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
})

test_that("arc_side values must be supported", {
    bams <- .placeholder_bams()

    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            arc_side = "top"
        ),
        "`arc_side` must be one of: both, above, below",
        fixed = TRUE
    )
})

test_that("psi and highlight must overlap the resolved region", {
    bams <- .placeholder_bams()

    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            psi = "chr1:1-100"
        ),
        "`psi` does not overlap `region`",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            psi = "chr14:1-100"
        ),
        "`psi` does not overlap `region`",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            highlight = "chr1:1-100"
        ),
        "`highlight` does not overlap `region`",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            highlight = "chr14:1-100"
        ),
        "`highlight` does not overlap `region`",
        fixed = TRUE
    )
})
