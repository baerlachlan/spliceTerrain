test_that("BAM names must be complete and unique when supplied", {
    bams <- .hnrnpc_bams()

    expect_error(
        spliceTerrain(
            bam = stats::setNames(bams[c(7, 1)], c("sample", "")),
            region = .hnrnpc_region()
        ),
        "`bam` names must not be NA or empty.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = stats::setNames(bams[c(7, 1)], c("sample", NA_character_)),
            region = .hnrnpc_region()
        ),
        "`bam` names must not be NA or empty.",
        fixed = TRUE
    )
})

test_that("per-BAM numeric arguments must be scalar or one value per BAM", {
    bams <- .hnrnpc_bams()

    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            min_coverage = c(1, 2, 3)
        ),
        "`min_coverage` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            min_junction_reads = c(1, 2, 3)
        ),
        "`min_junction_reads` must be length 1 or the number of BAMs",
        fixed = TRUE
    )
})

test_that("normalisation inputs must match BAMs", {
    bams <- .hnrnpc_bams()

    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            lib_size = 1e6
        ),
        "`lib_size` must be NULL or the number of BAMs",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            norm_factors = c(1, 1)
        ),
        "`norm_factors` requires `lib_size`.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            lib_size = c(1e6, -1)
        ),
        "`lib_size` values must be positive finite numbers.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            lib_size = c(1e6, 1e6),
            norm_factors = c(1, NA)
        ),
        "`norm_factors` values must be positive finite numbers.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[c(7, 1)],
            region = .hnrnpc_region(),
            lib_size = c(1e6, 1e6),
            normalise_to = 0
        ),
        "`normalise_to` must be a positive finite number.",
        fixed = TRUE
    )
})

test_that("strandedness values must be supported", {
    bams <- .hnrnpc_bams()

    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            strandedness = "antisense"
        ),
        "`strandedness` must be one of: unstranded, forward, reverse",
        fixed = TRUE
    )
})

test_that("arc_side values must be supported", {
    bams <- .hnrnpc_bams()

    ctx <- spliceTerrain:::.checkArcSide(list(input = list(arc_side = "abo")))
    expect_identical(ctx$input$arc_side, "above")

    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            arc_side = "top"
        ),
        "`arc_side` must be one of: both, above, below",
        fixed = TRUE
    )
})

test_that("psi and highlight must overlap the resolved region", {
    bams <- .hnrnpc_bams()

    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            psi = "chr1:1-100"
        ),
        "`psi` does not overlap `region`",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            psi = "chr14:1-100"
        ),
        "`psi` does not overlap `region`",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            highlight = "chr1:1-100"
        ),
        "`highlight` does not overlap `region`",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            highlight = "chr14:1-100"
        ),
        "`highlight` does not overlap `region`",
        fixed = TRUE
    )
})
