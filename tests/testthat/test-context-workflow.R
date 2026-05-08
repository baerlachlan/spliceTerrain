test_that("returned contexts can be replotted after removing junctions", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    ctx$plot$juncs <- ctx$plot$juncs[FALSE]

    expect_length(ctx$plot$juncs, 0L)
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})
