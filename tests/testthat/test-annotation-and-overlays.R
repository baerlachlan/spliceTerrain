test_that("EnsDb annotation is resolved into grouped plotting ranges", {
    bams <- .hnrnpc_bams()
    annotation <- .hnrnpc_annotation()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = annotation,
        anno_text_col = "exon_rank",
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_s4_class(ctx$input$annotation, "GRanges")
    expect_s4_class(ctx$plot$annotation, "GRanges")
    expect_true(all(ctx$input$annotation$group %in% names(annotation)))
    expect_true("exon_rank" %in% names(S4Vectors::mcols(ctx$input$annotation)))
    seqnames <- as.character(Seqinfo::seqnames(ctx$input$annotation))
    expect_true(all(seqnames == "chr14"))
})

test_that("annotation panel and annotation labels can be plotted", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = .hnrnpc_annotation(),
        anno_text_col = "exon_rank",
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    plotted <- spliceTerrain:::.plotSamples(ctx)
    plotted <- spliceTerrain:::.plotAnnotation(plotted)

    expect_true("annotation" %in% names(plotted$plot$plist))
    expect_s3_class(plotted$plot$plist$annotation, "ggplot")
    expect_gte(length(plotted$plot$plist$annotation$layers), 4L)
    expect_s3_class(spliceTerrain(ctx = ctx), "patchwork")
})

test_that("absent annotation label columns are ignored", {
    bams <- .hnrnpc_bams()
    expect_s3_class(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            annotation = .hnrnpc_annotation(),
            anno_text_col = "missing_column",
            min_coverage = 1,
            min_junction_reads = 1
        ),
        "patchwork"
    )
})

test_that("annotation input must be a GRangesList overlapping region", {
    bams <- .hnrnpc_bams()
    bad_annotation <- GenomicRanges::GRanges(
        seqnames = "chr14",
        ranges = IRanges::IRanges(70233810L, 70234097L)
    )
    off_region_annotation <- GenomicRanges::GRangesList(
        tx = GenomicRanges::GRanges(
            seqnames = "chr1",
            ranges = IRanges::IRanges(1L, 100L)
        )
    )

    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            annotation = bad_annotation
        ),
        "'annotation' must be a GRangesList.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams[7],
            region = .hnrnpc_region(),
            annotation = off_region_annotation
        ),
        "`annotation` does not overlap `region`",
        fixed = TRUE
    )
})

test_that("psi adds percentage labels to selected junctions", {
    bams <- .hnrnpc_bams()
    with_psi <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        psi = .hnrnpc_psi(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )
    without_psi <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    labels_with_psi <- spliceTerrain:::.junctionArcLabels(
        spliceTerrain:::.junctionArcLayout(
            with_psi$plot$juncs, with_psi$plot$cov, 0.15, NULL
        ),
        with_psi$plot$juncs,
        with_psi$plot$psi
    )
    labels_without_psi <- spliceTerrain:::.junctionArcLabels(
        spliceTerrain:::.junctionArcLayout(
            without_psi$plot$juncs, without_psi$plot$cov, 0.15, NULL
        ),
        without_psi$plot$juncs,
        without_psi$plot$psi
    )

    expect_true(any(grepl("%", labels_with_psi$label, fixed = TRUE)))
    expect_false(any(grepl("%", labels_without_psi$label, fixed = TRUE)))
})

test_that("highlight intervals are plotted with sample and annotation panels", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = .hnrnpc_annotation(),
        highlight = .hnrnpc_highlight(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )
    plotted <- spliceTerrain:::.plotSamples(ctx)
    plotted <- spliceTerrain:::.plotAnnotation(plotted)

    expect_s4_class(ctx$input$highlight, "GRanges")
    expect_s4_class(ctx$plot$highlight, "GRanges")
    expect_true(all(vapply(plotted$plot$plist, function(p) {
        any(vapply(p$layers, function(layer) {
            inherits(layer$geom, "GeomRect")
        }, logical(1)))
    }, logical(1))))
})
