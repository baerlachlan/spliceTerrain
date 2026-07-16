test_that("EnsDb annotation is resolved into grouped plotting ranges", {
    bams <- .hnrnpc_bams()
    annotation <- .hnrnpc_annotation()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = annotation,
        anno_label_by = "exon_rank",
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
        anno_label_by = "exon_rank",
        anno_label_colour = "black",
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    plotted <- spliceTerrain:::.plotSamples(ctx)
    plotted <- spliceTerrain:::.plotAnnotation(plotted)

    expect_true("annotation" %in% names(plotted$plot$plist))
    expect_s3_class(plotted$plot$plist$annotation, "ggplot")
    segment_layers <- vapply(
        plotted$plot$plist$annotation$layers,
        function(layer) inherits(layer$geom, "GeomSegment"),
        logical(1)
    )
    expect_true(any(vapply(
        plotted$plot$plist$annotation$layers[segment_layers],
        function(layer) !is.null(layer$geom_params$arrow),
        logical(1)
    )))
    text_layers <- vapply(
        plotted$plot$plist$annotation$layers,
        function(layer) inherits(layer$geom, "GeomText"),
        logical(1)
    )
    expect_true(any(text_layers))
    text_layer <- plotted$plot$plist$annotation$layers[[which(text_layers)[1]]]
    expect_identical(
        text_layer$aes_params$colour,
        "black"
    )
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("annotation fill can use transcript groups or exon metadata", {
    bams <- .hnrnpc_bams()
    annotation <- .hnrnpc_annotation()
    fill_colours <- stats::setNames(c("#1b9e77", "#d95f02"), names(annotation))
    rank_colours <- c("1" = "#1b9e77", "2" = "#d95f02")
    by_group <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = annotation,
        anno_fill_by = "group",
        anno_fill_colours = fill_colours,
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )
    by_exon <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = annotation,
        anno_fill_by = "exon_rank",
        anno_fill_colours = rank_colours,
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    plotted_group <- spliceTerrain:::.plotAnnotation(
        spliceTerrain:::.plotSamples(by_group)
    )
    plotted_exon <- spliceTerrain:::.plotAnnotation(
        spliceTerrain:::.plotSamples(by_exon)
    )
    expect_equal(
        unname(
            plotted_group$plot$plist$annotation$scales$get_scales("fill")$
                palette(2)
        ),
        unname(fill_colours)
    )
    expect_equal(
        unname(
            plotted_exon$plot$plist$annotation$scales$get_scales("fill")$
                palette(2)
        ),
        unname(rank_colours)
    )
    tile_layers <- vapply(
        plotted_group$plot$plist$annotation$layers,
        function(layer) inherits(layer$geom, "GeomTile"),
        logical(1)
    )
    expect_false(
        plotted_group$plot$plist$annotation$layers[[which(tile_layers)[1]]]$
            show.legend
    )
    .expect_patchwork_renders(spliceTerrain(ctx = by_group))
})

test_that("annotation styling columns must exist", {
    bams <- .placeholder_bams()
    annotation <- GenomicRanges::GRangesList(
        tx = GenomicRanges::GRanges(
            seqnames = "chr14",
            ranges = IRanges::IRanges(70233810L, 70234097L),
            exon_rank = 1L
        )
    )

    expect_error(
        spliceTerrain(
            bam = bams, region = .hnrnpc_region(), annotation = annotation,
            anno_fill_by = "missing_column"
        ),
        "`anno_fill_by` must name a metadata column in `annotation`.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams, region = .hnrnpc_region(), annotation = annotation,
            anno_label_by = "missing_column"
        ),
        "`anno_label_by` must name a metadata column in `annotation`.",
        fixed = TRUE
    )
})

test_that("unnamed annotation groups get default labels", {
    annotation <- GenomicRanges::GRangesList(
        GenomicRanges::GRanges("chr1:110-120"),
        GenomicRanges::GRanges("chr1:150-160")
    )
    names(annotation[[1]]) <- "exon_1"
    ctx <- list(
        input = list(
            annotation = annotation,
            region = GenomicRanges::GRanges("chr1:100-200"),
            anno_fill_by = NULL,
            anno_label_by = NULL
        ),
        plot = list()
    )
    resolved <- spliceTerrain:::.resolveAnnotation(ctx)

    expect_identical(
        unique(resolved$input$annotation$group),
        c("annotation_1", "annotation_2")
    )

    for (bad_names in list(c("tx", "tx"), c("tx", ""))) {
        names(ctx$input$annotation) <- bad_names
        expect_error(
            spliceTerrain:::.resolveAnnotation(ctx),
            "`annotation` group names must be non-empty and unique.",
            fixed = TRUE
        )
    }
})

test_that("annotation groups retain off-window ranges on one seqname", {
    annotation <- GenomicRanges::GRangesList(
        tx = GenomicRanges::GRanges(c("chr1:1-10", "chr1:150-160"))
    )
    ctx <- list(
        input = list(
            annotation = annotation,
            region = GenomicRanges::GRanges("chr1:100-200"),
            anno_fill_by = NULL,
            anno_label_by = NULL
        ),
        plot = list()
    )

    resolved <- spliceTerrain:::.resolveAnnotation(ctx)
    expect_identical(BiocGenerics::start(resolved$input$annotation), c(1L, 150L))

    ctx$input$annotation[[1]] <- GenomicRanges::GRanges(
        c("chr1:1-10", "chr1:150-160", "chr2:1-10")
    )
    expect_error(
        spliceTerrain:::.resolveAnnotation(ctx),
        paste(
            "Each `annotation` group must contain ranges only on the",
            "plotting seqname."
        ),
        fixed = TRUE
    )

    ctx$input$annotation[[1]] <- GenomicRanges::GRanges(
        c("chr1:1-10:+", "chr1:150-160:-")
    )
    expect_error(
        spliceTerrain:::.resolveAnnotation(ctx),
        "Ranges within each `annotation` group must share one strand.",
        fixed = TRUE
    )
})

test_that("single-exon annotation groups can be plotted", {
    bams <- .hnrnpc_bams()
    annotation <- .hnrnpc_annotation()[1]
    annotation[[1]] <- annotation[[1]][1]

    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = annotation,
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_length(ctx$input$annotation, 1)
    expect_identical(ctx$input$annotation$group, names(annotation))
    .expect_patchwork_renders(spliceTerrain(ctx = ctx))
})

test_that("annotation arrow ranges follow transcript strand", {
    introns <- data.frame(
        start = c(10L, 30L),
        end = c(20L, 40L),
        width = c(10L, 10L),
        midpoint = c(15, 35),
        strand = c("+", "-"),
        y = c(0, 0),
        draw_arrow = TRUE
    )

    arrow_ranges <- spliceTerrain:::.annotationArrowRange(introns)

    expect_equal(arrow_ranges$arrow_start, c(14.5, 35.5))
    expect_equal(arrow_ranges$arrow_end, c(15.5, 34.5))
})

test_that("annotation intron midpoint matches plotted segment center", {
    exons <- list(tx = data.frame(
        start = c(10L, 40L),
        end = c(20L, 50L),
        strand = "+",
        group = "tx",
        y = 1L
    ))

    introns <- spliceTerrain:::.getIntrons(exons, min_arrow = 1L)

    expect_equal(introns$midpoint, (introns$start + introns$end) / 2)
})

test_that("annotation input must be a GRangesList overlapping region", {
    bams <- .placeholder_bams()
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
    same_seq_off_region_annotation <- GenomicRanges::GRangesList(
        tx = GenomicRanges::GRanges(
            seqnames = "chr14",
            ranges = IRanges::IRanges(1L, 100L)
        )
    )

    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            annotation = bad_annotation
        ),
        "'annotation' must be a GRangesList.",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            annotation = off_region_annotation
        ),
        "`annotation` does not overlap `region`",
        fixed = TRUE
    )
    expect_error(
        spliceTerrain(
            bam = bams,
            region = .hnrnpc_region(),
            annotation = same_seq_off_region_annotation
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
    layout <- spliceTerrain:::.junctionArcLayout(
        with_psi$plot$juncs, with_psi$plot$cov, 1, NULL
    )
    labels_with_psi <- spliceTerrain:::.junctionArcLabels(
        layout,
        with_psi$plot$juncs,
        with_psi$plot$psi
    )
    labels_without_psi <- spliceTerrain:::.junctionArcLabels(
        layout,
        with_psi$plot$juncs,
        NULL
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
    expect_true(inherits(
        plotted$plot$plist$annotation$layers[[
            length(plotted$plot$plist$annotation$layers)
        ]]$geom,
        "GeomRect"
    ))
})

test_that("annotation and overlays render without intron compression", {
    bams <- .hnrnpc_bams()
    p <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = .hnrnpc_annotation(),
        psi = .hnrnpc_psi(),
        highlight = .hnrnpc_highlight(),
        min_coverage = 1,
        min_junction_reads = 1,
        compress_introns = FALSE
    )

    .expect_patchwork_renders(p)
})
