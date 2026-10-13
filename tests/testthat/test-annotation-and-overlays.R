test_that("EnsDb annotation is resolved into grouped plotting ranges", {
    bams <- .hnrnpc_bams()
    annotation <- .hnrnpc_annotation()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = annotation,
        annotated_junctions = TRUE,
        anno_label_by = "exon_rank",
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    expect_s4_class(ctx$annotation, "GRanges")
    expect_true(all(ctx$annotation$group %in% names(annotation)))
    expect_true("exon_rank" %in% names(S4Vectors::mcols(ctx$annotation)))
    expect_true(any(ctx$juncs$annotation_match))
    seqnames <- as.character(Seqinfo::seqnames(ctx$annotation))
    expect_true(all(seqnames == "chr14"))
})

test_that("annotation panel and annotation labels can be plotted", {
    bams <- .hnrnpc_bams()
    ctx <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        annotation = .hnrnpc_annotation(),
        annotated_junctions = TRUE,
        anno_label_by = "exon_rank",
        anno_label_colour = "black",
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )

    plotted <- .prepare_plot_context(ctx)
    plotted <- spliceTerrain:::.plotSamples(plotted)
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

    plotted_group <- .prepare_plot_context(by_group)
    plotted_group <- spliceTerrain:::.plotAnnotation(
        spliceTerrain:::.plotSamples(plotted_group)
    )
    plotted_exon <- .prepare_plot_context(by_exon)
    plotted_exon <- spliceTerrain:::.plotAnnotation(
        spliceTerrain:::.plotSamples(plotted_exon)
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

    expect_length(ctx$annotation, 1)
    expect_identical(ctx$annotation$group, names(annotation))
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

test_that("annotation intron midpoint matches plotted segment centre", {
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

test_that("junctions are matched exactly to annotation introns", {
    annotation <- GenomicRanges::GRanges(
        c(
            "chr1:100-120:+", "chr1:200-220:+",
            "chr1:100-120:-", "chr1:200-220:-"
        ),
        group = rep(c("tx1", "tx2"), each = 2)
    )
    juncs <- GenomicRanges::GRanges(c("chr1:121-199", "chr1:122-199"))

    matched <- spliceTerrain:::.matchAnnotatedJunctions(juncs, annotation)

    expect_identical(matched$annotation_match, c(TRUE, FALSE))
    expect_length(spliceTerrain:::.annotationJunctions(annotation), 1)
    expect_error(
        spliceTerrain:::.checkAnnotatedJunctions(list(input = list(
            annotated_junctions = TRUE, annotation = NULL
        ))),
        "`annotated_junctions = TRUE` requires `annotation`.",
        fixed = TRUE
    )
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

test_that("junction usage adds percentage labels to selected junctions", {
    bams <- .hnrnpc_bams()
    with_usage <- spliceTerrain(
        bam = bams[7],
        region = .hnrnpc_region(),
        junction_usage = .hnrnpc_junction_usage(),
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )
    with_usage <- .prepare_plot_context(with_usage)
    layout <- spliceTerrain:::.junctionArcLayout(
        with_usage$plot$juncs, with_usage$plot$cov, 1, NULL
    )
    labels_with_usage <- spliceTerrain:::.junctionArcLabels(
        layout,
        with_usage$plot$juncs,
        with_usage$plot$junction_usage
    )
    labels_without_usage <- spliceTerrain:::.junctionArcLabels(
        layout,
        with_usage$plot$juncs,
        NULL
    )

    expect_true(any(grepl("%", labels_with_usage$label, fixed = TRUE)))
    expect_false(any(grepl("%", labels_without_usage$label, fixed = TRUE)))
})

test_that("junction usage percentages retain hidden junction support", {
    .with_junction_usage_bam(function(bam) {
        low <- spliceTerrain(
            bam = bam, region = "synthetic:90-550",
            junction_usage = "synthetic:110-110",
            min_junction_reads = 1
        )
        high <- spliceTerrain(
            bam = bam, region = "synthetic:90-550",
            junction_usage = "synthetic:110-110",
            min_junction_reads = 10
        )
        clipped <- spliceTerrain(
            bam = bam, region = "synthetic:90-250",
            junction_usage = "synthetic:110-110",
            min_junction_reads = 1
        )
        percentage <- spliceTerrain(
            bam = bam, region = "synthetic:90-550",
            junction_usage = "synthetic:110-110", min_junction_reads = "50%",
            min_coverage = "100%", compress_introns = FALSE
        )

        expect_identical(.junction_labels(high)[1], .junction_labels(low)[1])
        expect_identical(.junction_labels(clipped)[1], .junction_labels(low)[1])
        expect_identical(.junction_labels(percentage)[1],
                         .junction_labels(low)[1])
        expect_identical(.junction_labels(high)[1], "90\n(90.9%)")
    })
})

test_that("out-of-window junctions remain in the usage reference", {
    .with_junction_usage_bam(function(bam) {
        ctx <- spliceTerrain(
            bam = bam, region = "synthetic:90-250",
            junction_usage = "synthetic:110-110", min_junction_reads = 10,
            return_ctx = TRUE
        )

        expect_identical(BiocGenerics::end(ctx$juncs), 199L)
        expect_identical(BiocGenerics::end(ctx$juncs_all), c(199L, 299L))
        expect_identical(ctx$juncs_all$coverage_raw, c(90L, 9L))
        expect_equal(ctx$juncs$usage_fraction, 90 / 99)
    })
})

test_that("usage is per sample and independent of normalisation", {
    .with_junction_usage_bam(function(first_bam) {
        .with_junction_usage_bam(function(second_bam) {
            ctx <- spliceTerrain(
                bam = c(first = first_bam, second = second_bam),
                region = "synthetic:90-550",
                junction_usage = "synthetic:110-110",
                min_junction_reads = c(10, 1), lib_size = c(1, 2),
                normalise_to = 1, return_ctx = TRUE
            )
            first <- ctx$juncs[ctx$juncs$sample == "first"]
            second <- ctx$juncs[ctx$juncs$sample == "second"]

            expect_equal(first$usage_fraction, c(90 / 99, NA_real_))
            expect_equal(second$usage_fraction, c(0.5, 0.5, NA_real_))
            expect_equal(second$coverage, c(25, 25, 10))
            p <- spliceTerrain(ctx = ctx)
            expect_identical(.junction_labels(p, 2),
                             c("25\n(50.0%)", "25\n(50.0%)", "10"))
        }, counts = c(50L, 50L, 20L))
    })
})

test_that("ranges combine junction support without double-counting anchors", {
    .with_junction_usage_bam(function(bam) {
        site <- spliceTerrain(
            bam = bam, region = "synthetic:90-350",
            junction_usage = "synthetic:110-110", min_junction_reads = 1,
            return_ctx = TRUE
        )
        broad <- spliceTerrain(
            bam = bam, region = "synthetic:90-350",
            junction_usage = c("synthetic:110-299", "synthetic:110-110"),
            min_junction_reads = 1, return_ctx = TRUE
        )

        expect_equal(site$juncs$usage_fraction, c(0.5, 0.5, NA_real_))
        expect_identical(broad$juncs$coverage_raw, rep(50L, 3))
        expect_equal(broad$juncs$usage_fraction, rep(1 / 3, 3))
        expect_identical(.junction_labels(spliceTerrain(ctx = broad)),
                         rep("50\n(33.3%)", 3))
    }, counts = c(50L, 50L), cigar = c("10M90N10M90N10M", "10M190N10M"),
    pos = c(100L, 100L), read_width = c(30L, 20L))
})

test_that("unselected and zero-support references omit usage percentages", {
    .with_junction_usage_bam(function(bam) {
        ctx <- spliceTerrain(
            bam = bam, region = "synthetic:90-550",
            junction_usage = "synthetic:150-150", min_junction_reads = 1,
            return_ctx = TRUE
        )
        expect_true(all(is.na(ctx$juncs$usage_fraction)))
        expect_identical(.junction_labels(spliceTerrain(ctx = ctx)),
                         c("90", "9", "20"))

        ctx$junction_usage <- GenomicRanges::GRanges("synthetic:110-110")
        ctx$juncs_all$coverage_raw[] <- 0
        expect_true(all(is.na(.prepare_plot_context(ctx)$input$juncs$
                                 usage_fraction)))
        expect_identical(.junction_labels(spliceTerrain(ctx = ctx)),
                         c("90", "9", "20"))
        ctx$juncs_all <- ctx$juncs_all[FALSE]
        expect_identical(.junction_labels(spliceTerrain(ctx = ctx)),
                         c("90", "9", "20"))
    })
})

test_that("usage references respect MAPQ and strand eligibility", {
    .with_junction_usage_bam(function(bam) {
        mapq <- spliceTerrain(
            bam = bam, region = "synthetic:90-550", min_mapq = 61,
            junction_usage = "synthetic:110-110", return_ctx = TRUE
        )
        strand <- spliceTerrain(
            bam = bam, region = "synthetic:90-550:+", strandedness = "reverse",
            junction_usage = "synthetic:110-110", return_ctx = TRUE
        )

        expect_length(mapq$juncs_all, 0)
        expect_length(strand$juncs_all, 0)
        expect_length(mapq$juncs$usage_fraction, 0)
        expect_length(strand$juncs$usage_fraction, 0)
    })
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
    plotted <- .prepare_plot_context(ctx)
    plotted <- spliceTerrain:::.plotSamples(plotted)
    plotted <- spliceTerrain:::.plotAnnotation(plotted)

    expect_s4_class(ctx$highlight, "GRanges")
    expect_s4_class(plotted$plot$highlight, "GRanges")
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
        junction_usage = .hnrnpc_junction_usage(),
        highlight = .hnrnpc_highlight(),
        min_coverage = 1,
        min_junction_reads = 1,
        compress_introns = FALSE
    )

    .expect_patchwork_renders(p)
})
