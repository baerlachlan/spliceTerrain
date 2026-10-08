.resolve_region <- function(region) {
    ctx <- list(input = list(region = region), plot = list())
    spliceTerrain:::.resolveRegion(ctx)$input$region
}

test_that("character regions are normalised before GRanges coercion", {
    region <- .resolve_region("chr14:70,222,436 - 70,237,375")

    expect_s4_class(region, "GRanges")
    expect_identical(as.character(Seqinfo::seqnames(region)), "chr14")
    expect_identical(BiocGenerics::start(region), 70222436L)
    expect_identical(BiocGenerics::end(region), 70237375L)

    en_dash_region <- .resolve_region("chr14:70222436\u201370237375")
    em_dash_region <- .resolve_region("chr14:70222436\u201470237375")
    expect_identical(BiocGenerics::start(en_dash_region), 70222436L)
    expect_identical(BiocGenerics::end(en_dash_region), 70237375L)
    expect_identical(BiocGenerics::start(em_dash_region), 70222436L)
    expect_identical(BiocGenerics::end(em_dash_region), 70237375L)
})

test_that("multiple region ranges resolve to one span", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr14",
        ranges = IRanges::IRanges(c(100L, 300L), c(150L, 350L)),
        strand = "+"
    )
    region <- .resolve_region(gr)

    expect_length(region, 1)
    expect_identical(as.character(Seqinfo::seqnames(region)), "chr14")
    expect_identical(BiocGenerics::start(region), 100L)
    expect_identical(BiocGenerics::end(region), 350L)
    expect_identical(as.character(BiocGenerics::strand(region)), "+")
})

test_that("GRangesList regions are unlisted before span calculation", {
    grl <- GenomicRanges::GRangesList(
        tx1 = GenomicRanges::GRanges(
            seqnames = "chr14",
            ranges = IRanges::IRanges(c(100L, 200L), c(120L, 220L)),
            strand = "-"
        )
    )
    region <- .resolve_region(grl)

    expect_length(region, 1)
    expect_identical(BiocGenerics::start(region), 100L)
    expect_identical(BiocGenerics::end(region), 220L)
    expect_identical(as.character(BiocGenerics::strand(region)), "-")
})

test_that("region spans preserve complete Seqinfo metadata", {
    info <- Seqinfo::Seqinfo(
        seqnames = c("synthetic", "unused"), seqlengths = c(1000L, 2000L),
        isCircular = c(FALSE, TRUE), genome = "assembly_one"
    )
    gr <- GenomicRanges::GRanges(
        seqnames = "synthetic",
        ranges = IRanges::IRanges(c(100L, 300L), c(119L, 319L)),
        strand = "+", seqinfo = info
    )
    inputs <- list(gr[1], gr, GenomicRanges::GRangesList(tx = gr))
    expected_ends <- c(119L, 319L, 319L)
    for (i in seq_along(inputs)) {
        region <- .resolve_region(inputs[[i]])

        expect_identical(Seqinfo::seqinfo(region), info)
        expect_identical(BiocGenerics::start(region), 100L)
        expect_identical(BiocGenerics::end(region), expected_ends[i])
    }
})

test_that("known and unspecified region metadata work with annotation", {
    .with_paired_bam(function(bam) {
        info <- Seqinfo::Seqinfo("synthetic", 1000L, genome = "assembly_one")
        known <- GenomicRanges::GRanges("synthetic:650-850", seqinfo = info)
        unknown <- GenomicRanges::GRanges("synthetic:650-850")
        annotation <- GenomicRanges::GRangesList(tx = GenomicRanges::GRanges(
            c("synthetic:700-709", "synthetic:800-809"), seqinfo = info
        ))
        unknown_annotation <- annotation
        Seqinfo::seqinfo(unknown_annotation) <- Seqinfo::Seqinfo("synthetic")
        inputs <- list(
            list(region = known, annotation = annotation),
            list(region = unknown, annotation = annotation),
            list(region = known, annotation = unknown_annotation)
        )
        for (input in inputs) {
            ctx <- do.call(spliceTerrain, c(
                list(bam = bam, min_junction_reads = 1, return_ctx = TRUE),
                input
            ))

            expect_identical(Seqinfo::seqinfo(ctx$region),
                             Seqinfo::seqinfo(input$region))
            expect_identical(Seqinfo::seqinfo(ctx$annotation),
                             Seqinfo::seqinfo(input$annotation))
            expect_identical(ctx$juncs$coverage_raw, 1L)
            .expect_patchwork_renders(spliceTerrain(ctx = ctx))
        }
    }, keep = "overlap")
})

test_that("region metadata conflicts are rejected before BAM summarisation", {
    .with_paired_bam(function(bam) {
        info <- Seqinfo::Seqinfo("synthetic", 1000L, genome = "assembly_one")
        region <- GenomicRanges::GRanges("synthetic:650-850", seqinfo = info)
        annotation <- GenomicRanges::GRangesList(tx = GenomicRanges::GRanges(
            c("synthetic:700-709", "synthetic:800-809"), seqinfo = info
        ))
        wrong_genome <- annotation
        Seqinfo::genome(wrong_genome) <- "assembly_two"
        wrong_length <- annotation
        Seqinfo::seqlengths(wrong_length) <- 1100L

        expect_error(
            spliceTerrain(bam = bam, region = region,
                          annotation = wrong_genome, return_ctx = TRUE),
            "incompatible genomes", fixed = TRUE
        )
        expect_error(
            spliceTerrain(bam = bam, region = region,
                          annotation = wrong_length, return_ctx = TRUE),
            "incompatible seqlengths", fixed = TRUE
        )
    }, keep = "overlap")
})

test_that("non-genomic region inputs fail clearly", {
    expect_error(
        .resolve_region(1:3),
        "`region` must be a GRanges or GRangesList.",
        fixed = TRUE
    )
})

test_that("region inputs must resolve to exactly one seqname", {
    multi_seq_region <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr2"),
        ranges = IRanges::IRanges(c(100L, 300L), c(150L, 350L))
    )

    expect_error(
        .resolve_region(multi_seq_region),
        "`region` must resolve to ranges on exactly one seqname.",
        fixed = TRUE
    )
})

test_that("multiple region ranges must resolve to exactly one strand", {
    mixed_strand_region <- GenomicRanges::GRanges(
        seqnames = "chr14",
        ranges = IRanges::IRanges(c(100L, 300L), c(150L, 350L)),
        strand = c("+", "-")
    )

    expect_error(
        .resolve_region(mixed_strand_region),
        "`region` ranges must all have the same strand.",
        fixed = TRUE
    )
})
