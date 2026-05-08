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

    expect_length(region, 1L)
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

    expect_length(region, 1L)
    expect_identical(BiocGenerics::start(region), 100L)
    expect_identical(BiocGenerics::end(region), 220L)
    expect_identical(as.character(BiocGenerics::strand(region)), "-")
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
