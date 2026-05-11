.example_ranges <- function() {
    GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(c(100L, 200L), width = 10L),
        strand = "+"
    )
}

test_that("buildMap creates expected compacted plot coordinates", {
    map <- spliceTerrain:::.buildMap(.example_ranges(), gap = 10)

    expect_s4_class(map, "GRanges")
    expect_identical(map$g_start, c(100L, 200L))
    expect_identical(map$g_end, c(109L, 209L))
    expect_equal(map$p_start, c(1, 21))
    expect_equal(map$p_end, c(10, 30))
})

test_that("genome-to-plot mapping preserves ranges and metadata", {
    gr <- .example_ranges()
    gr$id <- c("a", "b")
    map <- spliceTerrain:::.buildMap(gr, gap = 10)
    mapped <- spliceTerrain:::.mapGenomeToPlot(gr, map)[[1]]

    expect_s4_class(mapped, "GRanges")
    expect_identical(BiocGenerics::start(mapped), c(1L, 21L))
    expect_identical(BiocGenerics::end(mapped), c(10L, 30L))
    expect_identical(mapped$id, c("a", "b"))
})

test_that("plot-to-genome mapping handles blocks and compacted gaps", {
    map <- spliceTerrain:::.buildMap(.example_ranges(), gap = 10)

    expect_equal(spliceTerrain:::.mapPlotToGenome(c(1, 10, 21, 30), map),
        c(100, 109, 200, 209)
    )
    expect_equal(spliceTerrain:::.mapPlotToGenome(15.5, map), 154.5)
})

test_that("ranges can round-trip through anchor representation", {
    gr <- .example_ranges()
    gr$coverage <- c(5L, 10L)

    anchors <- spliceTerrain:::.rangesToAnchors(gr)
    round_trip <- spliceTerrain:::.anchorsToRanges(anchors, gr)

    expect_s4_class(anchors, "GRanges")
    expect_identical(anchors$anchor, rep(c("start", "end"), each = 2L))
    expect_identical(BiocGenerics::start(round_trip), BiocGenerics::start(gr))
    expect_identical(BiocGenerics::end(round_trip), BiocGenerics::end(gr))
    expect_identical(round_trip$coverage, gr$coverage)
})
